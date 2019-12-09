# OTROS CLASIFICADORES

library("e1071")
library("tidyverse")
library("magrittr")
library("ROSE")
library("caret")
library("neuralnet")

# Cargar datos
data <- read_csv("data/YearPredictionMSD.txt", col_names = FALSE)



# Agregar nombres a columnas para identificacion
# 1-year
# 2-13 TimbreAvg1-12
# 14-91 TimbreCov1-78
# 

names(data)<-c('year',
               sapply(1:12,function(x){paste('TimbreAvg',x,sep='')}),
               sapply(1:78,function(x){paste('TimbreCov',x,sep='')}))



#dividimos nuestros datos en prueba y validación
#importante la semilla
n = nrow(data)
p<-0.6
set.seed(1)
muestra <- sample(1:nrow(data), n*p)

X_tr = as.matrix(data[muestra,-1])
Y_tr = as.matrix(data[muestra,1])

X_te = as.matrix(data[-muestra,-1])
Y_te = as.matrix(data[-muestra,1])


# Agrupando las canciones por decadas, las canciones de las decadas 20 y 40 
# se ingresan a una misma categoría
data %<>% 
  mutate(decades=sapply(year,function(x){x-x%%10})) %>% 
  mutate(decades=sapply(decades,function(x){ifelse(x<1950,1940,x)}))

# para este caso vamos a dejar fuera las canciones antes de 1940
data$decades <- as.factor(data$decades)
data_tr <- data[muestra,]
data_tr <- data_tr[data_tr$decades!=1940,]
data_te <- data[muestra,]
data_te <- data_te[data_te$decades!=1940,]


#realizamos un muestreo balanceado tomando 1500 para entrenamiento y 1000 para prueba
set.seed(3)
db_tr <- data_tr %>% group_by(decades) %>% sample_n(1500)
db_te <- data_te %>% group_by(decades) %>% sample_n(1000)

db_tr <- db_tr[,-1]
db_te <- db_te[,-1]

#### SUPORT VECTOR MACHINE ####

modelo_svm <- tune("svm", decades ~ ., data = db_tr, ranges = list(cost = c(0.1, 1)), kernel = "linear", scale=TRUE)
# seleccionamos mejor modelo
svm_bmodel <- modelo_svm$best.model

#evaluamos en datos de entrenamiento
y_tr_pred <-  svm_bmodel$fitted
table(predict = y_tr_pred , real = db_tr$decades)
sum(y_tr_pred == db_tr$decades)/nrow(db_tr)

#evaluamos en datos de prueba
predicc <- predict(object = modelo_svm$best.model, newdata = db_te)
cf_te <- confusionMatrix(predicc, db_te$decades, mode = "everything")
cf_te
sum(predicc==db_te$decades)/nrow(db_te)

#### RED NEURONAL SIMPLE ####

nntr <- neuralnet(decades~.,data=db_tr, hidden=15, threshold = .01,linear.output = FALSE)

#Predict datos de entrenamiento
nntrain <- compute(nntr,db_tr[,-ncol(db_tr)])
Ynntrain <- (apply(nntrain$net.result,1, which.max) + 4)*10 + 1900
table(Ynntrain, db_tr$decades)
sum(Ynntrain==db_tr$decades)/nrow(db_tr)


#Predict datos de prueba
nntest <-  compute(nntr,db_te[,-ncol(db_te)])
Ynntest <- (apply(nntest$net.result,1, which.max) + 4 )*10 + 1900
table(Ynntest, db_te$decades)
sum(Ynntest==db_te$decades)/nrow(db_te)
