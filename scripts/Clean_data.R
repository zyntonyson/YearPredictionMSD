library("tidyverse")
library("magrittr")
library("ROSE")
# Cargar datos
data <- read_csv("data/YearPredictionMSD.txt", 
                 col_names = FALSE)



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

set.seed(1)
muestra <- sample(1:nrow(data), n/2)

X_tr = as.matrix(data[muestra,-1])
Y_tr = as.matrix(data[muestra,1])

X_te = as.matrix(data[-muestra,-1])
Y_te = as.matrix(data[-muestra,1])




# Agrupando las canciones por decadas, las canciones de las decadas 20 y 40 
# se ingresan a una misma categoría
data %<>% 
  mutate(decades=sapply(year,function(x){x-x%%10})) %>% 
  mutate(decades=sapply(decades,function(x){ifelse(x<1950,1940,x)}))

<<<<<<< HEAD

# No pude correrlo
v = table(data$decades)[-1]
muestras <- lapply(1:7, function (x) sample(1:v[x], v[x]*.6))
=======
#v = table(data$decades)[-1]
#muestras <- lapply(1:7, function (x) sample(1:v[x], v[x]*.6))


#data <- as.data.frame(data)
data$decades <- as.factor(data$decades)
data_tr <- data[muestra,]
data_tr <- data_tr[data_tr$decades!=1940,]
data_te <- data[muestra,]
data_te <- data_te[data_te$decades!=1940,]

>>>>>>> ec6e3b42ddc5e5abcf250221f24cbfb8d75420d4


#(data[muestra ])
#db_muestra <-  undersample_ds(data[muestra,], "decades", 10)

#dt <- ROSE(decades ~ ., data, method = "under",N = 100, seed = 3)
set.seed(3)
db_tr <- data_tr %>% group_by(decades) %>% sample_n(1500)
db_te <- data_te %>% group_by(decades) %>% sample_n(1000)


# Data balanceado para clasificación
# 
dataclasificador<-data %>% 
  mutate(decades=sapply(year,function(x){x-x%%10})) %>% 
  mutate(decades=sapply(decades,function(x){ifelse(x<1950,1940,x)}))





