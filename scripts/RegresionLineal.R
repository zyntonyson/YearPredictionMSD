# Análisis de Regresión Simple

# librerias
library("leaps")
library("MASS")
library("tidyverse")
library("magrittr")
library("glmnet")
library("pls")
library("ggplot2")


#lectura de archivos
data <- read_csv("data/YearPredictionMSD.txt", 
                 col_names = FALSE)

names(data)<-c('year',
               sapply(1:12,function(x){paste('TimbreAvg',x,sep='')}),
               sapply(1:78,function(x){paste('TimbreCov',x,sep='')}))


summary(data)

#dividimos nuestros datos en prueba y validación

#importante la semilla
n = nrow(data)

set.seed(1)
muestra <- sample(1:nrow(data), n*.6)

X_tr = as.matrix(data[muestra,-1])
Y_tr = as.matrix(data[muestra,1])

X_te = as.matrix(data[-muestra,-1])
Y_te = as.matrix(data[-muestra,1])

nVar<-90


#Regresion Lineal 

model_lm <- lm(year~., data = data[muestra,])
summary(model_lm)

X1_test <- cbind(1,X_te)
Y_gorro <- X1_test%*%coef(model_lm)
mse_test_ols <- mean((Y_te-Y_gorro)^2)

# seleccion mejor subconjunto de variables

modelo_fw <- regsubsets(X_tr, Y_tr, method = "forward",  nvmax = nVar)
modelo_bw <- regsubsets(X_tr, Y_tr, method = "backward", nvmax = nVar)

#modelo con paso fwd

msum_fwd  <- summary(modelo_fw )

#modelo con paso backward

msum_bwd  <- summary(modelo_bw )
#grafica
par(mfrow=c(3,2))
plot(msum_fwd$rsq ,xlab="Núm Variables", ylab="R cuadrado",type="b", pch=19, col="#95505c")
plot(msum_bwd$rsq ,xlab="Núm Variables", ylab="R cuadrado",type="b", pch=19, col="#95505c")
plot(msum_bwd$bic ,xlab="Núm Variables", ylab="Bic",type="b", pch=19, col="#5c9550")
plot(msum_fwd$bic ,xlab="Núm Variables", ylab="Bic",type="b", pch=19, col="#5c9550")
plot(msum_fwd$cp  ,xlab="Núm Variables", ylab="Cp", type="b", pch=19, col="#505c95")
plot(msum_bwd$cp  ,xlab="Núm Variables", ylab="Cp", type="b", pch=19, col="#505c95")

par(mfrow=c(1,1))


# #grafica
# par(mfrow=c(3,1))
# plot(msum_bwd$rsq ,xlab="Núm Variables",ylab="R cuadrado",type="b", pch=19, col="#95505c")
# plot(msum_bwd$bic ,xlab="Núm Variables", ylab="Bic",type="b", pch=19, col="#5c9550")
# plot(msum_bwd$cp ,xlab="Núm Variables",ylab="Cp", type="b", pch=19, col="#505c95")

#par(mfrow=c(1,1))

#aun seleccionando todas las variables el R_cuadrado es muy bajo, del .15, por lo que quizá la regresion no es la mejor opción
# veremos las 30 variables más importantes

coef_bw <- coef(modelo_bw, 30)
coef_fw <- coef(modelo_fw, 30)

# estas son las variables en las que difieren los modelos
names(coef_bw) [is.na ( match(names(coef_bw),names( coef_fw)) )]
names(coef_fw) [is.na ( match(names(coef_fw),names( coef_bw)) )]

mse_test_bw <- vector()

for (i in 1:nVar){
  coefs <- coef(modelo_bw,i)
  indice <- match(names(coefs)[-1],colnames(X_te))
  X1_test <- cbind(1,X_te[,indice])
  Y_gorro <- X1_test%*%coefs
  mse_test_bw <- c(mse_test_bw, mean((Y_te-Y_gorro)^2))    
}


mse_test_fw <- vector()
for (i in 1:nVar){
  coefs <- coef(modelo_fw,i)
  indice <- match(names(coefs)[-1],colnames(X_te))
  X1_test <- cbind(1,X_te[,indice])
  Y_gorro <- X1_test%*%coefs
  mse_test_fw <- c(mse_test_fw, mean((Y_te-Y_gorro)^2))    
}


plot(mse_test_bw,type="b", pch=19, ylab="Error cuadrático Medio - PRUEBA",xlab="Número de variables", main ="Modelo Paso Backward" )

plot(mse_test_fw,type="b", pch=19, ylab="Error cuadrático Medio - PRUEBA",xlab="Número de variables", main ="Modelo Paso Forward" )


#### REGRESION LASSO ####


mat_tr <- model.matrix(year ~ ., data = data[muestra,])
mat_te <- model.matrix(year ~ ., data = data[-muestra,])


# Conjunto de valores de lambda 
lambda = 10 ^ seq(from = -4, to = 4, length = 100)
# 10-fold cross validation para obtener el mejor lambda
set.seed(8)
#set.seed(1)
#cv_error_lasso <- cv.glmnet(x = x, y = y, alpha = 1, nfolds = 10)
cv_lasso <- cv.glmnet(x = mat_tr, y = Y_tr, alpha = 1, lambda = lambda, thresh = 1e-12, type.measure="mse",nfolds = 10)
plot(cv_lasso)
l_lasso <- cv_lasso$lambda.min
l_lasso <- cv_lasso$lambda.1se

#modelo lasso
model_lasso <- glmnet (mat_tr,Y_tr, alpha =1, lambda =l_lasso, thresh =1e-8) 

#evaluamos el modelo
ypred_lasso <- predict(model_lasso ,s=l_lasso ,newx=mat_te) 
#error cuadrático medio de prueba
msete_lasso <- mean((ypred_lasso - Y_te)^2)
msete_lasso 

coef(model_lasso)


##### PCR #####

model_pcr <- pcr(year ~ ., data=data[muestra,], scale=TRUE,validation ="CV")
summary(model_pcr)

validationplot(model_pcr,val.type="MSEP")

#Evaluamos el modelo
ypred_pcr <- predict(model_pcr, X_te, ncomp=30) [,1,]
msete_pcr <- mean((ypred_pcr- Y_te)^2) 


valores_testMSE <- data.frame(metodo = c("OLS", "Forward Select","Backward Select", "PCR",
                                         "Lasso"),
                              MSE_te = c(mse_test_ols, mse_test_fw[30], mse_test_bw[30],
                                         msete_pcr, msete_lasso))

ggplot(data = valores_testMSE, aes(x = metodo, y = MSE_te)) +
  geom_col(width = 0.5, fill="#2b602c") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+ xlab("Método") + ylab("MSE Test")              
