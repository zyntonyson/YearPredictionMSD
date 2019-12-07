# Análisis de Regresión Simple

# librerias
library("leaps")
library("MASS")
library("tidyverse")
library("magrittr")
library("glmnet")

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


# seleccion mejor subconjunto de variables

modelo_fw <- regsubsets(X_tr, Y_tr, method = "forward",  nvmax = 50)
modelo_bw <- regsubsets(X_tr, Y_tr, method = "backward", nvmax = 50)

#modelo con paso fwd

msum_fwd  <- summary(modelo_fw )
#grafica
par(mfrow=c(3,2))
plot(msum_fwd$rsq ,xlab="Núm Variables",ylab="R cuadrado",type="b", pch=19, col="#95505c")
plot(msum_fwd$bic ,xlab="Núm Variables", ylab="Bic",type="b", pch=19, col="#5c9550")
plot(msum_fwd$cp ,xlab="Núm Variables",ylab="Cp", type="b", pch=19, col="#505c95")

par(mfrow=c(1,1))

#modelo con paso backward

msum_bwd  <- summary(modelo_bw )
#grafica
par(mfrow=c(3,1))
plot(msum_bwd$rsq ,xlab="Núm Variables",ylab="R cuadrado",type="b", pch=19, col="#95505c")
plot(msum_bwd$bic ,xlab="Núm Variables", ylab="Bic",type="b", pch=19, col="#5c9550")
plot(msum_bwd$cp ,xlab="Núm Variables",ylab="Cp", type="b", pch=19, col="#505c95")

par(mfrow=c(1,1))

#aun seleccionando todas las variables el R_cuadrado es muy bajo, del .15, por lo que quizá la regresion no es la mejor opción
# veremos las 15 variables más importantes

coef_bw <- coef(modelo_bw, 15)
coef_fw <- coef(modelo_fw, 15)

# estas son las variables en las que difieren los modelos
names(coef_bw) [is.na ( match(names(coef_bw),names( coef_fw)) )]
names(coef_fw) [is.na ( match(names(coef_fw),names( coef_bw)) )]

mse_test_new <- vector()
for (i in 1:20){
  coefs <- coef(m_full,i)
  indice <- match(names(coefs)[-1],let)
  X1_test <- cbind(1,X_test[,indice])
  Y_gorro <- X1_test%*%coefs
  mse_test_new <- c(mse_test_new, mean((Y_test-Y_gorro)^2))    
}



# REGRESION RIDGE


data_mat_tr <- model.matrix(year ~ ., data = data)

# Conjunto de valores de lambda 
lambda = 10 ^ seq(from = -4, to = 4, length = 100)
# 10-fold cross validation para obtener el mejor lambda
set.seed(12)
cv.ridge <- cv.glmnet(x = data_mat, y = Y, alpha = 0, lambda = lambda, thresh = 1e-12, type.measure="mse")

plot(cv.ridge)  

cv.ridge$lambda.min
glmnet(X,Y)
# Se excluye la primera columna con los nombres de las universidades
modelo.ridge <- glmnet(X, Y, alpha = 0)
# Coeficientes del modelo
predict(modelo.ridge, type = "coefficients", s = cv.ridge$lambda.min)

filter()