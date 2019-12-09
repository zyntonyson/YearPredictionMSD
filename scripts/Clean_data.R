library(tidyverse)
library(magrittr)

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
muestra <- sample(1:nrow(data), n*.6)

X_tr = as.matrix(data[muestra,-1])
Y_tr = as.matrix(data[muestra,1])

X_te = as.matrix(data[-muestra,-1])
Y_te = as.matrix(data[-muestra,1])




# Agrupando las canciones por decadas, las canciones de las decadas 20 y 40 
# se ingresan a una misma categoría
data %<>% 
  mutate(decades=sapply(year,function(x){x-x%%10})) %>% 
  mutate(decades=sapply(decades,function(x){ifelse(x<1950,1940,x)}))


# No pude correrlo
v = table(data$decades)[-1]
muestras <- lapply(1:7, function (x) sample(1:v[x], v[x]*.6))


data <- as.data.frame(data)

data[data$decades==2010,]%>%data[muestras[[1]],]


# Data balanceado para clasificación
# 
dataclasificador<-data %>% 
  mutate(decades=sapply(year,function(x){x-x%%10})) %>% 
  mutate(decades=sapply(decades,function(x){ifelse(x<1950,1940,x)}))





