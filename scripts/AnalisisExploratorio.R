library(tidyverse)

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

### ANALISIS EXPLORATORIO ###









