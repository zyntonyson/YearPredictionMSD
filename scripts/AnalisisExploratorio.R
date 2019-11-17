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

### ANALISIS EXPLORATORIO ###

# Vemos la cantidad de canciones por año
# Se observa un gran desbalance
data$year %T>% 
  hist() %>% 
  table()


# Agrupando las canciones por decadas
data %<>% 
  mutate(decades=sapply(year,function(x){x-x%%10})) 
  

# Vemos la cantidad de canciones por año
data$decades %T>% 
  hist() %>% 
  table()



#########
# Continua desbalance, se trata de observar distinción 
# las  decadas


## ANALISIS POR TimbreAvg
data %>% 
  select(decades,starts_with('TimbreAvg')) %>% 
  gather('TimbreAvg','Val',starts_with('TimbreAvg'))%>% 
  #mutate(decades=as.factor(decades)) %>%
  group_by(decades,TimbreAvg) %>% 
  summarise(mean=mean(Val)) %>% 
  ggplot() + 
  aes(x=as.factor(decades),y=TimbreAvg,fill=mean) +
  geom_tile()+
  scale_fill_gradient(low="green", high="red")

# Observamos la distribución de las variables de interes

data %>% 
  select(decades,
         TimbreAvg2, ## Mejor resultado
         TimbreAvg7,
         TimbreAvg6,
         TimbreAvg3) %>%
  ggplot() + 
  aes(x=TimbreAvg3,color= as.factor(decades)) +
  #aes(x=TimbreAvg7,color= as.factor(decades)) +
  #aes(x=TimbreAvg6,color= as.factor(decades)) +
  #aes(x=TimbreAvg2,color= as.factor(decades)) +
  geom_density()
  

data %>% 
  mutate(decades=sapply(decades,
                        function(x){ifelse(x<1950,1940,x)})) %>% 
  select(decades,
         TimbreAvg2,
         TimbreAvg7,
         TimbreAvg6,
         TimbreAvg9) %>%
  ggplot() + 
  aes(x=TimbreAvg2,color= as.factor(decades)) +
  #aes(x=TimbreAvg7,color= as.factor(decades)) +
  #aes(x=TimbreAvg6,color= as.factor(decades)) +
  #aes(x=TimbreAvg9,color= as.factor(decades)) +
  geom_density()


## ANALISIS POR TimbreCov
## 
data %>% 
  select(decades,starts_with('TimbreCov')) %>% 
  gather('TimbreCov','Val',starts_with('TimbreCov'))%>% 
  #mutate(decades=as.factor(decades)) %>%
  group_by(decades,TimbreCov) %>% 
  summarise(mean=mean(Val)) %>% 
  ggplot() + 
  aes(x=as.factor(decades),y=TimbreCov,fill=mean) +
  geom_tile() + 
  scale_fill_gradient(low="green", high="red")
  