# Referencias
# https://stackoverflow.com/questions/51843592/glmnet-multinomial-logistic-regression-prediction-result
# https://bookdown.org/egarpor/PM-UC3M/app-ext-multinomialreg.html
# https://bookdown.org/egarpor/PM-UC3M/glm-model.html#glm-model-log
# http://www.sthda.com/english/articles/36-classification-methods-essentials/149-penalized-logistic-regression-essentials-in-r-ridge-lasso-and-elastic-net/
# https://stats.stackexchange.com/questions/305333/how-to-fit-multinomial-logistic-regression-using-lasso-into-k-1-in-r
# https://rpubs.com/heruwiryanto/multinom-reg
# https://www.institutomora.edu.mx/testU/SitePages/martinpaladino/modelos_logit_con_R.html#modelos-ligit-multinomiales.
# https://stats.idre.ucla.edu/r/dae/multinomial-logistic-regression/
# 



library(nnet) # Regresion multinomial
library(foreign)
library(tidyverse)
library(glmnet)  # Regresion multinomial más Lasso


####### REGRESION MULTINOMIAL ###################################

#  Data
set.seed(1)
muestra <- sample(1:nrow(data), nrow(data)*.6)


# formula
x<-paste(names(data[,names(data)!='decades']),collapse = 
        '+')
formula<-as.formula(paste('decades~',x,collapse=''))

# Ajuste
mod.MN<-multinom(formula,data=data[muestra,])













