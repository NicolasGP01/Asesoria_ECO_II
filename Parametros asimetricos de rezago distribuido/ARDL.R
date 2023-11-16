###############################################################################
#######                                                                 #######
#                       UNIVERSIDAD DEL QUINDÍO                               #
#               PROGRAMA DE ECONOMÍA | ECONOMETRÍA II                         #
#######                                                                 #######
###############################################################################

#By: Nicolás García Peñaloza
#+57 3122852823
#nicolasgp0109@gmail.com


print("Antes de iniciar un área de trabajo siempre, siempre tomen presente lo siguiente")


# DÓNDE ESTÁ PARADO R
getwd()

# NO USAR e+
options('scipen'=100, 'digits'=4) # Forzar a R a no usar e+


### LIMPIAR LA CONSOLA
cat('/f')
rm(list=ls())
list=ls()


## LIBRERIAS NECESARIAS
library(tidyverse); library(dynlm); library(dLagM); library(AER); library(xts)
library(ecm); library(openxlsx); library(urca) ; library(quantmod) ; library(nardl)



data = read.csv("https://raw.githubusercontent.com/NicolasGP01/Asesoria_ECO_II/main/Parametros%20asimetricos%20de%20rezago%20distribuido/Data/DATOSMACROEUROPA70-18.csv")


SIN_EMPLEO = ts(data = data$UNN , start = c(1970 , 1) , end = c(2017 , 4)  , frequency = 4)

plot(SIN_EMPLEO) 

PIB = ts(data = data$YER , start = c(1970 , 1) , end = c(2017 , 4)  , frequency = 4)

plot(PIB) 


data_2 = data |> dplyr::select(UNN , YER ) 

data_2 = log(data_2)

BASE = data.frame( SIN_EMPLEO , PIB)

BASE = log(BASE)



# Modelo de rezago distribuido
# dos rezagos
rezagod = dlm(formula = SIN_EMPLEO ~ PIB, 
               data = BASE , q = 2)
summary(rezagod)



# Modelo de auto regresivo
# un rezagos
autorezago  = lm(SIN_EMPLEO ~ stats::lag(SIN_EMPLEO , n = 1) ,
             data= BASE ) 

summary(autorezago )


finiteDLMauto(formula = SIN_EMPLEO ~ PIB , data = BASE ,
              q.min = 1, q.max = 3, model.type = "dlm",
              error.type = "AIC", trace = T)


MODASI = nardl( SIN_EMPLEO ~ PIB , data= BASE , ic = c("aic"),  maxlag = 3 , graph = TRUE , case = 3)

summary(MOD.ASI)

