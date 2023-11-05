###############################################################################
#######                                                                 #######
#                          UNIVERSIDA DEL QUINDÍO                             #
#                   ASESORIA QUIEBRE ESTRUCTURAL ECO I                        #
#######                                                                 #######
###############################################################################
#31 de octubre de 2023


#By: Nicolás García Peñaloza
#+57 3122852823
#nicolasgp0109@gmail.com



options('scipen'=100, 'digits'=4)
rm(list=ls())
list=ls()


# Si los paquetes no están descargados primero descargarlos con install.packages("Nombre del paquete")
library(tidyverse)
library(strucchange)
library(timeSeries)

EXP = haven::read_dta(file = "C:/Users/nicol.NICOLAS_GP/Downloads/EXPO_2005_2021.dta" , encoding = "UTF-8" ) |> data.frame()

EXP |> str(list = 9 )


## GENERAL ------
skimr::skim(data = EXP )

EXP_ARMENIA = EXP |> dplyr::filter( ORIGEN == "63")

EXP_ARMENIA = EXP_ARMENIA |> dplyr::group_by(AÑO_N , TRIMESTRES) |> dplyr::summarise(TOTAL = sum(I_DOLARES) )

SERIE = log(ts(EXP_ARMENIA$TOTAL , start = c(2005, 1) , end = c(2021 , 4) , frequency = 4))

plot(SERIE)


D_SERIE = diff(SERIE,1)

plot(D_SERIE)


CE = breakpoints(D_SERIE ~ D_SERIE , h = 0.1)

summary(CE)

plot(CE)

plot(D_SERIE)

lines(CE , breaks = 3 , col = "blue" )
