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


install.packages()

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



EMICRON = EMICRON |> mutate( N_EMPLEADOS = P3091 - 1 ) |> mutate(
 DUMMY = case_when(
    N_EMPLEADOS == 0 ~ 0 , 
    N_EMPLEADOS %in% seq(1,16,1) ~ 1 ))

c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16)
                           
EMICRON$DUMMY |> table()     
                         

EMICRON = EMICRON |> mutate(DUMMY_2 = DUMMY * P35)

EMICRON$DUMMY_2 |> table( useNA =  )


Copy_of_RP2017_1_ = read_excel("C:/Users/nicol.NICOLAS_GP/OneDrive/Escritorio/Portafolio/My Work/Armenia/My First Jobs_2022_1/Observatorio Fiscal/Copy of RP2017(1).xlsx", 
                               sheet = "RP2017")


copy_of_RP2017_1_ = Copy_of_RP2017_1_ |> 
  mutate(newCol = ifelse(str_detect( Observación , "CONTRATO"), 1 , 0))

table(Copy_of_RP2017_1_$newCol)


Copy_of_RP2017_1_ = Copy_of_RP2017_1_ |> 
  mutate(newCol_PROF = ifelse(str_detect( Observación , "CONTRATO DE PRESTACION DE SERVICIOS PROFESIONALES"), 0 , 1))

table(Copy_of_RP2017_1_$newCol_PROF) 


Copy_of_RP2017_1_ = Copy_of_RP2017_1_ |> 
  mutate(newCol_DEF = newCol * newCol_PROF  )


table(Copy_of_RP2017_1_$newCol_DEF) 


