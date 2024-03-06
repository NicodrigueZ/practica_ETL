## CARGAMOS LIBRERIAS

install.packages("tidyverse")
install.packages("tidyquant")
install.packages("webshot")
install.packages("orca")
library(tidyquant)
library(readr)
library(plotly)
library(webshot)
library(orca)
library(corrplot)
library(dplyr)


###############################################################################################################################
#########################################         ETL DATASET COVID        ####################################################
###############################################################################################################################

##HACEMOS EL CARGADO DEL SET DE DATOS 

rm(list = ls())

getwd()
setwd("C:/Users/Facundo/Desktop/UNO/DS/ETL/Covid/DataSet")

datos <- COVID_19_Global_Statistics_Dataset <- read_csv("COVID-19 Global Statistics Dataset.csv")
View(datos)
str(datos)
attach(datos)

summary(datos)

############################################################################

# NORMALIZACION Y LIMPIEZA

datos_normalizados <- c("Pais","Casos Totales","Casos Nuevos","Muertes Totales"
                        ,"Muertes Nuevas","Recuperados Totales","Nuevos Recuperados"
                        ,"Casos Activos","Casos Serios/Criticos"," Casos Infantiles c/1Millon"
                        ,"Muertes c/1Millon","Test Totales","Test c/1Millon","Poblacion")

names(datos) <- datos_normalizados

#datos <- datos[,-1]
datos$Pais <- as.character(gsub(",","",datos$Pais))
datos$`Casos Totales` <- as.numeric(gsub(",","",datos$`Casos Totales`))
datos$`Casos Nuevos` <- as.numeric(gsub(",","",datos$`Casos Nuevos` ))
datos$`Muertes Totales` <- as.numeric(gsub(",","",datos$`Muertes Totales`))
datos$`Muertes Nuevas` <- as.numeric(gsub(",","",datos$`Muertes Nuevas`))
datos$`Recuperados Totales` <- as.numeric(gsub(",","",datos$`Recuperados Totales` ))
datos$`Nuevos Recuperados` <- as.numeric(gsub(",","",datos$`Nuevos Recuperados`))
datos$`Casos Activos` <- as.numeric(gsub(",","",datos$`Casos Activos`))
datos$`Casos Serios/Criticos` <- as.numeric(gsub(",","",datos$`Casos Serios/Criticos`))
datos$` Casos Infantiles c/1Millon` <- as.numeric(gsub(",","",datos$` Casos Infantiles c/1Millon`))
datos$`Muertes c/1Millon` <- as.numeric(gsub(",","",datos$`Muertes c/1Millon`))
datos$`Test Totales` <- as.numeric(gsub(",","",datos$`Test Totales`))
datos$`Test c/1Millon` <- as.numeric(gsub(",","",datos$`Test c/1Millon`))
datos$Poblacion <- as.numeric(gsub(",","",datos$Poblacion))

View(datos)

summary(datos)
summary(datos[,'Casos Activos'])
datos <- datos[-82,]

sapply(datos , function(x) sum(is.na(x)))
nrow(datos)

clean_datos <- na.omit(datos)
nrow(clean_datos)

#VEMOS PORCENTAJE DE NA'S POR VARIABLE

sum(is.na(datos$Pais))/nrow(datos) #0
sum(is.na(datos$`Casos Totales`))/nrow(datos)#0
sum(is.na(datos$`Casos Nuevos`))/nrow(datos)#0.94
sum(is.na(datos$`Muertes Totales`))/nrow(datos)#0.02
sum(is.na(datos$`Muertes Nuevas`))/nrow(datos)#0.97
sum(is.na(datos$`Recuperados Totales`))/nrow(datos)#0.20
sum(is.na(datos$`Nuevos Recuperados`))/nrow(datos)#0.92
sum(is.na(datos$`Casos Activos`))/nrow(datos)#0.20
sum(is.na(datos$`Casos Serios/Criticos`))/nrow(datos)#0.74
sum(is.na(datos$` Casos Infantiles c/1Millon`))/nrow(datos)#0.03
sum(is.na(datos$`Muertes c/1Millon`))/nrow(datos)#0.05
sum(is.na(datos$`Test Totales`))/nrow(datos)#0.10
sum(is.na(datos$`Test c/1Millon`))/nrow(datos)#0.10
sum(is.na(datos$Poblacion))/nrow(datos)#0.04

#CHEQUEAMOS CUANTA INFORMACION SE PIERDE EN BASE A LA CANTIDAD DE 
#VARIABLES ELIMINADAS QUE PRESENTAR UNA MAYOR PERDIDA DE NA'S 

newdatos <- datos[,-c(3,5,7,9)] 

clean_datos_wtv <- na.omit(newdatos)

nrow(clean_datos_wtv)
View(clean_datos_wtv)

#################################################################################

corr_datos1 <- cor(datos[,-1])
corr_datos2 <- cor(clean_datos_wtv[,-1])

corrplot(corr_datos1, method = 'number')
corrplot(corr_datos2, method = 'number')


corrplot(corr_datos1, method = 'square', order = 'AOE', addCoef.col = 'black', tl.pos = 'd',
         cl.pos = 'n', col = COL2('RdYlBu'))

corrplot(corr_datos2, method = 'square', order = 'AOE', addCoef.col = 'black', tl.pos = 'd',
         cl.pos = 'n', col = COL2('RdYlBu'))

