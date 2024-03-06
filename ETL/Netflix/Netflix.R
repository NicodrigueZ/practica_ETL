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

###############################################################################################################################
#########################################        ETL DATASET NETFLIX       ####################################################
###############################################################################################################################

rm(list = ls())

getwd()
setwd("C:/Users/Facundo/Desktop/UNO/DS/ETL/Netflix/DataSet")

datanet <- read_csv("netflix1.csv")
View(datanet)
str(datanet)
attach(datanet)

datanet_names<-c("Id_Programa","Tipo","Titulo","Director","Pais"
             ,"Fecha_Adhesion","Año_de_lanzamiento","Clasificacion"
             ,"Duracion","Genero")

names(datanet) <- datanet_names

datanet$Id_Programa <- as.character(datanet$Id_Programa)
datanet$Tipo <- as.character(datanet$Tipo)
datanet$Titulo <- as.character(datanet$Titulo)
datanet$Director <- as.character(datanet$Director)
datanet$Pais <- as.character(datanet$Pais)
datanet$Fecha_Adhesion <- as.Date(datanet$Fecha_Adhesion, format ="%m/%d/%Y")
datanet$Año_de_lanzamiento <- as.numeric(datanet$Año_de_lanzamiento)
datanet$Clasificacion <- as.character(datanet$Clasificacion)
datanet$Duracion <- as.character(datanet$Duracion)
datanet$Genero <- as.character(datanet$Genero)

View(datanet)
str(datanet)

#######################################################################################

datanetNA <- datanet

sapply(datanetNA, function(x) sum(is.na(x)))

View(datanetNA)

print(datanetNA[c(34,53),c(4,5)])
print(any(datanet$Director == 'Not Given' | datanet$Pais == "Not Given")) #TRUE

datanetNA$Director[datanetNA$Director == 'Not Given'] <- NA #Casterar a NA
datanetNA$Pais[datanetNA$Pais == 'Not Given'] <- NA #Castear a NA

print(datanetNA[c(34,53),c(4,5)])

sapply(datanetNA, function(x) sum(is.na(x)))

sum(is.na(datanetNA$director))/nrow(datanetNA) #0.29
sum(is.na(datanetNA$country))/nrow(datanetNA)#0.03

datanet_clean1 <- na.omit(datanetNA) # 32.70%
datanet_clean2 <- datanetNA[,-c(4,5)] # 0.0%

#######################################################################################

frecmovies <- as.numeric(table(datanet$type)["Movie"])
frectv <- as.numeric(table(datanet$type)["TV Show"])

print(frecmovies)
print(frectv)

movies <- sum(datanet_clean2$type=="Movie")
tvshow <- sum(datanet_clean2$type=="TV Show")

print(movies)
print(tvshow)

colors <- c('rgb(211,94,96)','rgb(114,147,203)')

fig1 <- plot_ly(data = datanet_clean1, labels = ~type, values = ~length(datanet_clean1$type), type = 'pie',
               textposition = 'inside',
               textinfo = 'label+percent',
               insidetextfont = list(color = '#FFFFFF'),
               hoverinfo = 'text',
               marker = list(colors = colors,
                             line = list(color = '#FFFFFF', width = 1)),
               #The 'pull' attribute can also be used to create space between the sectors
               showlegend = FALSE)
fig1 <- fig1 %>% layout(title = 'TV Show vs Movies Without NA Values',
                      xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                      yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

fig1

fig2 <- plot_ly(data = datanet_clean2, labels = ~type, values = ~length(datanet_clean2$type), type = 'pie',
               textposition = 'inside',
               textinfo = 'label+percent',
               insidetextfont = list(color = '#FFFFFF'),
               hoverinfo = 'text',
               marker = list(colors = colors,
                             line = list(color = '#FFFFFF', width = 1)),
               #The 'pull' attribute can also be used to create space between the sectors
               showlegend = FALSE)
fig2 <- fig2 %>% layout(title = 'TV Show vs Movies Without Director & Country',
                      xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                      yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

fig2
