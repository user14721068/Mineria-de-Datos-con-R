library(tidyverse)
library(rpart)
library(rpart.plot)
library(caret)

####################################################################################
# Sistema de Minería de Datos   
####################################################################################

#Arbol CART

#leemos el .csv
listings_clean<- read.csv("~/Documentos/Almacenes y Minería de Datos/Proyecto_Final/Sistema-de-Mineria-de-Datos/datospreprocesados.csv")

#Creamos la semilla
set.seed(1649)

#Creamos los conjuntos de entrenamiento y prueba, particionando el dataset original
listings_entrenamiento <- sample_frac(listings_clean, .7)
listings_prueba <- setdiff(listings_clean, listings_entrenamiento)
listings_prueba <- listings_prueba[listings_prueba$property_type != "Nature lodge" & 
                                     listings_prueba$property_type != "Timeshare" & listings_prueba$property_type != "Train", ]

#Creamos el modelo del árbol
arbol_1 <- rpart(formula = price.C ~ (number_of_reviews + review_scores_rating + 
                                        guests_included + bedrooms + bed_type + room_type + property_type + square_feet.C 
                                      + guests_included.C + number_of_reviews.C + review_scores_rating.C), 
                 control = rpart.control(cp=0.00032, minbucket = 0),
                 data = listings_entrenamiento, method="class")

#0.000188
#Se muestra el árbol generado
rpart.plot(arbol_1, box.palette="Blues", main="Árbol de partición generado para Listings",extra=2,under=TRUE,varlen=0,faclen=0,cex=.6)

#Se muestran las especificaciones del árbol
printcp(arbol_1)

#Generamos la predicción
prediccion_1 <- predict(arbol_1, listings_prueba , type="class")

#Generamos la matriz de confusion y se muestran los resultados de la evaluación
confusionMatrix(prediccion_1, factor(listings_prueba[["price.C"]]))