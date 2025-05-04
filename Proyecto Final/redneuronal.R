####################################################################################
# Sistema de Minería de Datos   
####################################################################################

#Red Neuronal

#leemos el .csv
listings<- read.csv("~/Documentos/Almacenes y Minería de Datos/Proyecto_Final/Sistema-de-Mineria-de-Datos/datospreprocesados.csv")

library(neuralnet)
library(tidyverse)
library(rpart)
library(rpart.plot)
library(caret)

#Con el objetivo de reconvertir las variables discretizadas anteriormente a valores númericos para que la topología de la red las tome como varibles predictivas,
#se crean nuevas discretizaciones con números para identificar cada una 

# Convertir la variable numerica "number_of_reviews" en categorica
puntosCorte <- c(-Inf, 11, 22, 33, 44, Inf)
categorias <- c("0", "1", "2", "3", "4")
listings_clean$number_of_reviews.CN <- as.numeric(cut(listings_clean$number_of_reviews, breaks = puntosCorte, labels = categorias))

# Convertir la variable numerica "review_scores_rating" en categorica
puntosCorte <- c(70, 85, 90, 95, 100)
categorias <- c("0", "1", "2", "3")
listings_clean$review_scores_rating.CN <- as.numeric(cut(listings_clean$review_scores_rating, breaks = puntosCorte, labels = categorias))
summary(listings_clean$review_scores_rating.CN)

# Convertir la variable numerica "guests_included" en categorica
puntosCorte <- c(0, 1, 2, 3, Inf)
categorias <- c("0", "1", "2", "3")
listings_clean$guests_included.CN <- as.numeric(cut(listings_clean$guests_included, breaks = puntosCorte, labels = categorias))
summary(listings_clean$guests_included.CN)

# Convertir la variable numerica "square_feet" en categorica
puntosCorte <- c(-Inf, 360, 720, 1080, 1440, Inf)
categorias <- c("0", "1", "2", "3", "4")
listings_clean$square_feet.CN <- as.numeric(cut(listings_clean$square_feet, breaks = puntosCorte, labels = categorias))
summary(listings_clean$square_feet.CN)

# Convertir la variable numerica "price" en categorica
puntosCorte <- c(-Inf, 85, 170, Inf)
categorias <- c("0", "1", "2")
listings_clean$price.CN <- as.numeric(cut(listings_clean$price, breaks = puntosCorte, labels = categorias))
summary(listings_clean$price.CN)

#Convertimos la variable categorica "bed_type" en númerica
levels <- c("Airbed","Couch","Futon","Pull-out Sofa","Real Bed")
listings_clean$bed_type.CN <- match(listings_clean$bed_type, levels)

#Convertimos la variable categorica "room_type" en númerica
levels <- c("Entire home/apt","Private room","Shared room")
listings_clean$room_type.CN <- match(listings_clean$room_type, levels)

#Convertimos la variable categorica "property_type" en númerica
levels <- c("Aparthotel","Apartment","Bed and breakfast","Boat","Boutique hotel","Bungalow","Cabin","Camper/RV","Casa particular (Cuba)",
            "Castle","Cave","Chalet","Condominium","Cottage","Earth house","Guest suite","Guesthouse","Hostel","Hotel","House","Houseboat",
            "Island","Loft","Nature lodge","Other","Resort","Serviced apartment","Tent","Timeshare","Tiny house","Townhouse","Train","Villa")
listings_clean$property_type.CN <- match(listings_clean$property_type, levels)

#Se eliminan los atributos del dataset que no son útiles para la tarea de predicción


names(listings_clean)
v_obj <- names(listings_clean) %in% c("number_of_reviews.CN", "review_scores_rating.CN", "guests_included.CN", "bedrooms", 
                                      "bathrooms", "bed_type.CN", "room_type.CN", "property_type.CN", "square_feet.CN","price.C")
listings_RN <- listings_clean[,v_obj]

#Hacemos un un one hot encoding para las variables objetivo:

listings_tmp <- dummyVars(" ~ price.C", data = listings_RN)
listings_tmp_f <- data.frame(predict(listings_tmp, newdata = listings_clean))

#Asignamos las columnas a nuestro conjunto de datos
listings_RN$price.C.economico <- unlist(listings_tmp_f[1])
listings_RN$price.C.accesible <- unlist(listings_tmp_f[2])
listings_RN$price.C.exclusivo <- unlist(listings_tmp_f[3])

listings_RN_entrenamiento <- sample_frac(listings_RN, .66)
listings_RN_prueba <- setdiff(listings_RN, listings_RN_entrenamiento)

#creamos el modelo


red <- neuralnet((price.C.economico + price.C.accesible + price.C.exclusivo) ~ (bedrooms +
                                                                                  review_scores_rating.CN + square_feet.CN + bed_type.CN + room_type.CN + property_type.CN), 
                 listings_RN_entrenamiento, hidden=c(8,12,14,8), linear.output = FALSE, stepmax = 1000000)

#Graficamos la red neuronal
plot(red)
red

prueba <- listings_RN_prueba[,1:10]
head(prueba[,-3])

#Se genera la predicción
prediccion_rn <- compute(red, prueba[,-3])