####################################################################################
# Sistema de Minería de Datos   
####################################################################################

#Reglas de asociacion
library(tidyverse)
library(arules)

#leemos el .csv
listings<- read.csv("~/Documentos/Almacenes y Minería de Datos/Proyecto_Final/Sistema-de-Mineria-de-Datos/datospreprocesados.csv")

#Nota: Se usará una fracción reducida del conjunto de datos para generar las transacciones y reglas para evitar que el conjunto de estas últimas
#genere una explosión combinatoria
listings_m <- sample_frac(listings, .1)
listings <- listings_m
dim(listings)
#listings_RN_prueba <- setdiff(listings_RN, listings_RN_entrenamiento)

tid <- as.character(listings[["id"]])
listings <- listings[,-1]

#Hacemos que todas las columnas sean de tipo factor
for(i in 1:ncol(listings))listings[[i]]<-as.factor(listings[[i]])
trans <- as(listings, "transactions")

#Estaclecemos el ID de la transacción 
transactionInfo(trans)[["transactionID"]] <- tid 
inspect(trans[1:5])

# Reglas de asociación

#Calcula las reglas de asociación a partir del algoritmo apriori, con una confianza del 70%
# Se especifica en los parametros de la parte derecha de cada regla que los únicos valores permitidos
#son los de la variable discretizada de precio "price.C"
soporte <- 30 / dim(trans)[1]
reglas <- apriori(data = trans,
                  parameter = list(support = soporte,
                                   minlen = 5, #De esta manera garantizamos que la mayoria de atributos se vean implicados en las reglas
                                   confidence = 0.70,
                                   # Se especifica que se creen reglas
                                   target = "rules"),
                  appearance = list(rhs = c("price.C=economico", "price.C=accesible", "price.C=exclusivo"),
                                    lhs=c("number_of_reviews.C=Muchas","number_of_reviews.C=Muy pocas", "number_of_reviews.C=Pocas", "number_of_reviews.C=Regular","number_of_reviews.C=Varias",
                                          "bathrooms=0","bathrooms=1","bathrooms=3","bathrooms=4","bathrooms=6", "bedrooms=1","bedrooms=2","bedrooms=3","bedrooms=4","bedrooms=5", 
                                          "bed_type=Airbed","bed_type=Couch","bed_type=Futon","bed_type=Real Bed","bed_type=Pull-out Sofa", "property_type=Apartment","property_type=Bed and breakfast", "property_type=Boutique hotel","property_type=Bungalow","property_type=Loft","property_type=Other","property_type=Resort",
                                          "property_type=Serviced apartment", "property_type=Villa",
                                          "room_type=Entire home/apt","room_type=Private room","room_type=Shared room", "review_scores_rating.C=Buena","review_scores_rating.C=Excelente",
                                          "review_scores_rating.C=Mala","review_scores_rating.C=Regular","guests_included=1","guests_included=2","guests_included=3","square_feet.C=mediana",
                                          "square_feet.C=muy grande","square_feet.C=pequeño","square_feet.C=reducido")))

#Nos da información general de las reglas, incluyendo los valores de promedio, minimo, maximo, mediana, etc.
# del soporte, confiancia, cobertura, y otras medidas de las reglas
summary(reglas)

#Nos devuelve un desglose completo de las reglas ordenadas en orden decreciente de confianza,
#esto incluye el soporte, la confianza, cobertura, lift y frecuencuencia
#De esta manera obtenemos las reglas con mayor confianza que nos son útiles para obtener nuestra variable objetivo
inspect(sort(x = reglas, decreasing = TRUE, by = "confidence")[1:20])

#Como vemos, la mayor confianza que se tiene es del 87%, vamos a comparar con el metodo de ruleInduction

closed <- apriori(trans, 
                  parameter = list(target = "closed", minlen = 5, support = soporte, confidence = 0.7))

reglas_t <- ruleInduction(closed, trans, verbose = TRUE)

#Nos da información general de las reglas
summary(reglas_t)

#Obtenemos las reglas con mayor confianza que nos son útiles para obtener nuestra variable objetivo
inspect(sort(x = reglas_t, decreasing = TRUE, by = "confidence")[1:20])
