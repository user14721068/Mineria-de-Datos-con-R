#install.packages("dplyr")
#
####################################################################################


#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#
#                       EJERCICIO 2
#     Conocimiento de los datos (analisis exploratorio)
#
#:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

#///////////////////////////////////////////////////////////////////////////////////
#                       EJERCICIO "A"
#///////////////////////////////////////////////////////////////////////////////////

#...................................................................................
#Importacion del archivo, tipado y formacion de Tabla
#...................................................................................

library(dplyr)
#Importacion del archivo, tipado y formacion de Tabla
options(max.print = 60000)
listings<- read.csv("~/Documentos/Almacenes y Minería de Datos/Proyecto_Final/Sistema-de-Mineria-de-Datos/listings.csv")
atributo = c("room_id", "host_id", "room_type", "borough", "neighborhood", "number_of_reviews", "review_scores_rating", "accomodates", "bedrooms", "price", "minimum_nights/maximum_nights", "latitude/longitude", "last_review")
tipo_atributo=c("nominal", "nominal", "ordinal", "ordinal", "nominal", "numerico", "numerico", "numerico", "numerico", "numerico", "numerico", "numerico", "numerico")
dataf = data.frame("room_id", "host_id", "room_type", "borough", "neighborhood", "number_of_reviews", "review_scores_rating", "accomodates", "bedrooms", "price", "minimum_nights/maximum_nights", "latitude/longitude", "last_review")

#...................................................................................
#Tipos de datos
#...................................................................................
#Las siguientes vaiables son de tipo "integer", como podemos observar a continucación:
class(listings$number_of_reviews[1])
class(listings$review_scores_rating[1])
class(listings$accommodates[1])
class(listings$price[1])
class(listings$minimum_nights[1])
class(listings$maximum_nights[1])
class(listings$last_review[1]) 
class(listings$guests_included[1])
class(listings$bedrooms[1])
class(listings$square_feet[1])

#Las siguientes vaiables son de tipo "numeric", como podemos observar a continucación:
class(listings$latitude[1])
class(listings$longitude[1])
class(listings$bathrooms[1])

#Las siguientes vaiables son de tipo "character", como podemos observar a continucación:
class(listings$bed_type[1])
class(listings$room_type[1])
class(listings$property_type[1])

#...................................................................................
#Valores perdidos
#...................................................................................
#is.na.data.frame(listings$review_scores_rating)
sum(is.na.data.frame(listings$review_scores_rating))
sum(complete.cases(data.frame(listings$review_scores_rating)))
#Las únicas variables con datos perdidos son las siguientes, cuyo porcentaje es:
mean(is.na(listings$review_scores_rating))
mean(is.na(listings$last_review))
mean(is.na(listings$square_feet))
mean(is.na(listings$bedrooms))
mean(is.na(listings$bathrooms))

#Las demás no cuentan con datos perdidos:
mean(is.na(listings$number_of_reviews))
mean(is.na(listings$accommodates))
mean(is.na(listings$price))
mean(is.na(listings$minimum_nights))
mean(is.na(listings$maximum_nights))
mean(is.na(listings$latitude))
mean(is.na(listings$longitude))
mean(is.na(listings$guests_included))
mean(is.na(listings$bed_type))
mean(is.na(listings$room_type))
mean(is.na(listings$property_type))

#...................................................................................
#Valores minimos
#...................................................................................
min(listings$number_of_reviews)
min(na.omit(listings$review_scores_rating))
min(listings$accommodates)
min(listings$price)
min(listings$minimum_nights)
min(listings$maximum_nights)
min(listings$latitude)
min(listings$longitude)
min(na.omit(listings$last_review)) 
min(listings$guests_included)
min(na.omit(listings$bedrooms))
min(na.omit(listings$bathrooms))
min(na.omit(listings$square_feet))

#...................................................................................
# Valores maximos
#...................................................................................
max(listings$number_of_reviews)
max(na.omit(listings$review_scores_rating))
max(listings$accommodates)
max(listings$price)
max(listings$minimum_nights)
max(listings$maximum_nights)
max(listings$latitude)
max(listings$longitude)
max(na.omit(listings$last_review))
max(listings$guests_included)
max(na.omit(listings$bedrooms))
max(na.omit(listings$bathrooms))
max(na.omit(listings$square_feet))


#...................................................................................
# Media
#...................................................................................
mean(na.omit(listings$review_scores_rating))
mean(listings$number_of_reviews)
mean(listings$accommodates)
mean(listings$minimum_nights)
mean(listings$maximum_nights)
mean(listings$latitude)
mean(listings$longitude)
mean(listings$price)
mean(listings$guests_included)
mean(na.omit(listings$bedrooms))
mean(na.omit(listings$bathrooms))
mean(na.omit(listings$square_feet))

#...................................................................................
# Desviacion Estandar
#...................................................................................
sd(listings$number_of_reviews)
sd(na.omit(listings$review_scores_rating))
sd(listings$accommodates)
sd(listings$price)
sd(listings$minimum_nights)
sd(listings$maximum_nights)
sd(listings$latitude)
sd(listings$longitude)
sd(na.omit(listings$last_review))
sd(listings$guests_included)
sd(na.omit(listings$bedrooms))
sd(na.omit(listings$bathrooms))
sd(na.omit(listings$square_feet))

#Valores atipicos
#Para los valores atipicos mostramos la gráfica de caja de la variable y observamos esto

#number_of_reviews si tiene datos atipicos
boxplot(listings$number_of_reviews)
#review_scores_rating si tiene atipicos
boxplot(na.omit(listings$review_scores_rating))
#accommodates si tiene atipicos
boxplot(listings$accommodates)
#price si tiene atipicos
boxplot(listings$price)
#minimum_nights si tiene atipicos
boxplot(listings$minimum_nights)
#maximim_nights si tiene atipicos
boxplot(na.omit(listings$maximum_nights))
#latitude_nights si tiene atipicos
boxplot(listings$latitude)
#longitude si tiene atipicos
boxplot(listings$longitude)
#last_review si tiene atipicos
boxplot(na.omit(listings$last_review))
#guests_included si tiene atipicos
boxplot(listings$guests_included)
#bedrooms si tiene atipicos
boxplot(na.omit(listings$bedrooms))
#bedrooms si tiene atipicos
boxplot(na.omit(listings$bedrooms))
#square_feet si tiene atipicos
boxplot(na.omit(listings$square_feet))

#Los siguientes datos no tiene sentido eliminar datos atipicos
#guests_included
#bedrooms
#bathrooms 

#...................................................................................
# Tipos de Distribuciones
#...................................................................................
#Distribucion exponencial
plot(sort(listings$number_of_reviews), type ='l')
#Distribucion logaritmica
plot(sort(na.omit(listings$review_scores_rating)), type ='l')
#Distribucion exponencial
plot(sort(listings$accommodates),type = "l")
#Distribucion exponencial
plot(sort(listings$price),type = "l")
#Distribucion exponencial
plot(sort(listings$minimum_nights),type = "l")
#Distribucion exponencial
plot(sort(listings$maximum_nights),type = "l")
#Distribucion sigmoidal
plot(sort(listings$latitude),type = "l")
#Distribucion sigmoidal
plot(sort(listings$longitude),type = "l")
#Distribucion exponencial
plot(sort(listings$bedrooms),type = "l")
#Distribucion exponencial
plot(sort(listings$bathrooms),type = "l")
#Distribucion exponencial
plot(sort(listings$square_feet),type = "l")

#///////////////////////////////////////////////////////////////////////////////////
#                       EJERCICIO "B"
#///////////////////////////////////////////////////////////////////////////////////

#Este historiograma va de number_of_reviews a precio  
hist(listings$number_of_reviews, main = "variable Y y objetivo", ylab = "Frecuencia")
hist(listings$price, add = TRUE, col = rgb(1, 0, 1, alpha = .1))

#Este historiograma va de numero de reseñas a precio  
hist(listings$review_scores_rating, main = "variable Y y objetivo", ylab = "Frecuencia")
hist(listings$price, add = TRUE, col = rgb(1, 0, 1, alpha = .1))

#Este historiograma va de numero de reseñas a precio  
hist(listings$guests_included, main = "variable Y y objetivo", ylab = "Frecuencia")
hist(listings$price, add = TRUE, col = rgb(1, 0, 1, alpha = .1))

#Este historiograma va de numero de reseñas a precio  
hist(listings$bedrooms, main = "variable Y y objetivo", ylab = "Frecuencia")
hist(listings$price, add = TRUE, col = rgb(1, 0, 1, alpha = .1))

#Este historiograma va de numero de reseñas a precio  
hist(listings$bathrooms, main = "variable Y y objetivo", ylab = "Frecuencia")
hist(listings$price, add = TRUE, col = rgb(1, 0, 1, alpha = .1))

#Este historiograma va de numero de reseñas a precio  
hist(listings$bed_type, main = "variable Y y objetivo", ylab = "Frecuencia")
hist(listings$price, add = TRUE, col = rgb(1, 0, 1, alpha = .1))

#Este historiograma va de numero de reseñas a precio  
hist(listings$room_type, main = "variable Y y objetivo", ylab = "Frecuencia")
hist(listings$price, add = TRUE, col = rgb(1, 0, 1, alpha = .1))

#Este historiograma va de numero de reseñas a precio  
hist(listings$property_type, main = "variable Y y objetivo", ylab = "Frecuencia")
hist(listings$price, add = TRUE, col = rgb(1, 0, 1, alpha = .1))

#Este historiograma va de numero de reseñas a precio
factor_squart_feets <- as.factor(listings$squart_feets)

hist(factor_squart_feets, main = "variable Y y objetivo", ylab = "Frecuencia")
hist(listings$price, add = TRUE, col = rgb(1, 0, 1, alpha = .1))

#///////////////////////////////////////////////////////////////////////////////////
#                       EJERCICIO "C"
#///////////////////////////////////////////////////////////////////////////////////

#...................................................................................
# Grafica de dispersion 
#...................................................................................
# La variable objetivo que se ha escogido es price, por lo que graficaremos 
# la relación entre esta y las vaiables anteriores
library(ggplot2)
#A mayor cantidad de revisiones más barata es el precio
qplot(data=listings, x=price, y= number_of_reviews, colour = "red")
#A mejores reseñas mayor el precio (?)
qplot(data=listings, x=price, y= review_scores_rating, colour = "red")
#No tiene relaacion
qplot(data=listings, x=price, y= accommodates, colour = "red")
#No tiene relación
qplot(data=listings, x=price, y= minimum_nights, colour = "red")
#NOTA: Se necesita eliminar datos atipicos para poder analizar este gráfico, sin relación
qplot(data=listings_clean, x=price, y= maximum_nights.R, colour = "red")
#No tiene relación
qplot(data=listings, x=price, y= latitude, colour = "red")
#No tiene relación
qplot(data=listings, x=price, y= longitude, colour = "red")
#No tiene relación
qplot(data=listings, x=price, y= last_review, colour = "red")
#Más barata entre más huespedes aloje
qplot(data=listings, x=price, y= guests_included, colour = "red")
#Las precios caron indican pocas habitaciones
qplot(data=listings, x=price, y= bedrooms, colour = "red")
#A mayor cantidad de baños, mayor es el precio
qplot(data=listings, x=price, y= bathrooms, colour = "red")
#Más cara la cama, más cara la habitación
qplot(data=listings, x=price, y= bed_type, colour = "red")
#Las habitaciones compartidas son mmás baratas que las privadas, y estás más que los apartamentos
qplot(data=listings, x=price, y= room_type, colour = "red")
#Depende del tipo de propiedad esta puede ser más cara o barata
qplot(data=listings, x=price, y= property_type, colour = "red")
#A mayor cantidad de metros cuadrados mayor precio
qplot(data=listings, x=price, y= square_feet, colour = "red")+ xlim(0, 500) + ylim(0, 1000)

#Por el analisis anterior, nos quedaremos con los siguientes atributos, ya que ejercen influencia sobre la variable precio:
#number_of_reviews
#review_scores_rating
#guests_included
#bedrooms
#bathrooms
#bed_type
#room_type
#property_type
#square_feet