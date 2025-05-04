####################################################################################
# Sistema de Minería de Datos   
####################################################################################

#Importacion del archivo, tipado y formacion de Tabla
options(max.print = 60000)
listings<- read.csv("~/Documentos/Almacenes y Minería de Datos/Proyecto_Final/Sistema-de-Mineria-de-Datos/listings.csv")


#------------------------Preprocesamiento de datos:---------------------------------------x

library(dplyr)

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

include <- names(listings) %in% c("id","number_of_reviews", "review_scores_rating", "guests_included", "bedrooms", 
                               "bathrooms", "bed_type", "room_type", "property_type", "square_feet","price")
listings <- listings[,include]
head(listings)

# Se utilizará la siguiente funcion para eliminar los valores atipicos y perdidos de una lista de variables
# df es el dataFrame que recibimos 
# colNameData es la columna de los datos 
eliminar_v_atipicos <- function(df, l_colNameData){
  for (colNameData in l_colNameData){
    # creamos una nueva columna llamada igual que colNameData pero con .R
    colNameData.R <- paste(colNameData, "R", sep=".")
    df[colNameData.R] <- df[colNameData]
    data <- df[, c(colNameData) ]
    # Se crea una columna de referencia para elminar los datos faltantes
    colNameData.S <- paste(colNameData, "S", sep=".")
    df[colNameData.S] <- df[colNameData]
    
    Q  <- quantile(na.omit(data))
    minimo <- Q[1]    # valor minimo
    Q1     <- Q[2]    # primer cuartil
    Me     <- Q[3]    # mediana
    Q3     <- Q[4]    # tercer cuartil
    maximo <- Q[5]    # valor maximo
    IQR    <- Q3 - Q1
    
    lowLimit  <- max(minimo, Q1 - 1.5*IQR)
    lowLimit 
    highLimit <- min(maximo, Q3 + 1.5*IQR)
    highLimit
    if(anyNA(df[colNameData])) {
      df[is.na(df[colNameData]), c(colNameData.S)] <- Me #S e sustituyen los datos faltantes por la mediana
      df[colNameData.R] <- df[colNameData.S]
    }
    # todos los valores donde colNameBy es igual a id
    # y el valor de colNameData es > Q3 + 1.5 * IQR
    # lo reemplazamos por la mediana
    df[df[colNameData.S] > highLimit, c(colNameData.R)] <- Me
    # lo mismo para el umbral inferior
    df[df[colNameData.S] < lowLimit, c(colNameData.R)] <- Me
    
    #Se asigna la columna y se eliminan las columnas auxiliares
    df[colNameData] = df[colNameData.R]
    drop <- names(df) %in% c(colNameData.S,colNameData.R)
    df <- df[,!drop]
  }
  df   # devolvemos el valor del dataFrame
}

eliminar_v_perdidos <- function(df, l_colNameData){
  for (colNameData in l_colNameData){
    data <- df[, c(colNameData) ]
    # Se crea una columna de referencia para elminar los datos faltantes
    colNameData.S <- paste(colNameData, "S", sep=".")
    df[colNameData.S] <- df[colNameData]
    
    Q  <- quantile(na.omit(data))
    minimo <- Q[1]    # valor minimo
    Q1     <- Q[2]    # primer cuartil
    Me     <- Q[3]    # mediana
    Q3     <- Q[4]    # tercer cuartil
    maximo <- Q[5]    # valor maximo
    IQR    <- Q3 - Q1
    
    lowLimit  <- max(minimo, Q1 - 1.5*IQR)
    highLimit <- min(maximo, Q3 + 1.5*IQR)
    if(anyNA(df[colNameData])) {
      df[is.na(df[colNameData]), c(colNameData.S)] <- Me #S e sustituyen los datos faltantes por la media
    }
    
    #Se asigna la columna y se eliminan las columnas auxiliares
    df[colNameData] = df[colNameData.S]
    drop <- names(df) %in% c(colNameData.S)
    df <- df[,!drop]
  }
  df   # devolvemos el valor del dataFrame
}

#A continuación, se limpian las variables predictivas:

#Eliminamos los outliers de las columnas especificadas, y si tienen valores nulos tambien los sustituyen por la media
listings_clean <- eliminar_v_atipicos(listings,c("number_of_reviews","guests_included","review_scores_rating","square_feet","price"))

#Eliminamos valores perdidos 
#Nota, no eliminamos los valores atipicos de estas dos varibles pues el número de baños y habitaciones depende del tamaño de la casa,
#casas grandes hay pocas
listings_clean <- eliminar_v_perdidos(listings_clean,c("bedrooms","bathrooms"))

#Por el analisis anterior, nos quedaremos con los siguientes atributos, ya que ejercen influencia sobre la variable precio:
#number_of_reviews
summary(listings$number_of_reviews)
summary(listings_clean$number_of_reviews)
#review_scores_rating
summary(listings$review_scores_rating)
summary(listings_clean$review_scores_rating)
#guests_included
summary(listings$guests_included)
summary(listings_clean$guests_included)
#bedrooms
summary(listings$bedrooms)
summary(listings_clean$bedrooms)
#bathrooms
summary(listings$bathrooms)
summary(listings_clean$bathrooms)
#bed_type
summary(listings$bed_type)
summary(listings_clean$bed_type)
#room_type
summary(listings_clean$room_type)
#property_type
summary(listings_clean$property_type)
#square_feet
summary(listings$square_feet)
summary(listings_clean$square_feet)

#Discretizamos las variables numericas, como veremos, crearemos una nueva columna para la clasificación:

# Convertir la variable numerica "number_of_reviews" en categorica
# para ello definimos los puntos de corte
puntosCorte <- c(-Inf, 11, 22, 33, 44, Inf)
categorias_5 <- c("Muy pocas", "Pocas", "Regular", "Varias", "Muchas")
# y cortamos la variable número de pasos segun esta categorizacion
listings_clean$number_of_reviews.C <- cut(listings_clean$number_of_reviews, breaks = puntosCorte, labels = categorias_5)
summary(listings_clean$number_of_reviews.C)

# Convertir la variable numerica "review_scores_rating" en categorica
puntosCorte <- c(70, 85, 90, 95, 100)
categorias <- c("Mala", "Regular", "Buena", "Excelente")
listings_clean$review_scores_rating.C <- cut(listings_clean$review_scores_rating, breaks = puntosCorte, labels = categorias)
summary(listings_clean$review_scores_rating.C)

# Convertir la variable numerica "guests_included" en categorica
puntosCorte <- c(0, 1, 2, 3, Inf)
categorias <- c("uno", "dos", "tres", "cuatro")
listings_clean$guests_included.C <- cut(listings_clean$guests_included, breaks = puntosCorte, labels = categorias)
summary(listings_clean$guests_included.C)

# Convertir la variable numerica "square_feet" en categorica
puntosCorte <- c(-Inf, 360, 720, 1080, 1440, Inf)
categorias <- c("pequeño", "reducido", "mediana", "grande", "muy grande")
listings_clean$square_feet.C <- cut(listings_clean$square_feet, breaks = puntosCorte, labels = categorias)
summary(listings_clean$square_feet.C)

# Convertir la variable numerica "price" en categorica
puntosCorte <- c(-Inf, 85, 170, Inf)
categorias <- c("economico", "accesible", "exclusivo")
listings_clean$price.C <- cut(listings_clean$price, breaks = puntosCorte, labels = categorias)
summary(listings_clean$price.C)

#Normalización de variables
#Normalizar nos permite conservar la relación que existe entre los datos, 
#Por lo anterior, se genera una nueva columna con los datos ya normalizados
#number_of_reviews
listings_clean$number_of_reviews.N <- (listings_clean$number_of_reviews - mean (listings_clean$number_of_reviews)) / sd (listings_clean$number_of_reviews)
#review_scores_rating
listings_clean$review_scores_rating.N <- (listings_clean$review_scores_rating - mean (listings_clean$review_scores_rating)) / sd (listings_clean$review_scores_rating)
#guests_included
listings_clean$guests_included.N <- (listings_clean$guests_included - mean (listings_clean$guests_included)) / sd (listings_clean$guests_included)
#bedrooms
listings_clean$bedrooms.N <- (listings_clean$bedrooms - mean (listings_clean$bedrooms)) / sd (listings_clean$bedrooms)
#bathrooms
listings_clean$bathrooms.N <- (listings_clean$bathrooms - mean (listings_clean$bathrooms)) / sd (listings_clean$bathrooms)
#square_feet
listings_clean$square_feet.N <- (listings_clean$square_feet- mean (listings_clean$square_feet)) / sd (listings_clean$square_feet)
#price
listings_clean$price.N <- (listings_clean$price - mean (listings_clean$price)) / sd (listings_clean$price)

#Finalmente, exportamos el dataframe
write.csv(listings_clean, "datospreprocesados.csv")