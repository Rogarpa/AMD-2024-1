library(ggplot2)

data = read.csv(file = "../code/R/proyecto/src/globalterrorismdb_0718dist.csv", sep=",", header = T, stringsAsFactors = F)

##### SELECCION DE ATRIBUTOS #####

seleccion_de_atributos <- data[, c(
  "eventid", "iyear", "imonth", "iday",
  "country", "country_txt", "region", "region_txt", "provstate", "city", "latitude", "longitude",
  "specificity", "vicinity", "attacktype1", "attacktype1_txt", "targtype1", "targtype1_txt",
  "extended", "suicide"
)]

####### VALORES PERDIDOS #######

valores_perdidos <- function(datos) {
  resultados <- matrix(nrow = ncol(datos), ncol = 2)
  
  for (i in seq_along(datos)) {
    nombre_columna <- names(datos)[i]
    cantidad_perdidos <- sum(is.na(datos[, i]) | datos[, i] == "")
    resultados[i, ] <- c(nombre_columna, cantidad_perdidos)
  }
  
  colnames(resultados) <- c("Columna", "Valores_Perdidos")
  return(resultados)
}

valores_perdidos_data <- valores_perdidos(seleccion_de_atributos)

# Reemplazar NA en "latitude" por la media
media_latitude <- mean(seleccion_de_atributos$latitude, na.rm = TRUE)
seleccion_de_atributos$latitude[is.na(seleccion_de_atributos$latitude)] <- media_latitude

# Reemplazar NA en "longitude" por la media
media_longitude <- mean(seleccion_de_atributos$longitude, na.rm = TRUE)
seleccion_de_atributos$longitude[is.na(seleccion_de_atributos$longitude)] <- media_longitude

# Reemplazar los valores NA en la columna 'specificity' con 0
seleccion_de_atributos$specificity[is.na(seleccion_de_atributos$specificity)] <- 0

# Reemplazar los valores vacíos y cadenas vacías con "Desconocido" en las columnas "provstate" y "city"
seleccion_de_atributos$provstate[seleccion_de_atributos$provstate == ""] <- "Desconocido"
seleccion_de_atributos$city[seleccion_de_atributos$city == ""] <- "Desconocido"

####### VALORES ATÍPICOS #######

par(mfrow = c(4, 4), mar = c(2, 2, 2, 2))
boxplot(seleccion_de_atributos$iyear, ylab = "iyear")
boxplot(seleccion_de_atributos$imonth, ylab = "imonth")
boxplot(seleccion_de_atributos$iday, ylab = "iday")
boxplot(seleccion_de_atributos$country, ylab = "country")
boxplot(seleccion_de_atributos$region, ylab = "region")
boxplot(seleccion_de_atributos$latitude, ylab = "latitude")
boxplot(seleccion_de_atributos$longitude, ylab = "longitude")
boxplot(seleccion_de_atributos$specificity, ylab = "specificity")
boxplot(seleccion_de_atributos$vicinity, ylab = "vicinity")
boxplot(seleccion_de_atributos$attacktype1, ylab = "attacktype1")
boxplot(seleccion_de_atributos$targtype1, ylab = "targtype1")
boxplot(seleccion_de_atributos$extended, ylab = "extended")
boxplot(seleccion_de_atributos$suicide, ylab = "suicide")

# Extract potential outliers based on IQR criterion
outliers <- boxplot.stats(seleccion_de_atributos$targtype1)$out
outliers <- boxplot.stats(seleccion_de_atributos$longitude)$out

# Identify row numbers corresponding to outliers
outlier_indices <- which(seleccion_de_atributos$targtype1 %in% outliers)
outlier_indices <- which(seleccion_de_atributos$longitude %in% outliers)

# Print rows corresponding to outliers
seleccion_de_atributos[outlier_indices, ]
