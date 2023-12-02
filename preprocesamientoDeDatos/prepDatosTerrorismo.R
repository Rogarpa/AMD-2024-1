library(ggplot2)

data = read.csv(file = "../code/R/proyecto/src/globalterrorismdb_0718dist.csv", sep=",", header = T, stringsAsFactors = F)

##### SELECCION DE ATRIBUTOS #####

seleccion_de_atributos <- data[, c(
  "eventid", "iyear", "imonth", "iday",
  "country", "country_txt", "region", "region_txt", "provstate", "city", "latitude", "longitude",
  "specificity", "vicinity", "attacktype1", "attacktype1_txt", "attacktype2", "attacktype2_txt",
  "attacktype3", "attacktype3_txt", "extended", "success", "suicide"
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

# Reemplazar NA en  por la moda
moda_attacktype2 <- as.numeric(names(sort(table(seleccion_de_atributos$attacktype2), decreasing = TRUE)[1]))
seleccion_de_atributos$attacktype2[is.na(seleccion_de_atributos$attacktype2)] <- moda_attacktype2
moda_attacktype3 <- as.numeric(names(sort(table(seleccion_de_atributos$attacktype3), decreasing = TRUE)[1]))
seleccion_de_atributos$attacktype3[is.na(seleccion_de_atributos$attacktype3)] <- moda_attacktype3

# Reemplazar las cadenas vacías con a en las columnas "provstate", "city", "attacktype2" y "attacktype3"
seleccion_de_atributos$provstate[seleccion_de_atributos$provstate == ""] <- "Desconocido"
seleccion_de_atributos$city[seleccion_de_atributos$city == ""] <- "Desconocido"
seleccion_de_atributos$attacktype2_txt[seleccion_de_atributos$attacktype2_txt == ""] <- "Armed Assault"
seleccion_de_atributos$attacktype3_txt[seleccion_de_atributos$attacktype3_txt == ""] <- "Facility/Infrastructure Attack"

####### VALORES ATÍPICOS #######

par(mfrow = c(4, 4), mar = c(2, 2, 2, 2))
boxplot(seleccion_de_atributos$country, ylab = "country")
boxplot(seleccion_de_atributos$latitude, ylab = "latitude")
boxplot(seleccion_de_atributos$longitude, ylab = "longitude")
boxplot(seleccion_de_atributos$specificity, ylab = "specificity")
boxplot(seleccion_de_atributos$attacktype1, ylab = "attacktype1")
boxplot(seleccion_de_atributos$attacktype2, ylab = "attacktype2")
boxplot(seleccion_de_atributos$attacktype3, ylab = "attacktype3")


# Extract potential outliers based on IQR criterion
outliers_country <- boxplot.stats(seleccion_de_atributos$country)$out
outliers_latitude <- boxplot.stats(seleccion_de_atributos$latitude)$out
outliers_longitude <- boxplot.stats(seleccion_de_atributos$longitude)$out
outliers_specificity <- boxplot.stats(seleccion_de_atributos$specificity)$out
outliers_attacktype1 <- boxplot.stats(seleccion_de_atributos$attacktype1)$out
outliers_attacktype2 <- boxplot.stats(seleccion_de_atributos$attacktype2)$out
outliers_attacktype3 <- boxplot.stats(seleccion_de_atributos$attacktype3)$out

# Identify row numbers corresponding to outliers
outlier_indices_country <- which(seleccion_de_atributos$country %in% outliers_country)
outlier_indices_latitude <- which(seleccion_de_atributos$longitude %in% outliers_latitude)
outlier_indices_longitude <- which(seleccion_de_atributos$longitude %in% outliers_longitude)
outlier_indices_attacktype1 <- which(seleccion_de_atributos$longitude %in% outliers_attacktype1)

# Print rows corresponding to outliers
seleccion_de_atributos[outlier_indices_country, ]
seleccion_de_atributos[outlier_indices_latitude, ]
seleccion_de_atributos[outlier_indices_attacktype1, ]
