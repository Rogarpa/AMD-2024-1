#leemos nuestros datos
data = read.csv(file = "C:/AMD/globalterrorismdb_0718dist.csv", sep=",", header = T, stringsAsFactors = F,encoding = "UTF-8")

#hacemos una tabla con el conteo de las categorias de 1 a 22
tabla_frecuencia <- table(data$targtype1)
print(tabla_frecuencia)

# Cargar librerías
library(ggplot2)

# Frecuencia de valores en targtype1
value_counts <- table(data$targtype1)

# boxplot targtype1 para ver si tiene valores atipicos
boxplot(data$targtype1, horizontal = TRUE, main = "Boxplot de targtype1")

# Visualización de Frecuencia de targtype1
barplot(value_counts, main = "Frecuencia de valores en targtype1", xlab = "targtype1", ylab = "Frecuencia")



# Calcular el rango intercuartílico (IQR) para ver si targtype tiene valore
Q1 <- quantile(data$targtype1, 0.25)
Q3 <- quantile(data$targtype1, 0.75)
IQR <- Q3 - Q1

# Definir límites para identificar valores atípicos
lower_limit <- Q1 - 1.5 * IQR
upper_limit <- Q3 + 1.5 * IQR

# Identificar valores atípicos
outliers <- data$targtype1 < lower_limit | data$targtype1 > upper_limit

# Mostrar valores atípicos
outlier_values <- data$targtype1[outliers]
cat("Valores atípicos en targtype1:", unique(outlier_values), "\n")

tabla_frecuencia <- table(data$targtype1_txt)
print(tabla_frecuencia)

# Obtiene los niveles únicos en el orden en que aparecen los datos
unique_levels <- unique(data$targtype1_txt)

# Convierte a factor con niveles manuales
data$targtype1_txt <- factor(data$targtype1_txt, levels = unique_levels)

# Verifica que la columna haya sido convertida a factor con los niveles deseados
str(data$targtype1_txt)

#impime
print(data$targtype1_txt)

#resultado <- factor((Private Citizens & Property,Government (Diplomatic),Journalists & Media,Police,Utilities,Military,Government (General),Airports & Aircraft,Business,Educational Institution,Violent Political Party ,Religious Figures/Institutions,Unknown,Transportation,Tourists,NGO,Telecommunication,Food or Water Supply,Terrorists/Non-State Militia,Other,Maritime, Abortion Related),levels=c("Private Citizens & Property","Government (Diplomatic)","Journalists & Media","Police","Utilities","Military","Government (General)","Airports & Aircraft","Business","Educational Institution","Violent Political Party ","Religious Figures/Institutions","Unknown","Transportation","Tourists","NGO","Telecommunication","Food or Water Supply","Terrorists/Non-State Militia","Other","Maritime", "Abortion Related"))



# Cargar bibliotecas
library(dplyr)
library(ggplot2)

# Verificar la distribución de categorías
tabla_frecuencia <- data %>%
  group_by(targtype1_txt) %>%
  summarise(frecuencia = n())

# Imprimir la tabla de frecuencias
print(tabla_frecuencia)

# Visualizar la distribución con un gráfico de barras
ggplot(data, aes(x = targtype1_txt)) +
  geom_bar() +
  labs(title = "Distribución de la Variable Categórica",
       x = "Categoría",
       y = "Frecuencia")

# Identificar categorías poco frecuentes
umbral_frecuencia <- 5  # Puedes ajustar este umbral según tus necesidades
categorias_poco_frecuentes <- tabla_frecuencia %>%
  filter(frecuencia < umbral_frecuencia) %>%
  pull(targtype1_txt)

# Imprimir categorías poco frecuentes
if (length(categorias_poco_frecuentes) > 0) {
  cat("Categorías poco frecuentes:", paste(categorias_poco_frecuentes, collapse = ", "), "\n")
} else {
  cat("No hay categorías poco frecuentes.\n")
}
#conteo de targsubtype1
tabla_frecuencia <- table(data$targsubtype1)
print(tabla_frecuencia)
#valores perdidos

# Contar valores perdidos
valores_perdidos <- sum(is.na(data$targsubtype1))

# Imprimir la cantidad de valores perdidos
cat("Número de valores perdidos:", valores_perdidos, "\n")


# Calcular la media de la variable
media_targsubtype1 <- ceiling(mean(data$targsubtype1, na.rm = TRUE))

# Imputar la media redondeada hacia arriba
data$targsubtype1 <- ifelse(is.na(data$targsubtype1), media_targsubtype1, data$targsubtype1)


# Frecuencia de valores en targtype1
value_counts <- table(data$targsubtype1)

# boxplot targtype1 para ver si tiene valores atipicos
boxplot(data$targsubtype1, horizontal = TRUE, main = "Boxplot de targtype1")

# Visualización de Frecuencia de targtype1
barplot(value_counts, main = "Frecuencia de valores en targtype1", xlab = "targtype1", ylab = "Frecuencia")

# Rellenar los valores vacíos
data$targsubtype1_txt <- replace(data$targsubtype1_txt, data$targsubtype1_txt == "", "International Organization (peacekeeper, aid agency, compound)")


tabla_frecuencia <- table(data$targsubtype1_txt)
print(tabla_frecuencia)

#tabla de frecuencias para corp1
tabla_frecuencia <- table(data$corp1)
print(tabla_frecuencia)



# Calcular la frecuencia de cada categoría
frecuencia_categorias <- data %>%
  group_by(corp1) %>%
  summarise(frecuencia = n())

# Definir un umbral de frecuencia para categorías poco frecuentes
umbral_frecuencia <- 10  # Puedes ajustar este umbral según tus necesidades

# Identificar las categorías poco frecuentes
categorias_poco_frecuentes <- frecuencia_categorias %>%
  filter(frecuencia < umbral_frecuencia) %>%
  pull(corp1)

# Agrupar las categorías poco frecuentes bajo una etiqueta común
data <- data %>%
  mutate(variable_texto_agrupada = ifelse(corp1 %in% categorias_poco_frecuentes, "Otras", corp1))




# Verificar la distribución de categorías
tabla_frecuencia <- data %>%
  group_by(corp1) %>%
  summarise(frecuencia = n())

# Imprimir la tabla de frecuencias
print(tabla_frecuencia)

# Visualizar la distribución con un gráfico de barras
#ggplot(data, aes(x = corp1)) +
 # geom_bar() +
  #labs(title = "Distribución de la Variable Categórica",
   #    x = "Categoría",
    #   y = "Frecuencia")

# Identificar categorías poco frecuentes
#umbral_frecuencia <- 5  # Puedes ajustar este umbral según tus necesidades
#categorias_poco_frecuentes <- tabla_frecuencia %>%
 # filter(frecuencia < umbral_frecuencia) %>%
  #pull(corp1)

# Imprimir categorías poco frecuentes
#if (length(categorias_poco_frecuentes) > 0) {
 # cat("Categorías poco frecuentes:", paste(categorias_poco_frecuentes, collapse = ", "), "\n")
#} else {
 # cat("No hay categorías poco frecuentes.\n")
#}


#tabla de frecuencias para corp1
tabla_frecuencia <- table(data$target1)
print(tabla_frecuencia)


# Contar los valores perdidos (cadenas vacías)
valores_perdidos_contados <- sum(is.na(data$target1) | data$target1 == "")

# Imprimir el resultado
cat("Número de valores perdidos (cadenas vacías):", valores_perdidos_contados, "\n")


# Calcular la frecuencia de cada categoría
frecuencia_categorias <- data %>%
  group_by(target1) %>%
  summarise(frecuencia = n())

# Definir un umbral de frecuencia para categorías poco frecuentes
umbral_frecuencia <- 10  # Puedes ajustar este umbral según tus necesidades

# Identificar las categorías poco frecuentes
categorias_poco_frecuentes <- frecuencia_categorias %>%
  filter(frecuencia < umbral_frecuencia) %>%
  pull(target1)

# Agrupar las categorías poco frecuentes bajo una etiqueta común
data <- data %>%
  mutate(variable_texto_agrupada = ifelse(target1 %in% categorias_poco_frecuentes, "Otras", target1))

#hacemos una tabla con el conteo de las categorias de natlty1
tabla_frecuencia <- table(data$natlty1)
print(tabla_frecuencia)

table(data$natlty1, useNA = "ifany")

# Calcular la media de la variable cuantitativa
media_natlty1 <- mean(data$natlty1, na.rm = TRUE)

# Imputar los valores perdidos con el techo de la media
data$natlty1 <- ifelse(is.na(data$natlty1), ceiling(media_natlty1), data$natlty1)








# Calcular el rango intercuartílico (IQR) para ver si targtype tiene valore
Q1 <- quantile(data$natlty1, 0.25)
Q3 <- quantile(data$natlty1, 0.75)
IQR <- Q3 - Q1

# Definir límites para identificar valores atípicos
lower_limit <- Q1 - 1.5 * IQR
upper_limit <- Q3 + 1.5 * IQR

# Identificar valores atípicos
outliers <- data$natlty1 < lower_limit | data$natlty1 > upper_limit

# Mostrar valores atípicos
outlier_values <- data$natlty1[outliers]
cat("Valores atípicos en natlty1:", unique(outlier_values), "\n")

# Valores atípicos que deseas eliminar
valores_atipicos <- c(422, 359, 999, 403, 362, 603, 604, 377, 605, 349, 520, 351, 334, 1001, 347, 1003, 1002, 1004)

# Filtrar el dataframe para excluir filas con valores atípicos
data <- data[!data$natlty1 %in% valores_atipicos, ]








# Obtiene los niveles únicos en el orden en que aparecen los datos
unique_levels <- unique(data$natlty1_txt)

# Convierte a factor con niveles manuales
data$natlty1_txt <- factor(data$natlty1_txt, levels = unique_levels)

# Verifica que la columna haya sido convertida a factor con los niveles deseados
str(data$natlty1_txt)

# imprime 
print(data$natlty1_txt)


# Contar los valores perdidos (cadenas vacías)
valores_perdidos_contados <- sum(is.na(data$natlty1_txt) | data$natlty1_txt == "")

# Imprimir el resultado
cat("Número de valores perdidos (cadenas vacías):", valores_perdidos_contados, "\n")







#eliminamos targtype2 pues es imposible imputar datos

data$targtype2 <- NULL



#eliminamos targtype2_txt pues es imposible imputar datos

data$targtype2_txt <- NULL



#eliminamos targsubtype2 pues es imposible imputar datos

data$targsubtype2 <- NULL

#eliminamos targsubtype2_txt pues es imposible imputar datos

data$targsubtype2_txt <- NULL


#eliminamos corp2 pues es imposible imputar datos

data$corp2 <- NULL

#eliminamos target2 pues es imposible imputar datos

data$target2 <- NULL

#eliminamos natlty2 pues es imposible imputar datos

data$natlty2 <- NULL

#eliminamos natlty2_txt pues es imposible imputar datos

data$natlty2_txt <- NULL


#eliminamos targtype3 pues es imposible imputar datos

data$targtype3 <- NULL



#eliminamos targtype3_txt pues es imposible imputar datos

data$targtype3_txt <- NULL



#eliminamos targsubtype3 pues es imposible imputar datos

data$targsubtype3 <- NULL

#eliminamos targsubtype3_txt pues es imposible imputar datos

data$targsubtype3_txt <- NULL

#eliminamos corp3 pues es imposible imputar datos

data$corp3 <- NULL

#eliminamos target3 pues es imposible imputar datos

data$target3 <- NULL

#eliminamos natlty3 pues es imposible imputar datos

data$natlty3 <- NULL

#eliminamos natlty2_txt pues es imposible imputar datos

data$natlty3_txt <- NULL

# Contar los valores perdidos (cadenas vacías)
valores_perdidos_contados <- sum(is.na(data$gname) | data$gname == "")

# Imprimir el resultado
cat("Número de valores perdidos (cadenas vacías):", valores_perdidos_contados, "\n")

# Obtiene los niveles únicos en el orden en que aparecen los datos
unique_levels <- unique(data$gname)

# Convierte a factor con niveles manuales
data$gname <- factor(data$gname, levels = unique_levels)

# Verifica que la columna haya sido convertida a factor con los niveles deseados
str(data$gname)


#eliminamos gsubname pues es imposible imputar datos

data$gsubname <- NULL

#eliminamos gname2 pues es imposible imputar datos

data$gname2 <- NULL

#eliminamos gsubname2 pues es imposible imputar datos

data$gsubname2 <- NULL

#eliminamos gname3 pues es imposible imputar datos

data$gname3 <- NULL

#eliminamos gsubname3 pues es imposible imputar datos

data$gsubname3 <- NULL

#podemos guardar los motivos de algunos grupos antes de borrar la columna con 
# write.csv(data$motive, file = "ruta/del/archivo.csv", row.names = FALSE)

#eliminamos motive pues es imposible imputar datos

data$motive <- NULL

# checamos si guncertain1 tiene valores perdidos
valores_perdidos <- sum(is.na(data$guncertain1))

cat("Número de valores perdidos en guncertain1:", valores_perdidos, "\n")



# imputacion de guncertain con el techo de la media
media_guncertain1 <- mean(data$guncertain1, na.rm = TRUE)
techo_media <- ceiling(media_guncertain1)

# Imputar valores faltantes con el techo de la media
data$guncertain1 <- ifelse(is.na(data$guncertain1), techo_media, data$guncertain1)


#eliminamos guncertain2 pues es imposible imputar datos

data$guncertain2 <- NULL


#eliminamos guncertain3 pues es imposible imputar datos

data$guncertain3 <- NULL
summary(data)




# Obtiene los niveles únicos en el orden en que aparecen los datos
unique_levels <- unique(data$corp1)

# Convierte a factor con niveles manuales
data$corp1 <- factor(data$corp1 , levels = unique_levels)

# Verifica que la columna haya sido convertida a factor con los niveles deseados
str(data$corp1)

# Suponiendo que tu dataframe se llama "data"
data <- data %>%
  mutate(corp1 = ifelse(corp1 == 1, 924, corp1))

# Suponiendo que tu dataframe se llama "data"
data$corp1 <- ifelse(is.na(data$corp1), 936, data$corp1)


# Obtiene los niveles únicos en el orden en que aparecen los datos
unique_levels <- unique(data$corp1)

# Convierte a factor con niveles manuales
data$corp1 <- factor(data$corp1 , levels = unique_levels)

# Verifica que la columna haya sido convertida a factor con los niveles deseados
str(data$corp1)



# Obtiene los niveles únicos en el orden en que aparecen los datos
unique_levels <- unique(data$targsubtype1_txt)

# Convierte a factor con niveles manuales
data$targsubtype1_txt <- factor(data$targsubtype1_txt , levels = unique_levels)

# Verifica que la columna haya sido convertida a factor con los niveles deseados
str(data$targsubtype1_txt)


# cambiamos las cadenas vacias a la categoria Unnamed Civilian/Unspecified
data <- data %>%
  mutate(targsubtype1_txt = ifelse(targsubtype1_txt == 13, 45, targsubtype1_txt))
#regresamos a facotor targetsubtype1_txt
# Obtiene los niveles únicos en el orden en que aparecen los datos
unique_levels <- unique(data$targsubtype1_txt)

# Convierte a factor con niveles manuales
data$targsubtype1_txt <- factor(data$targsubtype1_txt , levels = unique_levels)

# Verifica que la columna haya sido convertida a factor con los niveles deseados
str(data$targsubtype1_txt)






# Obtiene los niveles únicos en el orden en que aparecen los datos
unique_levels <- unique(data$target1)

# Convierte a factor con niveles manuales
data$target1 <- factor(data$target1 , levels = unique_levels)

# Verifica que la columna haya sido convertida a factor con los niveles deseados
str(data$target1)



# Suponiendo que tu dataframe se llama "data"
data <- data %>%
  mutate(target1 = ifelse(is.na(target1), "Civilians", target1))


# Obtiene los niveles únicos en el orden en que aparecen los datos
unique_levels <- unique(data$target1)

# Convierte a factor con niveles manuales
data$target1 <- factor(data$target1 , levels = unique_levels)

# Verifica que la columna haya sido convertida a factor con los niveles deseados
str(data$target1)


summary(data)

