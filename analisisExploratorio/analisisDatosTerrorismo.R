data = read.csv(file = "C:/AMD/globalterrorismdb_0718dist.csv", sep=",", header = T, stringsAsFactors = F,encoding = "UTF-8")
 
library(dplyr)
select(data)

colum <- select(data, targtype1,targtype1_txt,targsubtype1,targsubtype1_txt,corp1,target1,natlty1,natlty1_txt,targtype2,targtype2_txt,targsubtype2,targsubtype2_txt,corp2,target2,natlty2,natlty2_txt,targtype3,targtype3_txt,targsubtype3,targsubtype3_txt,corp3,target3,natlty3,natlty3_txt,gname,gsubname,gname2,gsubname2,gname3,gsubname3,motive,guncertain1,guncertain2,guncertain3)
summary(colum)
library(skimr)
skim(colum)
#para campos que tienen caracteres invalidos usamos esta linea y podemos hacer  uso de summary y skim
data$motive <- iconv(data$motive, "UTF-8", "ASCII", sub = "")
summary(data$guncertain2)


skim(data$guncertain3)

# para hacer histogramas de las columnas y analizar el tipo de frecuencia
hist(data$guncertain1, main = "Histograma", xlab = "Valores", ylab = "Frecuencia")
summary(data$natlty1)
#para hacer un conteo de cada variable y ver si tiene NA o cadena vacia ademas de las categorias
tabla_frecuencia <- table(data$gname2)
print(tabla_frecuencia)


#procedimiento para determinar si las variables cuantitativas tienen valores atipicas
variable <- data$guncertain3

# Remover valores faltantes antes de calcular cuantiles
cuantiles <- quantile(variable, c(0.25, 0.75), na.rm = TRUE)
Q1 <- cuantiles[1]
Q3 <- cuantiles[2]

# Calcular el rango intercuartílico (IQR)
IQR <- Q3 - Q1

# Calcular límites para identificar valores atípicos
limite_inferior <- Q1 - 1.5 * IQR
limite_superior <- Q3 + 1.5 * IQR

# Identificar valores atípicos
valores_atipicos <- variable < limite_inferior | variable > limite_superior

# Imprimir los resultados
cat("Límite inferior:", limite_inferior, "\n")
cat("Límite superior:", limite_superior, "\n")
cat("Número de valores atípicos:", sum(valores_atipicos, na.rm = TRUE), "\n")





# Suponiendo que tu variable cualitativa es "categoria"
barplot(table(data$targtype2_txt), main = "Distribución de la Variable Cualitativa", xlab = "Categorías", ylab = "Frecuencia")


# valores perdidos de una variable cualitativa nominal
data %>%
  summarize(porcentaje_valores_perdidos = sum(is.na(corp1)) / n())

data$targtype2_txt <- na.omit(data$targtype2_txt)

# Verificar la cantidad de filas después de eliminar NA
nrow(data$targtype2_txt)


porcentaje_valores_perdidos <- sum(is.na(data$targtype2_txt)) / nrow(data$targtype2_txt)
cat("Porcentaje de valores perdidos:", porcentaje_valores_perdidos, "\n")



skim(data$targtype2_txt)


