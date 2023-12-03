install.packages("caret")



library(dplyr)
library(caret)

data <- read.csv("/home/carlows/Descargas/globalterrorismdb_0718dist.csv")

# Seleccionar atributos relevantes
selected_data <- data %>% select(individual, nkill, nwound, weaptype1_txt, attacktype1_txt, targtype1_txt, gname, country_txt)

# Manejo de valores perdidos
# Para numericos (nkill, nwound), usar la mediana para la imputacion
selected_data <- selected_data %>% mutate(nkill = ifelse(is.na(nkill), median(nkill, na.rm = TRUE), nkill))
selected_data <- selected_data %>% mutate(nwound = ifelse(is.na(nwound), median(nwound, na.rm = TRUE), nwound))

# Para categoricos (weaptype1_txt, attacktype1_txt, targtype1_txt, gname, country_txt), usar la moda
impute_mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

selected_data$weaptype1_txt <- ifelse(is.na(selected_data$weaptype1_txt), impute_mode(selected_data$weaptype1_txt), selected_data$weaptype1_txt)
selected_data$attacktype1_txt <- ifelse(is.na(selected_data$attacktype1_txt), impute_mode(selected_data$attacktype1_txt), selected_data$attacktype1_txt)
selected_data$targtype1_txt <- ifelse(is.na(selected_data$targtype1_txt), impute_mode(selected_data$targtype1_txt), selected_data$targtype1_txt)
selected_data$gname <- ifelse(is.na(selected_data$gname), impute_mode(selected_data$gname), selected_data$gname)
selected_data$country_txt <- ifelse(is.na(selected_data$country_txt), impute_mode(selected_data$country_txt), selected_data$country_txt)

# Definir una funcion para calcular los limites para identificar valores atipicos
calculate_bounds <- function(x) {
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  return(c(lower = lower_bound, upper = upper_bound))
}

# Eliminacion de valores atipicos
# Definir función para reemplazar valores atípicos con la mediana
replace_outliers <- function(x) {
  bounds <- calculate_bounds(x)
  x[x < bounds["lower"] | x > bounds["upper"]] <- median(x, na.rm = TRUE)
  return(x)
}

# Aplicamos la eliminacion de valores atipicos por la media
selected_data$nkill <- replace_outliers(selected_data$nkill)
selected_data$nwound <- replace_outliers(selected_data$nwound)
selected_data$individual <- replace_outliers(selected_data$individual)

# Discretizacion nkill
selected_data$nkill_discretizado <- cut(selected_data$nkill, breaks=c(0, 10, 50, max(selected_data$nkill, na.rm = TRUE)), labels=c("Bajo", "Medio", "Alto"))

# Normalizacion de variables numericas (nkill, nwound)
preproc <- preProcess(selected_data %>% select(nkill, nwound), method = c("center", "scale"))
normalized_data <- predict(preproc, selected_data %>% select(nkill, nwound))

# Combinar datos normalizados con datos no numericos
final_data <- bind_cols(selected_data %>% select(-nkill, -nwound), normalized_data)

# Guardar los datos procesados
write.csv(final_data, "datos_procesados.csv", row.names = FALSE)
