#install.packages("caret")

library(dplyr)
library(caret)

# Cargar datos
data <- read.csv("/home/carlows/Descargas/globalterrorismdb_0718dist.csv")

# Seleccionar atributos relevantes
selected_data <- data %>% select(individual, nkill, nwound, weaptype1_txt, attacktype1_txt, targtype1_txt, gname, country_txt)

# Definir limites de los valores
calculate_bounds <- function(x) {
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  return(c(lower = lower_bound, upper = upper_bound))
}

# Definir funcion para eliminar o limitar valores atipicos
limit_outliers <- function(x) {
  bounds <- calculate_bounds(x)
  x[x < bounds["lower"]] <- bounds["lower"]
  x[x > bounds["upper"]] <- bounds["upper"]
  return(x)
}

# Aplicar funcion de limites de valores atipicos
selected_data$nkill <- limit_outliers(selected_data$nkill)
selected_data$nwound <- limit_outliers(selected_data$nwound)
selected_data$individual <- limit_outliers(selected_data$individual)

# Imputacion de valores perdidos para variables numericas
selected_data <- selected_data %>% mutate(
  nkill = ifelse(is.na(nkill), median(nkill, na.rm = TRUE), nkill),
  nwound = ifelse(is.na(nwound), median(nwound, na.rm = TRUE), nwound)
)

# Imputacion para variables categoricas
impute_mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

selected_data <- selected_data %>% mutate(
  weaptype1_txt = ifelse(is.na(weaptype1_txt), impute_mode(weaptype1_txt), weaptype1_txt),
  attacktype1_txt = ifelse(is.na(attacktype1_txt), impute_mode(attacktype1_txt), attacktype1_txt),
  targtype1_txt = ifelse(is.na(targtype1_txt), impute_mode(targtype1_txt), targtype1_txt),
  gname = ifelse(is.na(gname), impute_mode(gname), gname),
  country_txt = ifelse(is.na(country_txt), impute_mode(country_txt), country_txt)
)

# Normalizacion de variables numericas (nkill, nwound)
numeric_data <- selected_data %>% select(nkill, nwound)
preproc <- preProcess(numeric_data, method = c("center", "scale"))
normalized_data <- predict(preproc, numeric_data)

# Combinar datos normalizados con datos no numericos
selected_data <- bind_cols(selected_data %>% select(-nkill, -nwound), normalized_data)

# Discretizacion de nkill (aplicada despues de la normalizacion)
selected_data$nkill_discretizado <- cut(selected_data$nkill, breaks=c(-Inf, 0, 10, 50, Inf), labels=c("Muy bajo", "Bajo", "Medio", "Alto"))

# Cambio de Character a factor
selected_data[sapply(selected_data, is.character)] <- lapply(selected_data[sapply(selected_data, is.character)], factor)

# Guardar los datos procesados
data$nkill <- selected_data$nkill
data$nwound <- selected_data$nwound
data$individual <- selected_data$individual
data$weaptype1_txt <- selected_data$weaptype1_txt 
data$attacktype1_txt <- selected_data$attacktype1_txt
data$targtype1_txt <- selected_data$ targtype1_txt
data$gname <- selected_data$gname
data$country_txt <- selected_data$country_txt

write.csv(data, "archivo.csv", row.names = FALSE)
