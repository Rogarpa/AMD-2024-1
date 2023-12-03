install.packages("caret")

library(dplyr)
library(caret)

data = read.csv(file = "RUTA/globalterrorismdb_0718dist.csv", sep=",", header = T, stringsAsFactors = F)

# Seleccionar atributos relevantes
selected_data <- data %>% select(individual, nkill, nwound, weaptype1_txt, attacktype1_txt, targtype1_txt, gname, country_txt)

# Manejo de valores perdidos
# Para numéricos (nkill, nwound), usar la mediana para la imputacion
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

# Eliminacion de valores atípicos
bounds <- calculate_bounds(selected_data$individual)
selected_data <- selected_data %>% filter(individual >= bounds$lower & individual <= bounds$upper)

# Discretización nkill
selected_data$nkill_discretizado <- cut(selected_data$nkill, breaks=c(0, 10, 50, max(selected_data$nkill, na.rm = TRUE)), labels=c("Bajo", "Medio", "Alto"))

# Normalización de variables numéricas (nkill, nwound)
preproc <- preProcess(selected_data %>% select(nkill, nwound), method = c("center", "scale"))
normalized_data <- predict(preproc, selected_data %>% select(nkill, nwound))

# Combinar datos normalizados con datos no numéricos
final_data <- bind_cols(selected_data %>% select(-nkill, -nwound), normalized_data)

# Guardar los datos procesados
write.csv(final_data, "datos_procesados.csv", row.names = FALSE)
