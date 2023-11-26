data <- read.csv(file = "/Users/LAB/code/R/proyecto/src/globalterrorismdb_0718dist.csv", sep = ",", header = TRUE, stringsAsFactors = FALSE)
selected_data <- data[, 1:34]

# Función para obtener el tipo de atributo
get_attribute_type <- function(column) {
  if (is.factor(column)) {
    return("Cualitativo")
  } else if (is.numeric(column)) {
    return("Cuantitativo")
  } else {
    return("Cualitativo")
  }
}

# Seleccionar las primeras 34 columnas
selected_data <- data[, 1:34]

# Crear un dataframe para almacenar la información
info_table <- data.frame(
  Columna = names(selected_data),
  Tipo = sapply(selected_data, get_attribute_type),
  Subtipo = sapply(selected_data, function(x) {
    if (is.numeric(x)) {
      if (all(!is.na(x) & x %% 1 == 0))  {
        return("Discreto")
      } else {
        return("Continuo")
      }
    } else if (is.character(x)) {
      return("Nominal")
    } else {
      return("Otro")
    }
  }),
  Porcentaje_Perdidos = sapply(selected_data, function(x) {
    if (length(x) > 0) {
      # Convertir cadenas vacías "" a NA
      x[x == ""] <- NA
      return(sprintf("%.2f", mean(is.na(x)) * 100))
    } else {
      return("0.00")
    }
  }),
  Valores_Permitidos = sapply(selected_data, function(x) {
    if (is.numeric(x)) {
      if (all(!is.na(x) & x %% 1 == 0))  {
        return("Enteros")
      } else {
        return("Flotantes")
      }
    } else if (is.character(x)) {
      return("Cadenas")
    } else {
      return("Otro")
    }
  }),
  Valor_Min = sapply(selected_data, function(x) ifelse(length(na.omit(x)) > 0, min(x, na.rm = TRUE), NA)),
  Valor_Max = sapply(selected_data, function(x) ifelse(length(na.omit(x)) > 0, max(x, na.rm = TRUE), NA)),
  Media = sapply(selected_data, function(x) ifelse(length(na.omit(x)) > 0, mean(x, na.rm = TRUE), NA)),
  Desviacion_Estandar = sapply(selected_data, function(x) ifelse(length(na.omit(x)) > 0, sd(x, na.rm = TRUE), NA))
)

info_table$Distribucion <- rep(NA, nrow(info_table))
info_table$NivesYFrecuencia <- rep(NA, nrow(info_table))
info_table$Valores_Atipicos <- rep(NA, nrow(info_table))

#Funcion para encontrar valores atípicos
has_outliers_iqr <- function(vector) {
  q <- quantile(vector, c(0.25, 0.75), na.rm = TRUE)
  iqr <- IQR(vector, na.rm = TRUE)
  lower_threshold <- q[1] - 1.5 * iqr
  upper_threshold <- q[2] + 1.5 * iqr
  any(vector < lower_threshold | vector > upper_threshold)
}

for (i in 1:nrow(info_table)) {
  # Verificar si la columna es numérica
  if (info_table$Tipo[i] == "Numérico") {
    # Agregar valores atípicos usando has_outliers_iqr
    info_table$Valores_Atipicos[i] <- has_outliers_iqr(selected_data[[info_table$Columna[i]]])
  }
}
