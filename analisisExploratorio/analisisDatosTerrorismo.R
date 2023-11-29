data = read.csv(file = "C:\\Users\\charl\\Music\\globalterrorismdb_0718dist.csv", sep=",", header = T, stringsAsFactors = F)

# Funcion para guaradar la salida en un txt
write_to_file <- function(file_path, text) {
  cat(text, file = file_path, append = TRUE)
}

# Funcion para hacer analisis exploratorio
analisis_exploratorio <- function(numero_columna, data, output_file_path) {
  write_to_file(output_file_path, paste("Columna no: ", numero_columna, "\n"))
  datos <- data[, numero_columna]
  datos_sin_na <- subset(data, !is.na(data[, numero_columna]))
  columna_sin_na <- datos_sin_na[, numero_columna]
  
  # Tipo de atributo
  write_to_file(output_file_path, paste("El tipo de atributo es: ", class(columna_sin_na), "\n"))
  
  # Verifica si hay valores perdidos
  hay_valores_perdidos <- sum(is.na(datos))
  
  if (hay_valores_perdidos > 0) {
    write_to_file(output_file_path, paste("Hay", hay_valores_perdidos, "valores perdidos en la columna.\n"))
  } else {
    write_to_file(output_file_path, paste("No hay valores perdidos en la columna.\n"))
  }
    
  # Porcentaje de valores perdidos
  porcentaje_na <- sprintf("%.2f%%", sum(is.na(datos)) / length(datos) * 100)
  write_to_file(output_file_path, paste("Porcentaje de valores perdidos: ", porcentaje_na, "\n"))
  
  if (is.numeric(columna_sin_na)) {
    # Valor minimo
    minimo <- min(columna_sin_na)
    write_to_file(output_file_path, paste("Valor minimo: ", minimo, "\n"))
    
    # Valor maximo
    maximo <- max(columna_sin_na)
    write_to_file(output_file_path, paste("Valor maximo: ", maximo, "\n"))
    
    # Media
    media <- mean(columna_sin_na)
    write_to_file(output_file_path, paste("Media: ", media, "\n"))
    
    # Desviacion estandar
    desviacion_estandar <- sd(columna_sin_na)
    write_to_file(output_file_path, paste("Desviacion Estandar: ", desviacion_estandar, "\n"))  
    
    # Coeficiente de asimetría (skewness)
    skewness <- sum((columna_sin_na - media)^3) / ((length(columna_sin_na) - 1) * desviacion_estandar^3)
    
    # Coeficiente de curtosis
    kurtosis <- sum((columna_sin_na - media)^4) / ((length(columna_sin_na) - 1) * desviacion_estandar^4) - 3
    
    # Identificación de la distribución
    if (abs(skewness) < 0.5 && abs(kurtosis - 3) < 0.5) {
      write_to_file(output_file_path, paste("Distribucion Normal.\n"))
    } else if (skewness > 0 && kurtosis > 3) {
      write_to_file(output_file_path, paste("Distribucion Leptocurtica.\n"))
    } else if (skewness < 0 && kurtosis < 3) {
      write_to_file(output_file_path, paste("Distribucion Platicurtica.\n"))
    } else {
      write_to_file(output_file_path, paste("DNC.\n"))
    }
    
    # IQR y límites para valores atípicos
    iqr <- IQR(columna_sin_na, na.rm = TRUE)
    limite_superior <- quantile(columna_sin_na, 0.75, na.rm = TRUE) + 1.5 * iqr
    limite_inferior <- quantile(columna_sin_na, 0.25, na.rm = TRUE) - 1.5 * iqr
    
    # Valores atipicos
    valores_atipicos <- columna_sin_na[columna_sin_na > limite_superior | columna_sin_na < limite_inferior]
    valores_atipicos <- unique(valores_atipicos)
    write_to_file(output_file_path, paste("Valores atipicos:\n"))
    write.table(valores_atipicos, file = output_file_path, append = TRUE, col.names = FALSE, row.names = FALSE)
  
  } else {
    # Niveles y Frecuencia
    niveles <- levels(as.factor(columna_sin_na))
    frecuencia <- table(columna_sin_na)
    
    # Crear un data.frame con niveles y frecuencias
    df_niveles_frecuencia <- data.frame(Niveles = niveles, Frecuencia = as.vector(frecuencia))
    
    # Imprimir y escribir en el archivo
    write_to_file(output_file_path, cat("Niveles de la variable categórica y su frecuencia:\n"))
    write.table(df_niveles_frecuencia, file = output_file_path, append = TRUE, row.names = FALSE, col.names = FALSE)
  }
  
  write_to_file(output_file_path, paste("############################################################\n"))
}

filepath = "info.txt"

# Aplicamos desde la columna 69 al 102
for (i in 69:102) {
  analisis_exploratorio(i, data, filepath)
}