data = read.csv(file = "E:/Descargas/Repos/AMD-2024-1/globalterrorismdb_0718dist.csv", sep=",", header = T, stringsAsFactors = F)
library(dplyr)

atipicosIQR <- function(data_column){
  data_column
  iqr <- IQR(data_column, na.rm = TRUE)
  
  limite_superior <- quantile(data_column, 0.75, na.rm = TRUE) + 1.5 * iqr
  limite_inferior <- quantile(data_column, 0.25, na.rm = TRUE) - 1.5 * iqr
  
  valores_atipicos <- data_column[data_column > limite_superior | data_column < limite_inferior]
  #valores_atipicos <- unique(valores_atipicos)
  valores_atipicos <- valores_atipicos[!is.na(valores_atipicos)]
  return (valores_atipicos)
  
}

#Eliminación de columnas y valores atípicos ====================================================================================================================
#column nwoundus
data <- subset(data, select = -c(nwoundus))
#column nwoundte
data <- subset(data, select = -c(nwoundte))
#column propextent
data <- subset(data, select = -c(propextent))
#column propextent_txt
data <- subset(data, select = -c(propextent_txt))
#column propvalue
data <- data %>%
          mutate(propvalue = ifelse((property == 0), 0, propvalue))%>%
          mutate(propvalue = ifelse((property == -9), NA, propvalue))%>%
          mutate(propvalue = ifelse((propvalue == -99), NA, propvalue))
data <- data[!(data$nhostkid %in% atipicosIQR(data$nhostkid)),]
#column property
data <- subset(data, select = -c(property))
#column propcomment
data <- subset(data, select = -c(propcomment))
#column nhostkid
data <- data %>%
          mutate(nhostkid = ifelse((ishostkid == 0), 0, nhostkid))%>%
          mutate(nhostkid = ifelse((ishostkid == -9), NA, nhostkid))%>%
          mutate(nhostkid = ifelse((nhostkid == -99), NA, nhostkid))
data <- data[!(data$nhostkid %in% atipicosIQR(data$nhostkid)),]
#column ishostkid
data <- subset(data, select = -c(ishostkid))
#column nhostkidus
data <- subset(data, select = -c(nhostkidus))
#column nhours
data <- mutate(data, nhours = (nhours+(24*ndays)))
data <- data[!(data$nhours %in% atipicosIQR(data$nhours)),]
#column ndays
data <- subset(data, select = -c(ndays))
#column divert
data$divert <- as.factor(data$divert)
#column kidhijcountry
data$kidhijcountry <- as.factor(data$kidhijcountry)
#column ransomamt
data <- data %>%
            mutate(ransomamt = ifelse((ransom == 0), 0, ransomamt))%>%
            mutate(ransomamt = ifelse((ransom == -9), NA, ransomamt))%>%
            mutate(ransomamt = ifelse((ransomamt == -99), NA, ransomamt))
#column ransomamtus
data <- subset(data, select = -c(ransomamtus))
#column ransompaid
data <- data %>%
  mutate(ransompaid = ifelse((ransom == 0), 0, ransompaid))%>%
  mutate(ransompaid = ifelse((ransom == -9), NA, ransompaid))%>%
  mutate(ransompaid = ifelse((ransom == -99), NA, ransompaid))

data <- data[!(data$ransompaid %in% atipicosIQR(data$ransompaid)),]
#column ransompaidus
data <- subset(data, select = -c(ransompaidus))
#column ransomnote
data <- subset(data, select = -c(ransomnote))
#column ransom
data <- subset(data, select = -c(ransom))
#column hostkidoutcome
data <- subset(data, select = -c(hostkidoutcome))
#column hostkidoutcome_txt
data$hostkidoutcome_txt <- as.factor(data$hostkidoutcome_txt)
#column nreleased
data <- data[!(data$nreleased %in% atipicosIQR(data$nreleased)),]
#column addnotes
data <- subset(data, select = -c(addnotes))
#column scite1
data <- subset(data, select = -c(scite1))
#column scite2
data <- subset(data, select = -c(scite2))
#column scite3
data <- subset(data, select = -c(scite3))
#column dbsource
data <- subset(data, select = -c(dbsource))
#column INT_LOG
data <- data[!(data$INT_LOG %in% atipicosIQR(data$INT_LOG)),]
#column INT_IDEO
data <- data[!(data$INT_IDEO %in% atipicosIQR(data$INT_IDEO)),]
#column INT_MISC
data <- data[!(data$INT_MISC %in% atipicosIQR(data$INT_MISC)),]
#column INT_ANY
data <- data[!(data$INT_ANY %in% atipicosIQR(data$INT_ANY)),]
#column related
data <- subset(data, select = -c(related))

#Imputación de columnas==================================================================================================================
data$propvalue[is.na(data$propvalue)] <- mean(data$propvalue, na.rm = TRUE)
data$nhostkid[is.na(data$nhostkid)] <- mean(data$nhostkid, na.rm = TRUE)
data$nhours[is.na(data$nhours)] <- mean(data$nhours, na.rm = TRUE)
data$ransomamt[is.na(data$ransomamt)] <- mean(data$ransomamt, na.rm = TRUE)
data$ransompaid[is.na(data$ransompaid)] <- mean(data$ransompaid, na.rm = TRUE)
data$nreleased[is.na(data$nreleased)] <- mean(data$nreleased, na.rm = TRUE)
data$INT_LOG[is.na(data$INT_LOG)] <- mean(data$INT_LOG, na.rm = TRUE)
data$INT_IDEO[is.na(data$INT_IDEO)] <- mean(data$INT_IDEO, na.rm = TRUE)
data$INT_MISC[is.na(data$INT_MISC)] <- mean(data$INT_MISC, na.rm = TRUE)
data$INT_ANY[is.na(data$INT_ANY)] <- mean(data$INT_ANY, na.rm = TRUE)

#Discretización de columnas

#Normalización de columnas
data$ransomamt <- scale(data$ransomamt)