# Carica le librerie necessarie
library(dplyr)

# Specifica la directory contenente i file CSV
setwd("C:\\Users\\frugi\\Desktop\\Uni\\Magistrale\\SAD\\Phishing_URL_Dataset\\")

# Ottieni la lista di file CSV
file_list <- list.files(pattern = "*.csv")

# Carica i file CSV e controlla se sono dataframe
dataset_list <- lapply(file_list, function(file) {
  data <- read.csv(file, sep = ";")
  
  # Forza la colonna URLSimilarityIndex a essere numerica (gestisce eventuali valori non numerici)
  data$URLSimilarityIndex <- suppressWarnings(as.numeric(data$URLSimilarityIndex))
  
  # Forza la colonna DomainTitleMatchScore a essere numerica (gestisce eventuali valori non numerici)
  data$DomainTitleMatchScore <- suppressWarnings(as.numeric(data$DomainTitleMatchScore))
  
  # Verifica se il dataset è un dataframe valido
  if (!is.data.frame(data)) {
    stop(paste("Il file", file, "non è un dataframe valido."))
  }
  
  return(data)
})

# Unisci i dataframe nella lista
combined_dataset <- bind_rows(dataset_list)

# Verifica la struttura del dataset combinato
str(combined_dataset)
