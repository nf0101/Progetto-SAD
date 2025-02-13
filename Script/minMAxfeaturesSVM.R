library(dplyr)
library(ggplot2)
library(e1071)

# Seleziona le feature numeriche (modifica i nomi delle colonne secondo le tue necessità)
numeric_features <- combined_dataset %>%
  select(URLLength, NoOfSubDomain, CharContinuationRate, URLCharProb, NoOfOtherSpecialCharsInURL, 
         LineOfCode, NoOfImage, NoOfSelfRef, LineOfCode, NoOfJS)

# Calcola i minimi e i massimi per ciascuna feature numerica
mins <- sapply(numeric_features, min)
maxs <- sapply(numeric_features, max)

# Visualizza i valori minimi e massimi (opzionale)
print("Minimi per ogni feature:")
print(mins)
print("Massimi per ogni feature:")
print(maxs)

# Normalizza le feature numeriche usando la formula:
# x_norm = (x - min) / (max - min)
normalized_numeric_features <- numeric_features %>%
  mutate(across(everything(), ~ (. - min(.)) / (max(.) - min(.))))

# Seleziona le feature binarie (assicurati che i nomi siano corretti)
binary_features <- combined_dataset %>%
  select(label, HasCopyrightInfo, IsResponsive, HasDescription, HasSocialNet, IsHTTPS, HasTitle)

# Combina i dati normalizzati e le feature binarie
final_data <- cbind(normalized_numeric_features, binary_features)

# Controlla la struttura del dataset finale
str(final_data)
