library(dplyr)

# Definisci le feature di interesse
selected_features <- c("URLLength", "NoOfSubDomain", "CharContinuationRate", 
                       "URLCharProb", "NoOfOtherSpecialCharsInURL", "LineOfCode", 
                       "NoOfImage", "NoOfSelfRef", "NoOfJS")

# Calcola le statistiche per le entry misclassificate
misclassified_summary <- original_misclassified %>% 
  select(all_of(selected_features)) %>% 
  summarise(across(everything(), list(
    mean   = mean,
    median = median,
    min    = min,
    max    = max,
    sd     = sd
  ), na.rm = TRUE))

print("Statistiche per le entry misclassificate:")
print(misclassified_summary)

# Filtra dal dataset originale tutti gli URL legittimi (ad es. label == 1)
legitimate_data <- combined_dataset %>% 
  filter(label == 1)  # Assicurati che questo filtro corrisponda al modo in cui sono codificate le etichette

# Calcola le statistiche per gli URL legittimi
legitimate_summary <- legitimate_data %>% 
  select(all_of(selected_features)) %>% 
  summarise(across(everything(), list(
    mean   = mean,
    median = median,
    min    = min,
    max    = max,
    sd     = sd
  ), na.rm = TRUE))

print("Statistiche medie per tutti gli URL legittimi in combined_dataset:")
print(legitimate_summary)

phishinginfo_data <- combined_dataset %>% 
  filter(label == 0)  # Assicurati che questo filtro corrisponda al modo in cui sono codificate le etichette

# Calcola le statistiche per gli URL legittimi
phishing_summary <- phishinginfo_data %>% 
  select(all_of(selected_features)) %>% 
  summarise(across(everything(), list(
    mean   = mean,
    median = median,
    min    = min,
    max    = max,
    sd     = sd
  ), na.rm = TRUE))

print("Statistiche medie per tutti gli URL legittimi in combined_dataset:")
print(phishing_summary)
