# Carica le librerie necessarie
library(dplyr)

# Calcolo delle statistiche per gli URL offuscati
obfuscated_stats <- combined_dataset %>%
  filter(HasObfuscation == 1) %>% # Filtra solo gli URL offuscati
  summarise(
    Mean = mean(NoOfObfuscatedChar, na.rm = TRUE),       # Media
    Median = median(NoOfObfuscatedChar, na.rm = TRUE),   # Mediana
    Variance = var(NoOfObfuscatedChar, na.rm = TRUE),    # Varianza
    StdDev = sd(NoOfObfuscatedChar, na.rm = TRUE),       # Deviazione standard
    Max = max(NoOfObfuscatedChar, na.rm = TRUE),         # Massimo
    Min = min(NoOfObfuscatedChar, na.rm = TRUE)          # Minimo
  )

# Funzione per calcolare la moda
calculate_mode <- function(x) {
  unique_x <- unique(x)
  unique_x[which.max(tabulate(match(x, unique_x)))]
}

# Calcola la moda separatamente
mode_value <- calculate_mode(
  combined_dataset %>%
    filter(HasObfuscation == 1) %>%
    pull(NoOfObfuscatedChar)
)

# Aggiungi la moda al risultato
obfuscated_stats$Mode <- mode_value

# Mostra i risultati
print(obfuscated_stats)
###############################################################
# Funzione per calcolare la moda
get_mode <- function(x) {
  uniq_x <- unique(x)
  uniq_x[which.max(tabulate(match(x, uniq_x)))]
}

# Filtra i dati per includere solo gli URL con offuscamento (HasObfuscation == 1)
obfuscated_data <- combined_dataset %>%
  filter(HasObfuscation == 1)

# Calcolare le statistiche per ObfuscationRatio solo per gli URL offuscati
obfuscation_stats <- obfuscated_data %>%
  summarise(
    Mean = mean(ObfuscationRatio, na.rm = TRUE),
    Median = median(ObfuscationRatio, na.rm = TRUE),
    Mode = get_mode(ObfuscationRatio),
    Variance = var(ObfuscationRatio, na.rm = TRUE),
    StdDev = sd(ObfuscationRatio, na.rm = TRUE),
    Min = min(ObfuscationRatio, na.rm = TRUE),
    Max = max(ObfuscationRatio, na.rm = TRUE)
  )

# Visualizzare il risultato
print(obfuscation_stats)

