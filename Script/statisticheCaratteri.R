# Lista delle colonne da analizzare
columns_to_analyze <- c(
  "NoOfLettersInURL", "LetterRatioInURL", "NoOfDegitsInURL", 
  "DegitRatioInURL", "NoOfEqualsInURL", "NoOfQMarkInURL", 
  "NoOfAmpersandInURL", "NoOfOtherSpecialCharsInURL", 
  "SpacialCharRatioInURL"
)

# Creazione di un dataframe lungo per facilitare il confronto
data_long <- combined_dataset %>%
  select(label, all_of(columns_to_analyze)) %>%
  pivot_longer(
    cols = -label,
    names_to = "Variabile",
    values_to = "Valore"
  )

# Aggiunta di etichette leggibili per phishing e legittimi
data_long$Tipo <- ifelse(data_long$label == 0, "Phishing", "Legittimo")


calculate_stats <- function(data, column) {
  data %>%
    summarise(
      Media = mean(.data[[column]], na.rm = TRUE),
      Mediana = median(.data[[column]], na.rm = TRUE),
      Moda = as.numeric(names(sort(table(.data[[column]]), decreasing = TRUE)[1])),
      Varianza = var(.data[[column]], na.rm = TRUE),
      Deviazione_Standard = sd(.data[[column]], na.rm = TRUE),
      Minimo = min(.data[[column]]),
      Massimo = max(.data[[column]])
      
    )
}

# Calcolo delle statistiche per phishing e legittimi
stats_phishing <- lapply(columns_to_analyze, function(col) {
  calculate_stats(combined_dataset %>% filter(label == 0), col)
}) %>% bind_rows() %>% mutate(Tipo = "Phishing", Colonna = columns_to_analyze)

stats_legit <- lapply(columns_to_analyze, function(col) {
  calculate_stats(combined_dataset %>% filter(label == 1), col)
}) %>% bind_rows() %>% mutate(Tipo = "Legittimo", Colonna = columns_to_analyze)

# Unione delle statistiche in un unico dataframe
stats_combined <- bind_rows(stats_phishing, stats_legit)

# Visualizzazione delle statistiche
print(stats_combined)