library(dplyr)

# Supponendo che i dataset "combined_dataset", "synthetic_dataframe" e "synthetic_dataframe_context"
# siano già caricati in ambiente e contengano le colonne URLLength, DomainLength e label.

# Aggiungere una colonna per distinguere i dataset
combined_dataset$Dataset <- "Dataset Originale"
synthetic_dataframe$Dataset <- "Synthetic Dataset"
synthetic_dataframe_context$Dataset <- "Synthetic Context Dataset"

# Combinare i dataset in un unico data frame
all_data <- bind_rows(combined_dataset, synthetic_dataframe, synthetic_dataframe_context)

# Convertire la variabile 'label' in fattore (1 = Legittimi, 0 = Phishing)
all_data <- all_data %>%
  mutate(label = factor(label, levels = c(1, 0), labels = c("Legittimi", "Phishing")))

# Calcolo delle statistiche per URLLength, raggruppate per Dataset e label
url_stats <- all_data %>%
  group_by(Dataset, label) %>%
  summarise(
    Min = min(URLLength, na.rm = TRUE),
    `1° Quart.` = quantile(URLLength, probs = 0.25, na.rm = TRUE),
    Mediana = median(URLLength, na.rm = TRUE),
    Media = mean(URLLength, na.rm = TRUE),
    `3° Quart.` = quantile(URLLength, probs = 0.75, na.rm = TRUE),
    Max = max(URLLength, na.rm = TRUE),
    `Dev. st.` = sd(URLLength, na.rm = TRUE),
    .groups = "drop"
  )

# Calcolo delle statistiche per DomainLength, raggruppate per Dataset e label
domain_stats <- all_data %>%
  group_by(Dataset, label) %>%
  summarise(
    Min = min(DomainLength, na.rm = TRUE),
    `1° Quart.` = quantile(DomainLength, probs = 0.25, na.rm = TRUE),
    Mediana = median(DomainLength, na.rm = TRUE),
    Media = mean(DomainLength, na.rm = TRUE),
    `3° Quart.` = quantile(DomainLength, probs = 0.75, na.rm = TRUE),
    Max = max(DomainLength, na.rm = TRUE),
    `Dev. st.` = sd(DomainLength, na.rm = TRUE),
    .groups = "drop"
  )

# Visualizzazione dei risultati
cat("Statistiche per URLLength:\n")
print(url_stats)

cat("\nStatistiche per DomainLength:\n")
print(domain_stats)
