library(dplyr)

# Conta il numero di URL per ogni TLD, separando per phishing (label = 0)
tld_counts_phishing <- combined_dataset_clean %>%
  filter(label == 0) %>%
  group_by(TLD) %>%
  summarise(n_url = n(), .groups = "drop")

# Calcola le statistiche separatamente per phishing
stats_phishing <- tld_counts_phishing %>%
  summarise(
    Min = min(n_url),
    Q1 = quantile(n_url, 0.25),
    Media = mean(n_url),
    Mediana = median(n_url),
    Moda = as.numeric(names(sort(table(n_url), decreasing = TRUE)[1])),  # Moda
    Q3 = quantile(n_url, 0.75),
    Max = max(n_url),
    `Dev. Standard` = sd(n_url)
  )

# Visualizza il risultato
stats_phishing <- stats_phishing %>%
  mutate_if(is.numeric, round, digits = 2)
print(as.data.frame(stats_phishing))

library(dplyr)

# Conta il numero di URL per ogni TLD e separa per phishing (label = 0)
tld_counts_phishing <- combined_dataset_clean %>%
  filter(label == 0) %>%
  group_by(TLD) %>%
  summarise(n_url = n(), .groups = "drop")

# Calcola la media e la mediana degli URL di phishing per ogni TLD
tld_stats <- tld_counts_phishing %>%
  summarise(
    Media = mean(n_url),
    Mediana = median(n_url)
  )

library(dplyr)

# Conta il numero di URL per ogni TLD, separando per phishing (label = 0) e legittimi (label = 1)
tld_counts <- combined_dataset_clean %>%
  group_by(TLD, label) %>%
  summarise(n_url = n(), .groups = "drop")

# Conta il numero totale di URL per ogni TLD (sia legittimi che di phishing)
tld_total_counts <- combined_dataset_clean %>%
  group_by(TLD) %>%
  summarise(total_urls = n(), .groups = "drop")

# Unisci i dati di phishing con i totali per ciascun TLD
tld_percentage <- tld_counts %>%
  left_join(tld_total_counts, by = "TLD") %>%
  filter(label == 0) %>%  # Seleziona solo i siti di phishing (label == 0)
  mutate(
    percentuale = (n_url / total_urls) * 100  # Calcola la percentuale di phishing per ciascun TLD
  )

# Calcola la media e la mediana delle percentuali di phishing per ogni TLD
tld_stats <- tld_percentage %>%
  summarise(
    Percentuale_Media = mean(percentuale),
    Percentuale_Mediana = median(percentuale)
  )

# Visualizza i risultati
print(tld_stats)

