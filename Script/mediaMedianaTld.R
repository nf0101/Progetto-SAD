# Calcolo delle frequenze degli URL per ciascun TLD
tld_frequency <- combined_dataset_clean %>%
  group_by(TLD) %>%
  summarise(Frequency = n(), .groups = 'drop')

# Calcolo della media e della mediana
tld_stats <- tld_frequency %>%
  summarise(
    Media = mean(Frequency),
    Mediana = median(Frequency)
  )

# Visualizzazione dei risultati
print(tld_stats)
