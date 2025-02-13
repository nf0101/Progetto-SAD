# Calcolo delle frequenze degli URL per ciascun TLD
tld_frequency <- combined_dataset_clean %>%
  group_by(TLD) %>%
  summarise(Frequency = n(), .groups = 'drop') %>%
  arrange(desc(Frequency))

# Somma totale di tutti gli URL
total_urls <- sum(tld_frequency$Frequency)

# Creazione della categoria "Altro" per TLD con pochi URL (es. < 1% del totale)
tld_frequency <- tld_frequency %>%
  mutate(
    Proportion = Frequency / total_urls,
    TLD = ifelse(Proportion < 0.01, "Altro", TLD) # Raggruppa i TLD rari in "Altro"
  ) %>%
  group_by(TLD) %>%
  summarise(Frequency = sum(Frequency), .groups = 'drop') %>%
  arrange(desc(Frequency))

# Aggiunta delle percentuali per il grafico
tld_frequency <- tld_frequency %>%
  mutate(
    Percentage = round(100 * Frequency / sum(Frequency), 2),
    TLD = factor(TLD, levels = TLD) # Ordina i TLD in ordine decrescente
  )

# Creazione del grafico a torta ordinato
print(ggplot(tld_frequency, aes(x = "", y = Frequency, fill = TLD)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar(theta = "y", start = 0) + # Inizia da zero per l'ordine corretto
  labs(
    title = "Distribuzione dei TLD tra gli URL",
    fill = "TLD"
  ) +
  theme_void() +  # Rimuove gli elementi grafici inutili
  geom_text(
    aes(label = paste0(Percentage, "%")), 
    position = position_stack(vjust = 0.5), size = 4
  )
)