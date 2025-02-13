# Caricamento delle librerie
library(dplyr)
library(ggplot2)

# Calcolo delle frequenze di URL phishing e legittimi per TLD
tld_comparison <- combined_dataset_clean %>%
  group_by(TLD, label) %>%
  summarise(Frequency = n(), .groups = 'drop') %>%
  mutate(label = ifelse(label == 1, "Legittimo", "Phishing")) %>%
  arrange(desc(Frequency))

# Calcolo del totale di URL di phishing per ciascun TLD
phishing_counts <- tld_comparison %>%
  filter(label == "Legittimo") %>%
  arrange(desc(Frequency))

# Selezione dei primi 50 TLD in base agli URL di phishing
top_50_phishing_tlds <- phishing_counts %>%
  slice_head(n = 50) %>%
  pull(TLD)

# Filtro dei dati per i TLD nei top 50 ordinati per URL di phishing
tld_comparison_filtered <- tld_comparison %>%
  filter(TLD %in% top_50_phishing_tlds) %>%
  mutate(TLD = factor(TLD, levels = top_50_phishing_tlds))

# Creazione del grafico a barre con confronto
print(ggplot(tld_comparison_filtered, aes(x = TLD, y = Frequency, fill = label)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("Phishing" = "red", "Legittimo" = "green")) +
  labs(
    title = "Confronto tra URL di Phishing e Legittimi per i primi 50 TLD (ordinati per Phishing)",
    x = "TLD",
    y = "Numero di URL",
    fill = "Tipo di URL"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 14),
    axis.text.y = element_text(size = 10),
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold")
  ))
