library(ggplot2)
library(dplyr)
library(tidyr)

# Filtra i TLD dove il numero di URL di phishing è maggiore di quelli legittimi
tld_phishing_greater <- combined_dataset_clean %>%
  group_by(TLD, label) %>%
  summarise(Frequency = n(), .groups = 'drop') %>%
  pivot_wider(names_from = label, values_from = Frequency, values_fill = list(Frequency = 0)) %>%
  mutate(PhishingCount = `0`, LegittimiCount = `1`) %>%
  filter(PhishingCount > LegittimiCount) %>%
  select(TLD, PhishingCount, LegittimiCount)

# Prendi i primi 50 TLD con più URL di phishing, ordinati in base agli URL di phishing
top_50_tlds <- tld_phishing_greater %>%
  arrange(desc(PhishingCount)) %>%
  head(50)

# Filtra il dataset originale per includere solo questi TLD
tld_comparison_filtered <- combined_dataset_clean %>%
  filter(TLD %in% top_50_tlds$TLD)

# Calcola le frequenze degli URL per ogni TLD e completa le combinazioni mancanti
tld_comparison_filtered <- tld_comparison_filtered %>%
  group_by(TLD, label) %>%
  summarise(Frequency = n(), .groups = 'drop') %>%
  complete(TLD, label = c(0, 1), fill = list(Frequency = 0))

# Assicurati che la colonna 'label' sia trattata come un fattore
tld_comparison_filtered$label <- factor(tld_comparison_filtered$label, levels = c(0, 1), labels = c("Phishing", "Legittimo"))

# Calcola il totale degli URL e la percentuale di phishing per ciascun TLD
tld_comparison_filtered <- tld_comparison_filtered %>%
  group_by(TLD) %>%
  mutate(
    TotalURLs = sum(Frequency),
    PhishingPercentage = if_else(label == "Phishing", round(100 * Frequency / TotalURLs, 1), NA_real_),
    TotalPhishing = sum(Frequency[label == "Phishing"])
  ) %>%
  ungroup() %>%
  arrange(desc(TotalPhishing))

# Crea il grafico a barre
print(ggplot(tld_comparison_filtered, aes(x = reorder(TLD, -TotalPhishing), y = Frequency, fill = label)) +
        geom_bar(stat = "identity", position = position_dodge(), color = "white") +
        scale_fill_manual(values = c("Phishing" = "red", "Legittimo" = "green")) +
        labs(
          title = "Confronto tra URL di Phishing e Legittimi per TLD a maggioranza Phishing",
          x = "TLD",
          y = "Numero di URL",
          fill = "Tipo di URL"
        ) +
        # Aggiungi le percentuali come etichette sulle barre di phishing
        geom_text(
          aes(
            label = ifelse(label == "Phishing", paste0(PhishingPercentage), ""),
            y = Frequency + 0.5 # Posiziona sopra la barra
          ),
          position = position_dodge(width = 0),
          size = 3.1,
          vjust = -0.5,
          color = "black"
        ) +
        theme_minimal() +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 13),
          axis.text.y = element_text(size = 12),
          plot.title = element_text(hjust = 0.5, size = 16, face = "bold")
        ))
