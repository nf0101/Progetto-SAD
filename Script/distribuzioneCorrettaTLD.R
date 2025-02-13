# Installa e carica i pacchetti
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
}
if (!requireNamespace("dplyr", quietly = TRUE)) {
  install.packages("dplyr")
}
library(ggplot2)
library(dplyr)

# Calcola le occorrenze per TLD e suddividi per label
tld_counts <- combined_dataset_clean %>%
  group_by(TLD, label) %>%
  summarise(Count = n(), .groups = 'drop') %>%
  mutate(label = ifelse(label == 1, "Legittimo", "Phishing"))

# Somma totale per ogni TLD
total_counts <- tld_counts %>%
  group_by(TLD) %>%
  summarise(Total = sum(Count), .groups = 'drop')

# Seleziona i primi 50 TLD per occorrenze totali
top_tlds <- total_counts %>%
  arrange(desc(Total)) %>%
  head(100) %>%
  pull(TLD)

# Crea un nuovo dataset con i TLD tra i primi 50 e la categoria "Altro"
tld_counts <- tld_counts %>%
  mutate(TLD = ifelse(TLD %in% top_tlds, TLD, "Altro"))

# Calcola le occorrenze totali per la categoria "Altro"
altro_counts <- combined_dataset_clean %>%
  filter(!TLD %in% top_tlds) %>%
  group_by(label) %>%
  summarise(Count = n(), .groups = 'drop') %>%
  mutate(TLD = "Altro", label = ifelse(label == 1, "Legittimo", "Phishing"))

# Aggiungi la categoria "Altro" al dataset
tld_counts_final <- tld_counts %>%
  group_by(TLD, label) %>%
  summarise(Count = sum(Count), .groups = 'drop') %>%
  bind_rows(altro_counts)

# Ordina i TLD per visualizzazione  
tld_counts_final$TLD <- factor(tld_counts_final$TLD, levels = c(top_tlds, "Altro"))

# Crea il grafico
print(ggplot(tld_counts_final, aes(x = TLD, y = Count, fill = label)) +
        geom_bar(stat = "identity", position = "dodge") +
        scale_fill_manual(values = c("Phishing" = "red", "Legittimo" = "green")) +
        labs(title = "Distribuzione dei TLD per siti di Phishing e Legittimi",
             x = "TLD",
             y = "Numero di Occorrenze",
             fill = "Tipo di Sito") +
        theme_minimal() +
        theme(axis.text.x = element_text(size = 12, angle = 45, hjust = 1),  # Modifica il font delle etichette sull'asse X
              axis.text.y = element_text(size = 12),                         # Modifica il font delle etichette sull'asse Y
              axis.title.x = element_text(size = 14),                        # Modifica il font del titolo dell'asse X
              axis.title.y = element_text(size = 14),                        # Modifica il font del titolo dell'asse Y
              plot.title = element_text(size = 16, hjust = 0.5)))# +
        #coord_flip())
