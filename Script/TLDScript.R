# Carica le librerie necessarie
library(ggplot2)
library(dplyr)

# Assumiamo che tu abbia già caricato e combinato i dataset come nel codice precedente

# Controlla che la colonna TLD esista
if (!"TLD" %in% colnames(combined_dataset)) {
  stop("La colonna 'TLD' non è presente nel dataset.")
}

# Raggruppa i dati per TLD e conta il numero di siti di phishing e legittimi
tld_summary <- combined_dataset_clean %>%
  group_by(TLD, label) %>%
  summarise(count = n(), .groups = 'drop') %>%
  ungroup()

# Filtra per i TLD più comuni (ad esempio, i primi 50 per conteggio totale)
top_tlds <- tld_summary %>%
  group_by(TLD) %>%
  summarise(total_count = sum(count), .groups = 'drop') %>%
  arrange(desc(total_count)) %>%
  slice_head(n = 50) %>%  # Mantieni solo i primi 50 TLD
  pull(TLD)

# Filtra il riepilogo per i TLD selezionati
filtered_tld_summary <- tld_summary %>%
  filter(TLD %in% top_tlds)

# Aggiungi una categoria "Altro" per i TLD non comuni
combined_tld_summary <- tld_summary %>%
  filter(!TLD %in% top_tlds) %>%
  group_by(label) %>%
  summarise(count = sum(count), TLD = "Altro", .groups = 'drop')

# Correggi il raggruppamento e somma correttamente
final_summary <- final_summary %>%
  group_by(TLD, label) %>%
  summarise(count = sum(count), .groups = "drop")

# Riordina i TLD mantenendo l'ordine decrescente dei primi 50 e posizionando "Altro" alla fine
final_summary <- final_summary %>%
  mutate(TLD = factor(TLD, levels = c(
    unique(final_summary %>%
             filter(TLD != "Altro") %>%
             arrange(desc(count)) %>%
             pull(TLD)),
    "Altro"
  )))

# Crea il grafico a barre
bar_plot <- ggplot(final_summary, aes(x = TLD, y = count, fill = factor(label))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Conteggio dei URL di Phishing e Legittimi per TLD (Top 50 + Altro)",
       x = "TLD",
       y = "Conteggio",
       fill = "Tipo di Sito") +
  scale_fill_manual(values = c("red", "green"), labels = c("Phishing", "Legittimo")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
        axis.text.y = element_text(size = 12))

# Stampa il grafico
print(bar_plot)

print(bar_plot)

