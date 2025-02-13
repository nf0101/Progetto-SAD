# Carica le librerie necessarie
library(ggplot2)
library(dplyr)
library(tidyr)

# Assumiamo che tu abbia già caricato e combinato i dataset come nel codice precedente

# Controlla che la colonna TLD esista
if (!"TLD" %in% colnames(combined_dataset_clean)) {
  stop("La colonna 'TLD' non è presente nel dataset.")
}

# Raggruppa i dati per TLD e tipo di sito (label) e conta il numero di phishing (label=0) e legittimi (label=1)
tld_summary <- combined_dataset_clean %>%
  group_by(TLD, label) %>%
  summarise(count = n(), .groups = 'drop') %>%
  pivot_wider(names_from = label, values_from = count, values_fill = list(count = 0)) %>%
  rename(Legittimo = `1`, Phishing = `0`)

# Filtra i TLD dove il numero di siti di phishing è maggiore dei legittimi
tlds_phishing_dominant <- tld_summary %>%
  filter(Phishing > Legittimo)
  count(tlds_phishing_dominant)
  count(tld_summary)
# Ordina i TLD per il numero di siti di phishing e seleziona i primi 25
top_25_tlds <- tlds_phishing_dominant %>%
  arrange(desc(Phishing)) %>%
  head(50)

# Trasforma i dati in formato lungo per poter fare il grafico a barre affiancate
tlds_long <- top_25_tlds %>%
  pivot_longer(cols = c("Phishing", "Legittimo"), names_to = "Legittimità", values_to = "Conteggio")

# Crea il grafico a barre affiancate per i primi 25 TLD
bar_plot <- ggplot(tlds_long, aes(x = reorder(TLD, -Conteggio), y = Conteggio, fill = label)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  labs(title = "Primi 50 TLD con più URL di Phishing rispetto a Legittimi",
       x = "TLD",
       y = "Conteggio") +
  scale_fill_manual(values = c("Phishing" = "red", "Legittimo" = "green")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Stampa il grafico
print(bar_plot)

