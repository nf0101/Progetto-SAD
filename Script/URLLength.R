library(ggplot2)
library(dplyr)

# Crea due versioni del dataset: uno filtrato (senza outliers) e uno completo
dataset_filtered <- combined_dataset %>%
  filter(URLLength <= 500) %>%
  mutate(group = "Senza Outliers")

dataset_complete <- combined_dataset %>%
  mutate(group = "Con Outliers")

# Combina i due dataset
combined_data <- bind_rows(dataset_filtered, dataset_complete)

# Crea il grafico a violino con due pannelli: uno per i dati filtrati e uno per i dati completi
violin_plot <- ggplot(combined_data, aes(x = factor(label), y = URLLength, fill = factor(label))) +
  geom_violin(trim = FALSE) +  # Grafico a violino
  labs(title = "Distribuzione della lunghezza degli URL",
       x = "Tipo di sito (1 = Legittimo, 0 = Phishing)",
       y = "Lunghezza URL") +
  scale_fill_manual(values = c("red", "green"), labels = c("Phishing", "Legittimo")) +
  theme_minimal() +
  facet_wrap(~group, scales = "free_y")  # Dividi in due sezioni con scale indipendenti

# Aggiungi i cerchietti rossi solo per gli outliers con URLLength > 500 per i siti di phishing
violin_plot <- violin_plot +
  geom_jitter(data = subset(combined_data, label == 0 & group == "Con Outliers" & URLLength > 500),
              aes(x = factor(label), y = URLLength),
              color = "red",
              size = 1.5,   # Dimensione dei cerchietti
              width = 0.1,  # Per evitare sovrapposizione
              alpha = 0.6)  # Trasparenza dei punti

# Stampa il grafico
print(violin_plot)
