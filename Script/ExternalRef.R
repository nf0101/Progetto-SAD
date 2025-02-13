# Carica le librerie necessarie
library(ggplot2)
library(dplyr)

# Assicurati che le colonne necessarie siano presenti
if (!all(c("NoOfExternalRef", "label") %in% colnames(combined_dataset))) {
  stop("Il dataset deve contenere le colonne 'NoOfExternalRef' e 'label'.")
}

# Filtra i dati per includere solo i valori di NoOfExternalRef inferiori o uguali a 10000
filtered_dataset <- combined_dataset %>%
  filter(NoOfExternalRef <= 50)

# Crea un grafico a violino per mostrare la distribuzione di NoOfExternalRef in base a label
violin_plot <- ggplot(filtered_dataset, aes(x = factor(label), y = NoOfExternalRef, fill = factor(label))) +
  geom_violin(trim = FALSE) +
  labs(title = "Distribuzione di NoOfExternalRef in base alla legittimità del sito (fino a 50)",
       x = "Tipo di sito (0 = Phishing, 1 = Legittimo)",
       y = "Numero di riferimenti esterni (NoOfExternalRef)") +
  scale_fill_manual(values = c("red", "green"), labels = c("Phishing", "Legittimo")) +
  theme_minimal()

# Stampa il grafico
print(violin_plot)
