# Carica le librerie necessarie
library(ggplot2)
library(dplyr)

# Controlla che le colonne necessarie esistano
if (!all(c("SpacialCharRatioInURL", "label") %in% colnames(combined_dataset))) {
  stop("Il dataset deve contenere le colonne 'SpacialCharRatioInURL' e 'label'.")
}

# Converte la colonna label in un fattore per un'etichettatura chiara nel grafico
combined_dataset <- combined_dataset %>%
  mutate(label = factor(label, labels = c("Phishing", "Legittimo")))

# Crea un box plot per confrontare il valore di SpacialCharRatioInURL tra siti di phishing e legittimi
box_plot <- ggplot(combined_dataset, aes(x = label, y = SpacialCharRatioInURL, fill = label)) +
  geom_boxplot(outlier.color = "red", outlier.shape = 16, outlier.size = 2) +
  labs(title = "Confronto del SpacialCharRatioInURL tra siti di Phishing e Legittimi",
       x = "Tipo di Sito",
       y = "Special Character Ratio in URL") +
  scale_fill_manual(values = c("Phishing" = "red", "Legittimo" = "green")) +
  theme_minimal()

# Stampa il box plot
print(box_plot)
