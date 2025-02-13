library(ggplot2)
library(dplyr)

# Raggruppa i dati in base ai valori di label e HasObfuscation, e conta le occorrenze
obfuscation_by_label <- combined_dataset %>%
  group_by(label, HasObfuscation) %>%
  summarise(count = n(), .groups = 'drop')

# Crea una nuova colonna per combinare label e HasObfuscation
obfuscation_by_label <- obfuscation_by_label %>%
  mutate(category = paste0("Label: ", label, " - Obfuscation: ", HasObfuscation))

# Crea un grafico a barre per ciascuna combinazione di label e HasObfuscation
bar_plot <- ggplot(obfuscation_by_label, aes(x = category, y = count, fill = factor(HasObfuscation))) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  labs(title = "Distribuzione di HasObfuscation per siti di Phishing e Legittimi",
       x = "Categoria",
       y = "Conteggio",
       fill = "HasObfuscation") +
  scale_fill_manual(values = c("0" = "lightblue", "1" = "orange"), labels = c("No Offuscamento", "Con Offuscamento")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Stampa il grafico
print(bar_plot)
