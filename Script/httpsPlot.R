# Carica le librerie necessarie
library(ggplot2)
library(dplyr)
library(vcd)
# Calcola la proporzione di IsHTTPS per ciascun gruppo label
https_percentage <- combined_dataset %>%
  group_by(label, IsHTTPS) %>%
  summarise(count = n(), .groups = 'drop') %>%
  group_by(label) %>%
  mutate(percentage = count / sum(count) * 100)

# Crea un grafico a barre percentuali impilato
stacked_bar <- ggplot(https_percentage, aes(x = label, y = percentage, fill = IsHTTPS)) +
  geom_bar(stat = "identity", color = "black") +
  labs(title = "Percentuale di siti HTTPS e Non HTTPS tra Phishing e Legittimi",
       x = "Tipo di Sito",
       y = "Percentuale",
       fill = "Protocollo") +
  scale_fill_manual(values = c("Non HTTPS" = "orange", "HTTPS" = "blue")) +
  theme_minimal()

# Stampa il grafico
print(stacked_bar)

# Crea la tabella di contingenza
contingency_table <- table(combined_dataset$label, combined_dataset$IsHTTPS)
print(contingency_table)
