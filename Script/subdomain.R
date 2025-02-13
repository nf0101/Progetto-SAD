library(ggplot2)

print(
  ggplot(combined_dataset, aes(x = factor(label), y = NoOfSubDomain, fill = factor(label))) +
    geom_violin(trim = FALSE) +
    labs(title = "Distribuzione del numero di sottodomini per siti di Phishing e Legittimi",
         x = "Tipo di sito (0 = Phishing, 1 = Legittimo)",
         y = "Numero di sottodomini") +
    scale_fill_manual(values = c("red", "green"), labels = c("Phishing", "Legittimi")) +
    scale_y_continuous(breaks = seq(0, max(combined_dataset$NoOfSubDomain, na.rm = TRUE), 1)) +  # Mostra solo numeri interi
    theme_minimal()
  
)

library(dplyr)
library(tidyr)

# Crea la tabella con il conteggio per ogni combinazione di NoOfSubDomain e label
tabella_sottodomini <- combined_dataset %>%
  group_by(NoOfSubDomain, label) %>%
  summarise(Conteggio = n(), .groups = 'drop') %>%
  mutate(label = ifelse(label == 0, "Phishing", "Legittimo")) %>%
  pivot_wider(names_from = label, values_from = Conteggio, values_fill = 0) %>%
  arrange(NoOfSubDomain)

# Stampa la tabella
print(tabella_sottodomini)
