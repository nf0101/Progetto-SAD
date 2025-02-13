library(ggplot2)
library(dplyr)
library(tidyr)

# Assicurati che il dataset abbia tutti i valori possibili di TLDLength
tld_complete <- expand.grid(
  TLDLength = unique(combined_dataset$TLDLength),  # Tutte le lunghezze possibili
  label = c(0, 1)  # 0 = Phishing, 1 = Legittimo
) %>%
  left_join(tld_counts, by = c("TLDLength", "label")) %>%
  mutate(Count = replace_na(Count, 0))  # Sostituisci i NA con 0

# Grafico a barre
print(ggplot(tld_complete, aes(x = factor(TLDLength), y = Count, fill = factor(label))) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_text(aes(label = Count), 
            position = position_dodge(width = 0.9), 
            vjust = -0.4, size = 3) + 
  scale_fill_manual(values = c("red", "green"), labels = c("Phishing", "Legittimi")) +
  labs(
    title = "Distribuzione della lunghezza del TLD per tipo di URL",
    x = "Lunghezza del TLD",
    y = "Numero di URL",
    fill = "Tipo di URL"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Ruota i valori se sono troppi
)