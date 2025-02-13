library(ggplot2)
library(dplyr)

# Creazione di un data frame con le occorrenze per NoOfQMarkInURL dove label è uguale a 0 (phishing)
occurrences_df <- combined_dataset %>%
  filter(label == 0) %>%
  count(NoOfQMarkInURL) %>%
  arrange(NoOfQMarkInURL)
occurrences_df$NoOfQMarkInURL <- factor(occurrences_df$NoOfQMarkInURL)

# Creazione del grafico
print(ggplot(occurrences_df, aes(x = factor(NoOfQMarkInURL), y = n, fill = NoOfQMarkInURL)) +
  geom_bar(stat = "identity", show.legend = FALSE, width = 0.5) +
  geom_text(aes(label = n), vjust = -0.5, size = 4) +  # Aggiungi i valori sopra le barre
  
  scale_y_continuous(
    trans = scales::pseudo_log_trans(base = 10),  # Gestione dei valori 0
    breaks = c(0, 10, 100, 1000, 10000, 100000),       # Specifica i punti della scala
    labels = c("0", "10", "100", "1000", "10000", "100000") # Etichette desiderate
  ) +
  labs(
    title = "Distribuzione di NoOfQMarkInURL nei siti di phishing",
    x = "Numero di QMark",
    y = "Occorrenze"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
)