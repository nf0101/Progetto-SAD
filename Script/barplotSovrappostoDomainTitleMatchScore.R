library(ggplot2)
labelValue <- 1
binsNum = 35
# Crea un istogramma con conteggi sulle barre
print(ggplot() +
  # Istogramma del combined_dataset
  geom_histogram(
    data = combined_dataset[combined_dataset$label == labelValue, ],
    aes(x = DomainTitleMatchScore, y = ..count..),
    bins = binsNum,
    fill = "blue",
    color = "black",
    alpha = 0.4
  ) +
  # Etichette sui conteggi del combined_dataset
  stat_bin(
    data = combined_dataset[combined_dataset$label == labelValue, ],
    aes(x = DomainTitleMatchScore, y = ..count.., label = ..count..),
    bins = binsNum,
    geom = "text",
    vjust = -0.5,  # Posiziona sopra le barre
    size = 3,      # Dimensione del testo
    color = "blue"
  ) +
  # Istogramma del filtered_dataset
  geom_histogram(
    data = filtered_dataset[filtered_dataset$label == labelValue, ],
    aes(x = DomainTitleMatchScore, y = ..count..),
    bins = binsNum,
    fill = "red",
    color = "black",
    alpha = 0.4
  ) +
  # Etichette sui conteggi del filtered_dataset
  stat_bin(
    data = filtered_dataset[filtered_dataset$label == labelValue, ],
    aes(x = DomainTitleMatchScore, y = ..count.., label = ..count..,
        vjust = ifelse(..x.. < first_bin_max, 1.5, -0.5)),
    bins = binsNum,
    geom = "text",
    #vjust = -0.5,  # Posiziona sopra le barre
    size = 3,      # Dimensione del testo
    color = "red"
  ) +
  scale_y_continuous(
    trans = scales::pseudo_log_trans(base = 10),  # Gestione dei valori 0
    breaks = c(0, 10, 100, 1000, 10000),       # Specifica i punti della scala
    labels = c("0", "10", "100", "1000", "10000") # Etichette desiderate
  ) +
  labs(
    title = "Istogramma sovrapposto di DomainTitleMatchScore (Legittimi)",
    x = "DomainTitleMatchScore",
    y = "Frequenza (scala logaritmica)"
  ) +
  theme_minimal()
)