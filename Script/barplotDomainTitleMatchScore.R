library(ggplot2)

# Crea un istogramma con asse y logaritmico
print(
  # Sovrapposizione di due istogrammi
  ggplot() +
    # Istogramma del combined_dataset
    geom_histogram(
      data = filtered_dataset[filtered_dataset$label == 1, ],
      aes(x = DomainTitleMatchScore),
      bins = 30,
      fill = "red",
      color = "black",
      alpha = 0.4
    ) +
    # Istogramma del filtered_dataset
    scale_y_continuous(
      trans = scales::pseudo_log_trans(base = 10),  # Gestione dei valori 0
      breaks = c(0, 10, 100, 1000, 10000),       # Specifica i punti della scala
      labels = c("0", "10", "100", "1000", "10000") # Etichette desiderate
    ) +
    labs(
      title = "Istogramma sovrapposto di DomainTitleMatchScore (Phishing)",
      x = "DomainTitleMatchScore",
      y = "Frequenza (scala logaritmica)"
    ) +
    theme_minimal()
  
)