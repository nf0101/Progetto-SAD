library(ggplot2)

labelValue <- 1
binsNum = 35
# Calcola i conteggi dei bin per il combined_dataset
combined_histogram <- ggplot_build(
  ggplot(data = combined_dataset[combined_dataset$label == labelValue, ]) +
    geom_histogram(aes(x = URLTitleMatchScore), bins = binsNum)
)$data[[1]]

# Trova il limite superiore del primo bin
first_bin_max <- combined_histogram$xmax[which.min(combined_histogram$xmin)]

# Grafico aggiornato
print(ggplot() +
        # Istogramma del combined_dataset
        geom_histogram(
          data = combined_dataset[combined_dataset$label == labelValue, ],
          aes(x = URLTitleMatchScore, y = ..count..),
          bins = binsNum,
          fill = "blue",
          color = "black",
          alpha = 0.4
        ) +
        # Etichette sui conteggi del combined_dataset
        stat_bin(
          data = combined_dataset[combined_dataset$label == labelValue, ],
          aes(
            x = URLTitleMatchScore, 
            y = ..count.., 
            label = ..count..,
            vjust = ifelse(..x.. < first_bin_max, -0.5, -0.5) # Modifica vjust solo per il primo bin
          ),
          bins = binsNum,
          geom = "text",
          size = 3,
          color = "blue"
        ) +
        # Istogramma del filtered_dataset
        geom_histogram(
          data = filtered_dataset[filtered_dataset$label == labelValue, ],
          aes(x = URLTitleMatchScore, y = ..count..),
          bins = binsNum,
          fill = "red",
          color = "black",
          alpha = 0.4
        ) +
        # Etichette sui conteggi del filtered_dataset
        stat_bin(
          data = filtered_dataset[filtered_dataset$label == labelValue, ],
          aes(x = URLTitleMatchScore, y = ..count.., label = ..count..,#),
             vjust = ifelse(..x.. < first_bin_max, 1.5, -0.5)
          ),
          bins = binsNum,
          geom = "text",
         # vjust = -0.5, # Mantiene il valore predefinito per il filtered_dataset
          size = 3,
          color = "red"
        ) +
        scale_y_continuous(
          trans = scales::pseudo_log_trans(base = 10),  # Gestione dei valori 0
          breaks = c(0, 10, 100, 1000, 10000, 100000),  # Specifica i punti della scala
          labels = c("0", "10", "100", "1000", "10000", "100000") # Etichette desiderate
        ) +
        labs(
          title = "Istogramma sovrapposto di URLTitleMatchScore (Legittimi)",
          x = "URLTitleMatchScore",
          y = "Frequenza (scala logaritmica)"
        ) +
        theme_minimal() +
        theme(
          axis.text.y = element_text(size = 12), 
          axis.title.y = element_text(size = 14)
        )
)
