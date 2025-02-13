library(ggplot2)
library(dplyr)

# Creazione di un data frame con le occorrenze per NoOfOtherSpecialCharsInURL dove label è uguale a 0 (phishing)
occurrences_df <- combined_dataset %>%
  filter(label == 0) %>%
  count(NoOfOtherSpecialCharsInURL) %>%
  arrange(NoOfOtherSpecialCharsInURL)
occurrences_df$NoOfOtherSpecialCharsInURL <- factor(occurrences_df$NoOfOtherSpecialCharsInURL)

# Creazione del grafico
print(ggplot(occurrences_df, aes(x = factor(NoOfOtherSpecialCharsInURL), y = n, fill = NoOfOtherSpecialCharsInURL)) +
        geom_bar(stat = "identity", show.legend = FALSE, width = 0.4, position = position_dodge(width = 2)) +
        geom_text(aes(label = n), vjust = +0.5, size = 3.5, angle = 45, nudge_y = 0.1) +  # Aggiungi i valori sopra le barre
        
        scale_y_continuous(
          trans = scales::pseudo_log_trans(base = 10),  # Gestione dei valori 0
          breaks = c(0, 10, 100, 1000, 10000, 100000),       # Specifica i punti della scala
          labels = c("0", "10", "100", "1000", "10000", "100000") # Etichette desiderate
        ) +
        scale_x_discrete(expand = c(0.022, 0.022)) +
        labs(
          title = "Distribuzione di NoOfOtherSpecialCharsInURL negli URL di phishing",
          x = "NoOfOtherSpecialCharsInURL",
          y = "Occorrenze"
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(angle = 45, hjust = 1),
         
        )
)