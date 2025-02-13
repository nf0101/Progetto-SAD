library(ggplot2)

# Lista delle colonne da analizzare
columns_to_plot <- c("NoOfEqualsInURL")#, "NoOfDegitsInURL", "DegitRatioInURL", 
                     #"LetterRatioInURL", "NoOfLettersInURL", "NoOfQMarkInURL", 
                     #"NoOfEqualsInURL", "NoOfAmpersandInURL", 
                     #"SpacialCharRatioInURL")

# Loop per creare un grafico per ogni colonna
for (col in columns_to_plot) {
  # Creazione del grafico
  p <- ggplot(combined_dataset, aes(x = factor(label, labels = c("Phishing", "Legittimi")), 
                                    y = .data[[col]], 
                                    fill = factor(label, labels = c("Phishing", "Legittimi")))) +
    geom_violin(trim = FALSE, alpha = 0.7) +  # Grafico a violino senza tagliare i bordi
    scale_fill_manual(values = c("Phishing" = "red", "Legittimi" = "green")) +
   # scale_y_log10() +
    labs(
      title = paste("Grafico a Violino per", col),
      x = "Tipo di URL",
      y = col,
      fill = "Tipo di URL"
    ) +
    theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(hjust = 0.5),
      legend.position = "none"  # Nascondi la legenda
    )
  
  # Mostra il grafico
  print(p)
}
