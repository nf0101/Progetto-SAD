library(ggplot2)

# Lista delle colonne da analizzare
columns_to_plot <- c("NoOfEqualsInURL") # Puoi aggiungere altre colonne alla lista

# Loop per creare un grafico per ogni colonna
for (col in columns_to_plot) {
  # Creazione del grafico
  p <- ggplot(combined_dataset, aes(x = factor(label, labels = c("Phishing", "Legittimi")), 
                                    y = .data[[col]], 
                                    fill = factor(label, labels = c("Phishing", "Legittimi")))) +
    geom_violin(trim = FALSE, alpha = 0.7) +  # Grafico a violino senza tagliare i bordi
    scale_fill_manual(values = c("Phishing" = "red", "Legittimi" = "green")) +
    scale_y_continuous(
     trans = scales::pseudo_log_trans(base = 10),  # Gestione dei valori 0
      breaks = c(0, 10, 100, 1000),       # Specifica i punti della scala
      labels = c("0", "10", "100", "1000") # Etichette desiderate
      ) +
    # Aggiungi mediana
    stat_summary(fun = median, geom = "point", shape = 23, size = 3, fill = "white", color = "black") +
    # Aggiungi primo e terzo quartile
    stat_summary(
      fun.min = function(y) quantile(y, 0.25),
      fun.max = function(y) quantile(y, 0.75),
      fun = median,  # Linea centrale opzionale, altrimenti usa NA
      geom = "errorbar", 
      width = 0.05,
      color = "black"
    ) +
    labs(
      title = paste("Grafico a Violino per", col),
      x = "Tipo di URL",
      y = col,
      fill = "Tipo di URL"
    ) +
    theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(hjust = 0.5),
      legend.position = "none"  # Nasconde la legenda
    )
  
  # Mostra il grafico
  print(p)
}
