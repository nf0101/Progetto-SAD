library(ggplot2)
#LineOfCode
# Creare un grafico a violino per la distribuzione di 'LineOfCode' con mediana e quartili
print(ggplot(combined_dataset, aes(x = factor(label, labels = c("Phishing", "Legittimi")), 
                                   y = LargestLineLength, 
                                   fill = factor(label, labels = c("Phishing", "Legittimi")))) +
        geom_violin(trim = FALSE, alpha = 0.7) +  # Grafico a violino senza tagliare i bordi
        scale_fill_manual(values = c("Phishing" = "red", "Legittimi" = "green")) +
        scale_y_continuous(
          trans = scales::pseudo_log_trans(base = 10),  # Gestione dei valori 0
          breaks = c(0, 10, 100, 1000, 10000, 100000, 1000000, 10000000),       # Specifica i punti della scala
          labels = c("0", "10", "100", "1000", "10k", "100k", "1mln", "10mln")         # Etichette desiderate
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
          title = "Distribuzione di LargestLineLength per Tipo di URL",
          x = "Tipo di URL",
          y = "LargestLineLength",
          fill = "Tipo di URL"
        ) +
        theme_minimal(base_size = 14) +
        theme(
          plot.title = element_text(hjust = 0.5),
          legend.position = "none"  # Nasconde la legenda
        )
)