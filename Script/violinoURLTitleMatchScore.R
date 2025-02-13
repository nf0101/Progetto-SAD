library(ggplot2)

# Creare l'istogramma della distribuzione di URLLength diviso per label

# Creare il grafico a violino con boxplot sovrapposto
print(ggplot(synthetic_dataframe_context, aes(x = as.factor(label), y = URLLength, fill = as.factor(label))) +
        geom_violin(alpha = 0.6, ) +  # Grafico a violino con trasparenza
        geom_boxplot(width = 0.06, outlier.color = "black", outlier.size = 1, alpha = 0.4, outlier.alpha = 0.4, outlier.shape = 1, staplewidth = 1) +  # Boxplot più sottile senza outlier visibili
        scale_fill_manual(values = c("red", "green"), labels = c("Phishing", "Legittimo")) +
        labs(
          title = "Distribuzione di URLLength per URL Legittimi e di Phishing",
          x = "Tipo di URL",
          y = "URLLength",
          fill = "Tipo di URL"
        ) +
      scale_y_continuous(
       trans = scales::pseudo_log_trans(base = 10),  # Gestione dei valori 0
          breaks = c(0, 10, 100, 1000, 10000),#, 100000, 1000000, 10000000),       # Specifica i punti della scala
          labels = c("0", "10", "100", "1000", "10k")#, "100k", "1mil", "10mil") # Etichette desiderate
        ) +
        theme_minimal() +
        theme(
          #legend.position = "none", # Nasconde la legenda (opzionale, dato che c'è l'asse X)
          legend.text = element_text(size = 15),
          legend.title = element_text(size = 15),
          axis.text.x = element_blank(),
          axis.text = element_text(size = 15),
          axis.title = element_text(size = 16),
          plot.title = element_text(size = 16, hjust = 0.5),
          #panel.grid.minor = element_blank() # Rimuove le griglie minori per chiarezza
        )
)
