library(ggplot2)
library(scales)
library(patchwork)

# Lista dei dataset e nomi corrispondenti
datasets <- list(combined_dataset, synthetic_dataframe, synthetic_dataframe_context)
dataset_names <- c("Dataset Originale", "Synthetic Dataset", "Synthetic Context Dataset")

# Trova i valori minimi e massimi per una scala comune
y_min <- 0  # Minimo assoluto
y_max <- max(sapply(datasets, function(df) max(df$URLCharProb, na.rm = TRUE)))  # Massimo tra tutti i dataset

# Funzione per generare il grafico a violino con boxplot e scalaa fissa
plot_violin_boxplot <- function(data, dataset_name) {
  ggplot(data, aes(x = as.factor(label), y = URLCharProb, fill = as.factor(label))) +
    geom_violin(alpha = 0.6, color = "black", scale = "width", trim = TRUE) +  # Grafico a violino
    geom_boxplot(width = 0.06, outlier.color = "black", outlier.size = 1, 
                 alpha = 0.4, outlier.alpha = 0.4, outlier.shape = 1, staplewidth = 1) +  # Boxplot incorporato
    scale_fill_manual(
      values = c("red", "green"),
      labels = c("Phishing", "Legittimo"),
      name = "Tipo di URL"  # **Rinomina l'estetica della legenda**
    ) +
    scale_y_continuous(
      trans = scales::pseudo_log_trans(base = 10),  # Scala pseudo-logaritmica
      breaks = c(0, 10, 100, 1000, 10000),
      labels = c("0", "10", "100", "1000", "10k"),
      limits = c(y_min, y_max)  # **Fissa la scala Y**
    ) +
    labs(
      title = dataset_name,
      x = NULL,  # Rimuove il titolo dell'asse X
      y = NULL  # Rimuove il titolo dell'asse Y (sarà aggiunto solo a sinistra)
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_blank(),
      axis.text = element_text(size = 12),
      plot.title = element_text(size = 14, hjust = 0.5)
    )
}

# Creare i tre grafici con scala Y comune
plot1 <- plot_violin_boxplot(datasets[[1]], dataset_names[1])
plot2 <- plot_violin_boxplot(datasets[[2]], dataset_names[2])
plot3 <- plot_violin_boxplot(datasets[[3]], dataset_names[3])

# Unire i tre grafici in un unico pannello con una sola etichetta Y e la legenda in basso
final_plot <- (plot1 + plot2 + plot3) +
  plot_layout(ncol = 3, guides = "collect") &  # Disposizione orizzontale con legenda comune
  theme(
    legend.position = "bottom",
    plot.title = element_text(size = 16, hjust = 0.5)
  )

# Aggiungere una sola etichetta per l'asse Y
final_plot <- final_plot + plot_annotation(
  title = "Distribuzione di URLCharProb nei tre dataset",
) & theme(
  axis.title.y = element_text(size = 14)
)

# Mostrare il grafico combinato
print(final_plot)
