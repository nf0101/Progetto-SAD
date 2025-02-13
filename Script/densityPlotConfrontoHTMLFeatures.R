# Caricamento delle librerie necessarie
library(ggplot2)
library(tidyr)
library(dplyr)
library(scales)

# Ristrutturazione del dataset in formato lungo
long_data <- combined_dataset %>%
  select(label, NoOfPopup, NoOfiFrame, NoOfURLRedirect, NoOfSelfRedirect, 
         NoOfSelfRef, NoOfEmptyRef, NoOfExternalRef) %>%
  pivot_longer(cols = -label, names_to = "Feature", values_to = "Value")
# Creazione del grafico
print(# Creazione del grafico
  # Creazione del grafico
  # Grafico di densità per tutte le feature
  ggplot(long_data, aes(x = Value, fill = as.factor(label), color = as.factor(label))) +
    geom_density(alpha = 0.5) + # Grafico di densità con trasparenza
    scale_fill_manual(
      values = c("1" = "darkgreen", "0" = "darkred"),
      labels = c("Legittimi", "Phishing")
    ) +
    scale_color_manual(
      values = c("1" = "darkgreen", "0" = "darkred"),
      labels = c("Legittimi", "Phishing")
    ) +
    scale_x_continuous(trans = "pseudo_log", labels = scales::comma) + # Scala pseudo-logaritmica per l'asse X
    labs(
      title = "Distribuzione di densità per URL legittimi e di phishing",
      x = "Valore (Scala pseudo-logaritmica)",
      y = "Densità",
      fill = "Tipo di URL",
      color = "Tipo di URL"
    ) +
    facet_wrap(~ Feature, scales = "free") + # Faceting per ogni feature
    theme_minimal() +
    theme(
      axis.text.x = element_text(size = 10),
      axis.text.y = element_text(size = 10),
      axis.title.x = element_text(size = 14),
      axis.title.y = element_text(size = 14),
      strip.text = element_text(size = 14), # Titoli delle feature
      legend.text = element_text(size = 12),
      legend.position = "top"
    )
  
  
)