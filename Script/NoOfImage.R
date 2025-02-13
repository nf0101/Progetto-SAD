library(ggplot2)
library(dplyr)

# Calcola i limiti per rimuovere gli outliers (1.5 volte l'intervallo interquartile) per NoOfImage
Q1_image <- quantile(combined_dataset$NoOfImage, 0.25)
Q3_image <- quantile(combined_dataset$NoOfImage, 0.75)
IQR_image <- Q3_image - Q1_image

lower_bound_image <- Q1_image - 1.5 * IQR_image
upper_bound_image <- Q3_image + 1.5 * IQR_image

# Filtra i dati per rimuovere gli outliers in NoOfImage
filtered_dataset_image <- combined_dataset %>%
  filter(NoOfImage >= lower_bound_image & NoOfImage <= upper_bound_image)

# Crea un grafico a violino senza outliers per NoOfImage
violin_plot_image_filtered <- ggplot(filtered_dataset_image, aes(x = factor(label), y = NoOfImage, fill = factor(label))) +
  geom_violin(trim = FALSE) +
  labs(title = "Distribuzione di NoOfImage in base alla legittimità del sito (Senza outliers)",
       x = "Tipo di sito (0 = Phishing, 1 = Legittimo)",
       y = "Numero di immagini (NoOfImage)") +
  scale_fill_manual(values = c("red", "green"), labels = c("Phishing", "Legittimo")) +
  theme_minimal()

# Stampa il grafico
print(violin_plot_image_filtered)
