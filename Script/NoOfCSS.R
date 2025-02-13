library(ggplot2)
library(dplyr)

# Calcola i limiti per rimuovere gli outliers (1.5 volte l'intervallo interquartile) per NoOfCSS
Q1_css <- quantile(combined_dataset$NoOfCSS, 0.25)
Q3_css <- quantile(combined_dataset$NoOfCSS, 0.75)
IQR_css <- Q3_css - Q1_css

lower_bound_css <- Q1_css - 1.5 * IQR_css
upper_bound_css <- Q3_css + 1.5 * IQR_css

# Filtra i dati per rimuovere gli outliers in NoOfCSS
filtered_dataset_css <- combined_dataset %>%
  filter(NoOfCSS >= lower_bound_css & NoOfCSS <= upper_bound_css)

# Crea un grafico a violino senza outliers per NoOfCSS
violin_plot_css_filtered <- ggplot(filtered_dataset_css, aes(x = factor(label), y = NoOfCSS, fill = factor(label))) +
  geom_violin(trim = FALSE) +
  labs(title = "Distribuzione di NoOfCSS in base alla legittimità del sito (Senza outliers)",
       x = "Tipo di sito (0 = Phishing, 1 = Legittimo)",
       y = "Numero di file CSS (NoOfCSS)") +
  scale_fill_manual(values = c("red", "green"), labels = c("Phishing", "Legittimo")) +
  theme_minimal()

# Stampa il grafico
print(violin_plot_css_filtered)
