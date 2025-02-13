library(ggplot2)
library(dplyr)

# Calcola i limiti per rimuovere gli outliers (1.5 volte l'intervallo interquartile)
Q1 <- quantile(combined_dataset$NoOfJS, 0.25)
Q3 <- quantile(combined_dataset$NoOfJS, 0.75)
IQR <- Q3 - Q1

lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR

# Filtra i dati per rimuovere gli outliers
filtered_dataset <- combined_dataset %>%
  filter(NoOfJS >= lower_bound & NoOfJS <= upper_bound)

# Crea un grafico a violino senza outliers
violin_plot_js_filtered <- ggplot(filtered_dataset, aes(x = factor(label), y = NoOfJS, fill = factor(label))) +
  geom_violin(trim = FALSE) +
  labs(title = "Distribuzione di NoOfJS in base alla legittimità del sito (Senza outliers)",
       x = "Tipo di sito (0 = Phishing, 1 = Legittimo)",
       y = "Numero di file JavaScript (NoOfJS)") +
  scale_fill_manual(values = c("red", "green"), labels = c("Phishing", "Legittimo")) +
  theme_minimal()

# Stampa il grafico
print(violin_plot_js_filtered)
