if (!requireNamespace("ineq", quietly = TRUE)) {
  install.packages("ineq")
}
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
}

library(ineq)
library(ggplot2)

# Calcola la frequenza degli URL per TLD
tld_frequency <- combined_dataset_clean %>%
  group_by(TLD) %>%
  summarise(Frequency = n(), .groups = 'drop') %>%
  arrange(Frequency)  # Ordina in ordine crescente

# Calcola la curva di Lorenz
lorenz_curve <- Lc(tld_frequency$Frequency)

# Calcola il coefficiente di Gini
gini_coefficient <- ineq(tld_frequency$Frequency, type = "Gini")

# Visualizza il risultato
print(paste("Coefficiente di Gini:", round(gini_coefficient, 4)))

# Crea il grafico della curva di Lorenz
lorenz_data <- data.frame(
  Cumulative_TLD = lorenz_curve$p,    # Proporzione cumulativa dei TLD
  Cumulative_URLs = lorenz_curve$L   # Proporzione cumulativa degli URL
)

print(ggplot(lorenz_data, aes(x = Cumulative_TLD, y = Cumulative_URLs)) +
  geom_line(color = "blue", size = 0.7) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(
    title = "Curva di Lorenz per la Distribuzione degli URL tra i TLD",
    x = "Proporzione cumulativa dei TLD",
    y = "Proporzione cumulativa degli URL"
  ) +
  theme_minimal())
