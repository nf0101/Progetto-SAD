library(ggplot2)
library(dplyr)
# Filtra solo gli URL offuscati con lunghezza inferiore a 1000
filtered_data_limited <- combined_dataset %>%
  filter(HasObfuscation == 1, URLLength <= 10000)

# Crea il grafico di correlazione limitando URLLength
correlation_plot_limited <- ggplot(filtered_data_limited, aes(x = URLLength, y = NoOfObfuscatedChar, use = "complete.obs")) +
  geom_point(alpha = 0.5, color = "blue") +  # Scatter plot con punti semitrasparenti
 geom_smooth(method = "lm", color = "red", se = TRUE) +  # Linea di regressione lineare con intervallo di confidenza
  labs(
    title = "Correlazione tra NoOfObfuscatedChar e URLLength (URL < 2000 caratteri)",
    x = "Lunghezza dell'URL",
    y = "Numero di Caratteri Offuscati",
    caption = "Dati filtrati per URL con offuscamento (HasObfuscation = 1) e URLLength ≤ 2000"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    plot.caption = element_text(size = 10, hjust = 0.5)
  )

# Mostra il grafico
print(correlation_plot_limited)

# Analisi dei dati esclusi (facoltativo)
excluded_data <- combined_dataset %>%
  filter(HasObfuscation == 1, URLLength > 2000)

# Stampa il numero di URL esclusi
cat("Numero di URL esclusi (URLLength > 10000):", nrow(excluded_data), "\n", excluded_data$URLLength)
