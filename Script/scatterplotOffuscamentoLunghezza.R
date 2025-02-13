# Caricare le librerie necessarie
library(ggplot2)
library(dplyr)

# Calcolare il coefficiente di correlazione sull'intero dataset
correlation_value <- cor(combined_dataset$URLLength, combined_dataset$NoOfObfuscatedChar, 
                         method = "pearson", use = "complete.obs")
print(correlation_value)

# Creare un dataset solo per la visualizzazione (filtrando URLLength <= 2000)
filtered_data_plot <- combined_dataset %>%
  filter(URLLength <= 2000)

# Calcolare la regressione lineare senza intercetta
lm_model <- lm(NoOfObfuscatedChar ~ 0 + URLLength, data = combined_dataset) 

# Creare lo scatter plot con la linea di regressione basata sul modello senza intercetta
print(ggplot(filtered_data_plot, aes(x = URLLength, y = NoOfObfuscatedChar)) +
  geom_point(alpha = 0.3, color = "blue") +  # Scatter plot con trasparenza
  geom_abline(intercept = 0, slope = coef(lm_model)[1], 
              color = "red", linewidth = 1.2) +  # Linea che parte da (0,0)
  labs(
    title = "Correlazione tra URLLength e NoOfObfuscatedChar\n",
                  
    x = "Lunghezza dell'URL",
    y = "Numero di caratteri offuscati"
  ) +
  theme_minimal()
)