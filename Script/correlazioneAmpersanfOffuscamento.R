# Caricare le librerie necessarie
library(ggplot2)
library(dplyr)

# Filtrare solo gli URL di phishing (label == 0)
phishing_data <- combined_dataset %>%
  filter(label == 0, NoOfAmpersandInURL <= 60)

# Calcolare il coefficiente di correlazione
correlation_value <- cor(phishing_data$NoOfAmpersandInURL, phishing_data$NoOfEqualsInURL, 
                         method = "pearson", use = "complete.obs")

# Modello di regressione lineare
lm_model <- lm(NoOfEqualsInURL ~ NoOfAmpersandInURL, data = phishing_data)

# Estrarre il coefficiente di determinazione R^2
r_squared <- summary(lm_model)$r.squared

# Creare lo scatter plot con la linea di regressione
print(ggplot(phishing_data, aes(x = NoOfAmpersandInURL, y = NoOfEqualsInURL)) +
  geom_point(alpha = 0.3, color = "blue") +  # Punti con trasparenza
  geom_smooth(method = "lm", color = "red", se = TRUE) +  # Linea di regressione
  labs(
    title = paste("Correlazione tra NoOfAmpersandInURL e NoOfEqualsInURL\nr =", round(correlation_value, 3), 
                  " | R^2 =", round(r_squared, 3)),
    x = "NoOfAmpersandInURL",
    y = "NoOfEqualsInURL"
  ) +
  theme_minimal()
)