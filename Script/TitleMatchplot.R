library(ggplot2)
# Crea il boxplot per visualizzare la distribuzione di DomainTitleMatchScore per label
print(ggplot(combined_dataset, aes(x = factor(label), y = DomainTitleMatchScore, fill = factor(label))) +
  geom_boxplot(alpha = 0.6) +  # Boxplot con colori diversi per i due valori di label
  labs(title = "Distribuzione di DomainTitleMatchScore per Legittimità dell'URL",
       x = "Legittimità del URL (Label)",
       y = "Limited Domain Title Match Score",
       fill = "Label (Legittimità)") +
  scale_fill_manual(values = c("red", "green")) +  # Colori per label
  theme_minimal())
# Calcola media, mediana e moda per i siti legittimi (label = 1) e di phishing (label = 0)
# Usa subset() per separare i dati in base al valore di label

# Media
mean_legit <- mean(subset(combined_dataset, label == 1)$LimitedDomainTitleMatchScore, na.rm = TRUE)
mean_phish <- mean(subset(combined_dataset, label == 0)$LimitedDomainTitleMatchScore, na.rm = TRUE)

# Mediana
median_legit <- median(subset(combined_dataset, label == 1)$LimitedDomainTitleMatchScore, na.rm = TRUE)
median_phish <- median(subset(combined_dataset, label == 0)$LimitedDomainTitleMatchScore, na.rm = TRUE)

# Moda
# Funzione per calcolare la moda
calc_mode <- function(x) {
  uniq_x <- unique(x)
  uniq_x[which.max(tabulate(match(x, uniq_x)))]
}

mode_legit <- calc_mode(subset(combined_dataset, label == 1)$LimitedDomainTitleMatchScore)
mode_phish <- calc_mode(subset(combined_dataset, label == 0)$LimitedDomainTitleMatchScore)

# Stampa i risultati
cat("Siti Legittimi (label = 1):\n")
cat("Media:", mean_legit, "\n")
cat("Mediana:", median_legit, "\n")
cat("Moda:", mode_legit, "\n\n")

cat("Siti di Phishing (label = 0):\n")
cat("Media:", mean_phish, "\n")
cat("Mediana:", median_phish, "\n")
cat("Moda:", mode_phish, "\n")
