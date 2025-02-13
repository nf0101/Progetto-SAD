library(ggplot2)
library(dplyr)

phishing_entries_withTitle <- phishing_entries %>%
  filter(HasTitle == 1) 
# Calcola la differenza tra URLTitleMatchScore e DomainTitleMatchScore
phishing_entries_withTitle <- phishing_entries_withTitle %>%
  mutate(ScoreDifference = abs(URLTitleMatchScore - DomainTitleMatchScore))

# Calcola le statistiche descrittive della differenza
stats <- summary(phishing_entries_withTitle$ScoreDifference)

# Mostra le statistiche descrittive
cat("Statistiche descrittive della differenza (ScoreDifference):\n")
print(stats)

# Statistiche aggiuntive: deviazione standard e varianza
std_dev <- sd(phishing_entries_withTitle$ScoreDifference, na.rm = TRUE)
variance <- var(phishing_entries_withTitle$ScoreDifference, na.rm = TRUE)

cat("\nDeviazione standard:", std_dev, "\n")
cat("Varianza:", variance, "\n")

# Creazione dell'istogramma
print(ggplot(phishing_entries_withTitle, aes(x = ScoreDifference)) +
  geom_histogram(bins = 100, fill = "red", color = "black", alpha = 0.7) +
  labs(
    title = "Distribuzione della differenza tra URLTitleMatchScore e DomainTitleMatchScore",
    x = "Differenza tra URLTitleMatchScore e DomainTitleMatchScore",
    y = "Frequenza"
  ) +
  theme_minimal() +
    theme(
      axis.text.x = element_text(size = 15),  # Dimensione etichette asse X
      axis.text.y = element_text(size = 15),  # Dimensione etichette asse Y
      axis.title.x = element_text(size = 17), # Dimensione titolo asse X
      axis.title.y = element_text(size = 17), # Dimensione titolo asse Y
      legend.text = element_text(size = 14),
      legend.title = element_text(size = 15)
    )
  
  )
