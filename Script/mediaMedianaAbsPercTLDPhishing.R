library(dplyr)
library(ineq)
# Calcola il numero totale di URL (phishing e legittimi) per ciascun TLD
tld_totals <- combined_dataset_clean %>%
  group_by(TLD) %>%
  summarise(
    TotalURLs = n(),  # Totale URL per TLD
    .groups = 'drop'
  )

# Calcola il numero di URL di phishing per ciascun TLD
phishing_totals <- combined_dataset_clean %>%
  filter(label == 0) %>%  # Considera solo gli URL di phishing
  group_by(TLD) %>%
  summarise(
    PhishingURLs = n(),  # Numero di URL di phishing per TLD
    .groups = 'drop'
  )

# Unisce i totali e calcola la percentuale di URL di phishing per ciascun TLD
percentages <- phishing_totals %>%
  inner_join(tld_totals, by = "TLD") %>%
  mutate(
    PhishingPercentage = (PhishingURLs / TotalURLs) * 100 # Percentuale di phishing
  )

# Calcola la media e la mediana dei valori assoluti (numero di URL di phishing)
mean_phishing <- mean(percentages$PhishingURLs, na.rm = TRUE)    # Media assoluta
median_phishing <- median(percentages$PhishingURLs, na.rm = TRUE) # Mediana assoluta

# Calcola la media e la mediana delle percentuali
mean_percentage <- mean(percentages$PhishingPercentage, na.rm = TRUE)    # Media percentuale
median_percentage <- median(percentages$PhishingPercentage, na.rm = TRUE) # Mediana percentuale

# Stampa i risultati
cat("Valori assoluti:\n")
cat("- Media del numero di URL di phishing per TLD:", mean_phishing, "\n")
cat("- Mediana del numero di URL di phishing per TLD:", median_phishing, "\n\n")

cat("Valori percentuali:\n")
cat("- Percentuale media di URL di phishing rispetto a tutti gli URL per TLD:", mean_percentage, "%\n")
cat("- Percentuale mediana di URL di phishing rispetto a tutti gli URL per TLD:", median_percentage, "%\n")

