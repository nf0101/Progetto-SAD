library(dplyr)
library(tidyr)

# Calcolo della percentuale di phishing per ogni TLD
tld_phishing_percentage <- combined_dataset_clean %>%
  group_by(TLD, label) %>%
  summarise(Frequency = n(), .groups = 'drop') %>%
  pivot_wider(names_from = label, values_from = Frequency, values_fill = list(Frequency = 0)) %>%
  mutate(
    TotalURLs = `0` + `1`,
    PhishingPercentage = (`0` / TotalURLs) * 100
  )

# Definizione delle fasce di percentuale
tld_phishing_percentage <- tld_phishing_percentage %>%
  mutate(
    PercentageRange = case_when(
      PhishingPercentage >= 0 & PhishingPercentage < 10 ~ "0-10%",
      PhishingPercentage >= 10 & PhishingPercentage < 20 ~ "10-20%",
      PhishingPercentage >= 20 & PhishingPercentage < 30 ~ "20-30%",
      PhishingPercentage >= 30 & PhishingPercentage < 40 ~ "30-40%",
      PhishingPercentage >= 40 & PhishingPercentage < 50 ~ "40-50%",
      PhishingPercentage >= 50 & PhishingPercentage < 60 ~ "50-60%",
      PhishingPercentage >= 60 & PhishingPercentage < 70 ~ "60-70%",
      PhishingPercentage >= 70 & PhishingPercentage < 80 ~ "70-80%",
      PhishingPercentage >= 80 & PhishingPercentage < 90 ~ "80-90%",
      PhishingPercentage >= 90 & PhishingPercentage <= 100 ~ "90-100%"
    )
  )

# Calcolo del numero e della percentuale di TLD per ogni fascia
percentage_summary <- tld_phishing_percentage %>%
  group_by(PercentageRange) %>%
  summarise(
    Count = n(),
    Percentage = (Count / nrow(tld_phishing_percentage)) * 100,
    .groups = "drop"
  )

# Ordina le fasce
percentage_summary <- percentage_summary %>%
  arrange(match(PercentageRange, c(
    "0-10%", "10-20%", "20-30%", "30-40%", "40-50%",
    "50-60%", "60-70%", "70-80%", "80-90%", "90-100%"
  )))

# Stampa dei risultati
print(percentage_summary)

# Output chiaro
cat("Fascia | Numero di TLD | Percentuale\n")
cat("----------------------------------\n")
percentage_summary %>%
  mutate(
    Output = paste(PercentageRange, "|", Count, "|", round(Percentage, 2), "%")
  ) %>%
  pull(Output) %>%
  cat(sep = "\n")
