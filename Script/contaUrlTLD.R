library(dplyr)
library(tidyr)

# Specifica il TLD di interesse
specific_tld <- "240"  # Sostituisci con il TLD desiderato

# Filtra il dataset per il TLD specifico e calcola le frequenze per label
tld_summary <- synthetic_dataframe_context %>%
  filter(TLD == specific_tld) %>%
  group_by(label) %>%
  summarise(Frequency = n(), .groups = "drop") %>%
  mutate(
    Tipo = ifelse(label == 0, "Phishing", "Legittimo")
  ) %>%
  select(Tipo, Frequency)

# Mostra il riepilogo per il TLD specificato
cat("Sommario per il TLD specifico '", specific_tld, "':\n", sep = "")
print(tld_summary)

# Mostra gli URL ottenuti per il TLD specificato
urls_specific_tld <- synthetic_dataframe_context %>%
  filter(TLD == specific_tld) %>%
  select(URL, label)

cat("\nGli URL con TLD '", specific_tld, "' sono:\n", sep = "")
print(urls_specific_tld)

# Calcolo della percentuale di URL di phishing per ogni TLD nel dataset sintetico
tld_phishing_percentage <- synthetic_dataframe_context %>%
  group_by(TLD, label) %>%
  summarise(Frequency = n(), .groups = 'drop') %>%
  pivot_wider(names_from = label, values_from = Frequency, values_fill = list(Frequency = 0)) %>%
  mutate(
    TotalURLs = `0` + `1`,
    PhishingPercentage = (`0` / TotalURLs) * 100
  )

# Numero totale di TLD
total_tlds <- nrow(tld_phishing_percentage)

# Numero e percentuale di TLD con percentuale di phishing superiore al 50%
num_tlds_above_50 <- tld_phishing_percentage %>%
  filter(PhishingPercentage > 50) %>%
  nrow()
percent_tlds_above_50 <- (num_tlds_above_50 / total_tlds) * 100

# Numero e percentuale di TLD con 0 URL di phishing
num_tlds_zero_phishing <- tld_phishing_percentage %>%
  filter(`0` == 0) %>%
  nrow()
percent_tlds_zero_phishing <- (num_tlds_zero_phishing / total_tlds) * 100

# Numero e percentuale di TLD con percentuale di phishing inferiore al 10%
num_tlds_below_5 <- tld_phishing_percentage %>%
  filter(PhishingPercentage < 10) %>%
  nrow()
percent_tlds_below_5 <- (num_tlds_below_5 / total_tlds) * 100

# Numero e percentuale di TLD con percentuale di phishing superiore al 90%
num_tlds_above_90 <- tld_phishing_percentage %>%
  filter(PhishingPercentage > 90) %>%
  nrow()
percent_tlds_above_90 <- (num_tlds_above_90 / total_tlds) * 100

# Numero e percentuale di TLD con 0 URL legittimi
num_tlds_zero_legit <- tld_phishing_percentage %>%
  filter(`1` == 0) %>%
  nrow()
percent_tlds_zero_legit <- (num_tlds_zero_legit / total_tlds) * 100

# Stampa dei risultati
cat("\nNumero di TLD con percentuale di URL di phishing superiore al 50%:", num_tlds_above_50, 
    "(", round(percent_tlds_above_50, 2), "%)\n")
cat("Numero di TLD con 0 URL di phishing:", num_tlds_zero_phishing, 
    "(", round(percent_tlds_zero_phishing, 2), "%)\n")
cat("Numero di TLD con percentuale di phishing inferiore al 10%:", num_tlds_below_5, 
    "(", round(percent_tlds_below_5, 2), "%)\n")
cat("Numero di TLD con percentuale di phishing superiore al 90%:", num_tlds_above_90, 
    "(", round(percent_tlds_above_90, 2), "%)\n")
cat("Numero di TLD con 0 URL legittimi:", num_tlds_zero_legit, 
    "(", round(percent_tlds_zero_legit, 2), "%)\n")
