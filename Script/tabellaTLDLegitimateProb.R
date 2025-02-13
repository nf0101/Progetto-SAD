# Caricare le librerie necessarie
library(dplyr)

# Creare il riassunto per ogni TLD
tld_summary <- combined_dataset_clean %>%
  group_by(TLD) %>%
  summarise(
    TLDLegitimateProb = mean(TLDLegitimateProb, na.rm = TRUE),
    PhishingRatio = mean(label == 0, na.rm = TRUE),  # Proporzione di URL phishing
    Count = n()
  ) %>%
  arrange(desc(TLDLegitimateProb)) %>%  # Ordinare in ordine decrescente
  head(30)  # Selezionare i primi 10

# Stampare la tabella
print(tld_summary, n = 40)

# Per una visualizzazione più chiara con tibble
tld_summary %>% as_tibble()
