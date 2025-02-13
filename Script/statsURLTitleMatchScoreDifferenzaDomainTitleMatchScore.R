library(dplyr)
library(ggplot2)
# Filtra le entry dove DomainTitleMatchScore è diverso da URLTitleMatchScore
filtered_entries <- combined_dataset %>%
  filter(DomainTitleMatchScore != URLTitleMatchScore)

# Dividi i dati per URL di phishing e legittimi
phishing_entries <- filtered_entries %>%
  filter(label == 0)  # label == 0 indica URL di phishing

legitimate_entries <- filtered_entries %>%
  filter(label == 1)  # label == 1 indica URL legittimi

# Mostra un'anteprima dei dati
cat("Entry di phishing con DomainTitleMatchScore diverso da URLTitleMatchScore:\n")
#print(head(phishing_entries))

cat("\nEntry di URL legittimi con DomainTitleMatchScore diverso da URLTitleMatchScore:\n")
 #print(head(legitimate_entries))

# Mostra il numero di entry per ciascuna categoria
cat("\nNumero di entry phishing:", nrow(phishing_entries), "\n")
cat("Numero di entry legittime:", nrow(legitimate_entries), "\n")

