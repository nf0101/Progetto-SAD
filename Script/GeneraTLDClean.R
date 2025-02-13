library(dplyr)

# Controlla che la colonna TLD esista
if (!"TLD" %in% colnames(combined_dataset)) {
  stop("La colonna 'TLD' non è presente nel dataset.")
}

# Rimuovi gli URL basati su indirizzi IP
combined_dataset_clean <- combined_dataset %>%
  filter(!grepl("^http://(\\d{1,3}\\.){3}\\d{1,3}([:/?]|$)", URL))
combined_dataset_clean <- combined_dataset_clean %>%
  filter(!grepl("^https://(\\d{1,3}\\.){3}\\d{1,3}([:/?]|$)", URL))

# Rimuovi le porte dalla colonna TLD (tutto ciò che segue il ':')
combined_dataset_clean <- combined_dataset_clean %>%
  mutate(TLD = sub(":.*$", "", TLD))

# Stampa il numero totale di TLD unici
cat("Il numero totale di TLD unici è:", n_distinct(combined_dataset_clean$TLD), "\n")

# Calcola il conteggio dei TLD
tld_counts <- combined_dataset_clean %>%
  group_by(TLD) %>%
  summarise(Conteggio = n(), .groups = 'drop')

# Trova i 10 TLD più frequenti
top_10_tlds <- tld_counts %>%
  arrange(desc(Conteggio)) %>%
  slice_head(n = 5)

# Stampa i 10 TLD più frequenti
cat("\nI 10 TLD più frequenti sono:\n")
apply(top_10_tlds, 1, function(row) cat(sprintf("{%s: %d}\n", row["TLD"], as.integer(row["Conteggio"]))))

# Calcola il numero totale di URL nei primi 10 TLD
total_urls_top_10 <- sum(top_10_tlds$Conteggio)

# Stampa il numero totale di URL nei primi 10 TLD
cat("\nIl numero totale di URL che rientrano nei primi 50 TLD è:", total_urls_top_10, "\n")

