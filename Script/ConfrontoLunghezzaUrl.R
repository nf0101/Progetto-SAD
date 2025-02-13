labelmode <- 0
# Calcola il valore minimo di URLLength per gli URL di phishing
min_phishing <- min(subset(combined_dataset, label == labelmode)$DomainLength, na.rm = TRUE)

# Calcola il valore massimo di URLLength per gli URL di phishing
max_phishing <- max(subset(combined_dataset, label == labelmode)$DomainLength, na.rm = TRUE)

# Conta gli URL di phishing con lunghezza tra il minimo e 130
#count_min_to_130 <- nrow(subset(combined_dataset, label == labelmode & URLLength >= min_phishing & URLLength <= 130))

# Conta gli URL di phishing con lunghezza tra 500 e 1000
#count_500_to_1000 <- nrow(subset(combined_dataset, label == labelmode & URLLength > 500 & URLLength <= 1000))

# Conta gli URL di phishing con lunghezza maggiore di 1000
#count_greater_1000 <- nrow(subset(combined_dataset, label == labelmode & URLLength > 1000))

# Stampa i risultati
cat("Valore minimo di URLLength per gli URL di phishing:", min_phishing, "\n")
cat("Valore massimo di URLLength per gli URL di phishing:", max_phishing, "\n")
cat("Numero di URL di phishing con lunghezza tra il minimo e 130:", count_min_to_130, "\n")
cat("Numero di URL di phishing con lunghezza tra 500 e 1000:", count_500_to_1000, "\n")
cat("Numero di URL di phishing con lunghezza maggiore di 1000:", count_greater_1000, "\n")
