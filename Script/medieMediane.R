# Calcolare le statistiche di URLLength e DomainLength separatamente per siti di phishing (label == 0) e legittimi (label == 1)

# Per i siti di phishing (label == 0)
phishing_data <- subset(combined_dataset, label == 0)

# Media e mediana di URLLength per siti di phishing
mean_phishing_url <- mean(phishing_data$URLLength, na.rm = TRUE)
median_phishing_url <- median(phishing_data$URLLength, na.rm = TRUE)

# Media e mediana di DomainLength per siti di phishing
mean_phishing_domain <- mean(phishing_data$DomainLength, na.rm = TRUE)
median_phishing_domain <- median(phishing_data$DomainLength, na.rm = TRUE)

# Per i siti legittimi (label == 1)
legit_data <- subset(combined_dataset, label == 1)

# Media e mediana di URLLength per siti legittimi
mean_legit_url <- mean(legit_data$URLLength, na.rm = TRUE)
median_legit_url <- median(legit_data$URLLength, na.rm = TRUE)

# Media e mediana di DomainLength per siti legittimi
mean_legit_domain <- mean(legit_data$DomainLength, na.rm = TRUE)
median_legit_domain <- median(legit_data$DomainLength, na.rm = TRUE)

# Stampa i risultati
cat("Statistiche per URL di Phishing (label = 0):\n")
cat("Media lunghezza URL:", mean_phishing_url, "\n")
cat("Mediana lunghezza URL:", median_phishing_url, "\n")
cat("Media Domain Length:", mean_phishing_domain, "\n")
cat("Mediana Domain Length:", median_phishing_domain, "\n\n")

cat("Statistiche per URL Legittimi (label = 1):\n")
cat("Media lunghezza URL:", mean_legit_url, "\n")
cat("Mediana lunghezza URL:", median_legit_url, "\n")
cat("Media Domain Length:", mean_legit_domain, "\n")
cat("Mediana Domain Length:", median_legit_domain, "\n")
