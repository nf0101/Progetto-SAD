# Filtra i dati per HasObfuscation = 1
filtered_data <- combined_dataset[combined_dataset$HasObfuscation == 1, ]
legit_url_df <- combined_dataset[combined_dataset$label == 1, ]
phishing_url_df <- combined_dataset[combined_dataset$label == 0, ]

# Calcolo delle statistiche per URLLength siti legittimi
mean_url_length_legit <- mean(legit_url_df$URLLength, na.rm = TRUE)
median_url_length_legit <- median(legit_url_df$URLLength, na.rm = TRUE)
min_url_length_legit <- min(legit_url_df$URLLength, na.rm = TRUE)
max_url_length_legit <- max(legit_url_df$URLLength, na.rm = TRUE)

# Calcolo delle statistiche per URLLength siti phishing
mean_url_length_phishing <- mean(phishing_url_df$URLLength, na.rm = TRUE)
median_url_length_phishing <- median(phishing_url_df$URLLength, na.rm = TRUE)
min_url_length_phishing <- min(phishing_url_df$URLLength, na.rm = TRUE)
max_url_length_phishing <- max(phishing_url_df$URLLength, na.rm = TRUE)

# Calcolo delle statistiche per URLLength
mean_url_length <- mean(filtered_data$URLLength, na.rm = TRUE)
median_url_length <- median(filtered_data$URLLength, na.rm = TRUE)
min_url_length <- min(filtered_data$URLLength, na.rm = TRUE)
max_url_length <- max(filtered_data$URLLength, na.rm = TRUE)
# Funzione per calcolare la moda
calculate_mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

mode_url_length <- calculate_mode(filtered_data$URLLength)
mode_url_length_legit <- calculate_mode(legit_url_df$URLLength)
mode_url_length_phishing <- calculate_mode(phishing_url_df$URLLength)

# Calcolo delle statistiche per NoOfObfuscatedChar
mean_obfuscated_char <- mean(filtered_data$NoOfObfuscatedChar, na.rm = TRUE)
median_obfuscated_char <- median(filtered_data$NoOfObfuscatedChar, na.rm = TRUE)
min_obfuscated_char <- min(filtered_data$NoOfObfuscatedChar, na.rm = TRUE)
max_obfuscated_char <- max(filtered_data$NoOfObfuscatedChar, na.rm = TRUE)
mode_obfuscated_char <- calculate_mode(filtered_data$NoOfObfuscatedChar)


# Calcolo delle statistiche per NoOfObfuscatedChar
mean_obfuscated_char <- mean(filtered_data$NoOfObfuscatedChar, na.rm = TRUE)
median_obfuscated_char <- median(filtered_data$NoOfObfuscatedChar, na.rm = TRUE)
min_obfuscated_char <- min(filtered_data$NoOfObfuscatedChar, na.rm = TRUE)
max_obfuscated_char <- max(filtered_data$NoOfObfuscatedChar, na.rm = TRUE)
mode_obfuscated_char <- calculate_mode(filtered_data$NoOfObfuscatedChar)
# Stampa dei risultati
cat("Statistiche per URL con HasObfuscation = 1:\n")

cat("\nURLLength:\n")
cat("Media:", mean_url_length, "\n")
cat("Moda:", mode_url_length, "\n")
cat("Mediana:", median_url_length, "\n")
cat("Minimo:", min_url_length, "\n")
cat("Massimo:", max_url_length, "\n")

cat("\nNoOfObfuscatedChar:\n")
cat("Media:", mean_obfuscated_char, "\n")
cat("Moda:", mode_obfuscated_char, "\n")
cat("Mediana:", median_obfuscated_char, "\n")
cat("Minimo:", min_obfuscated_char, "\n")
cat("Massimo:", max_obfuscated_char, "\n")

cat("Statistiche per URL generali:\n")
cat("\nURLLength legit:\n")
cat("Media:", mean_url_length_legit, "\n")
cat("Moda:", mode_url_length_legit, "\n")
cat("Mediana:", median_url_length_legit, "\n")
cat("Minimo:", min_url_length_legit, "\n")
cat("Massimo:", max_url_length_legit, "\n")

cat("\nURLLength phishing:\n")
cat("Media:", mean_url_length_phishing, "\n")
cat("Moda:", mode_url_length_phishing, "\n")
cat("Mediana:", median_url_length_phishing, "\n")
cat("Minimo:", min_url_length_phishing, "\n")
cat("Massimo:", max_url_length_phishing, "\n")
      