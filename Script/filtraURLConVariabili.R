# Filtrare gli URL di phishing con almeno un '=' e almeno un '&'
phishing_subset <- phishing_data[
  phishing_data$NoOfEqualsInURL >= 1 & phishing_data$NoOfAmpersandInURL >= 1 & phishing_data$NoOfDegitsInURL >= 1 & phishing_data$label == 0 & phishing_data$HasObfuscation == 1, 
]

# Contare il numero di URL filtrati
num_phishing_urls <- nrow(phishing_subset)

# Stampare il risultato
cat("Numero di URL di phishing con almeno un '=' e un '&':", num_phishing_urls, "\n")
