# Carica i pacchetti necessari
library(ggplot2)
library(reshape2)  # Per riorganizzare i dati
library(corrplot)  # Per una heatmap avanzata

# Filtra solo gli URL di phishing
phishing_data <- combined_dataset[combined_dataset$label == 1, ]

# Seleziona le colonne per la matrice di correlazione (solo phishing)
variabili_phishing <- phishing_data[, c(
  "URLLength", "DomainLength", "NoOfSubDomain",
  #"NoOfObfuscatedChar",
  #"ObfuscationRatio", 
  "NoOfLettersInURL", "LetterRatioInURL",
  "NoOfDegitsInURL", "DegitRatioInURL", 
  #"NoOfEqualsInURL",
  #"NoOfQMarkInURL", "NoOfAmpersandInURL",
  "NoOfOtherSpecialCharsInURL",
  "SpacialCharRatioInURL"
)]

# Calcola la matrice di correlazione
cor_matrix_phishing <- cor(variabili_phishing, use = "complete.obs")
corrplot(cor_matrix_phishing, method = "color",
        type = "full", tl.col = "black", tl.srt = 45,
        mar = c(0.1, 0.1, 0.1, 0.1), )
#mtext("Matrice di correlazione per URL di phishing", at=6, line=-2, cex=1.6)