# Carica i pacchetti necessari
library(ggplot2)
library(reshape2)  # Per riorganizzare i dati
library(corrplot)  # Per una heatmap avanzata

# Seleziona le colonne per la matrice di correlazione
variabili <- combined_dataset[, c(
  "URLLength", "DomainLength", "NoOfSubDomain", "NoOfObfuscatedChar",
  "ObfuscationRatio", "NoOfLettersInURL", "LetterRatioInURL",
  "NoOfDegitsInURL", "DegitRatioInURL", "NoOfEqualsInURL",
  "NoOfQMarkInURL", "NoOfAmpersandInURL", "NoOfOtherSpecialCharsInURL",
  "SpacialCharRatioInURL"
)]

# Calcola la matrice di correlazione
cor_matrix <- cor(variabili, use = "complete.obs")

# Visualizzazione base con ggplot2
cor_data <- melt(cor_matrix)  # Trasforma la matrice in un formato lungo
colnames(cor_data) <- c("Var1", "Var2", "Correlation")  # Rinomina le colonne

print(ggplot(cor_data_phishing, aes(x = Var1, y = Var2, fill = Correlation)) +
        geom_tile() +
        scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, 
                             limit = c(-1, 1), space = "Lab", name = "Correlazione") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        labs(
          title = "Matrice di Correlazione (Solo Phishing)",
          x = "Variabili",
          y = "Variabili"
        )
)
# Visualizzazione avanzata con corrplot
corrplot(cor_matrix, method = "circle",
         type = "full",  tl.col = "black", tl.srt = 45,
         title = "Matrice di Correlazione", mar = c(0, 0, 1, 0))
