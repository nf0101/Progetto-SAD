# Filtra il dataset per escludere gli URL con URLSimilarityIndex = 100
filtered_data <- combined_dataset %>%
  filter(URLSimilarityIndex != 100) %>%
  select(URLSimilarityIndex, NoOfObfuscatedChar, ObfuscationRatio, NoOfDegitsInURL, 
         DegitRatioInURL, NoOfEqualsInURL, NoOfQMarkInURL, NoOfAmpersandInURL, 
         NoOfOtherSpecialCharsInURL, SpacialCharRatioInURL, LineOfCode, NoOfImage, 
         NoOfCSS, NoOfJS, NoOfSelfRef, NoOfEmptyRef, NoOfExternalRef)

# Calcola la matrice di correlazione
cor_matrix <- cor(filtered_data, use = "complete.obs", method = "pearson")

# Stampa la matrice di correlazione
print(cor_matrix)

# Visualizza la matrice con una heatmap
library(ggcorrplot)
print(ggcorrplot(cor_matrix, lab = TRUE, type = "lower", outline.col = "white", 
           colors = c("blue", "white", "red"), title = "Matrice di Correlazione")
)