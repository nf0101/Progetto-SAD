normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
#URLLength, NoOfSubDomain, CharContinuationRate, URLCharProb, NoOfOtherSpecialCharsInURL, removedSpacialCharRatioInURL, LineOfCode, 
#         NoOfImage, NoOfSelfRef, NoOfExternalRef, removedNoOfiFrame, NoOfCSS, removedNoOfJS
#HasCopyrightInfo, IsResponsive, HasDescription, HasSocialNet, IsHTTPS, HasTitle, label,  removedNoOfCSS, , removedNoOfExternalRef
# feature numeriche normalizzate --- removed NoOfiFrame, , SpacialCharRatioInURL,
numeric_features <- combined_dataset %>%
  select(URLLength, NoOfSubDomain, CharContinuationRate, URLCharProb, NoOfOtherSpecialCharsInURL, LineOfCode, 
         NoOfImage, NoOfSelfRef,LineOfCode, NoOfJS) %>%
  mutate(across(everything(), normalize))

# feature binarie
binary_features <- combined_dataset %>%
  select(label, HasCopyrightInfo,IsResponsive, HasDescription, HasSocialNet, IsHTTPS, HasTitle)

# combina dati
final_data <- cbind(numeric_features, binary_features)

# Controlla la struttura
str(final_data)
