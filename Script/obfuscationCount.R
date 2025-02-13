library(dplyr)
offuscation_stats <- combined_dataset %>%
  group_by(label) %>%
  summarise(
    TotalURLs = n(),
    ObfuscatedURLs = sum(HasObfuscation == 1),
    ObfuscationRate = (ObfuscatedURLs / TotalURLs) * 100
  )
print(offuscation_stats)
obfuscation_chars <- combined_dataset %>%
  group_by(label) %>%
  summarise(
    MeanObfuscatedChar = mean(NoOfObfuscatedChar, na.rm = TRUE),
    MedianObfuscatedChar = median(NoOfObfuscatedChar, na.rm = TRUE),
    MaxObfuscatedChar = max(NoOfObfuscatedChar, na.rm = TRUE)
  )
print(obfuscation_chars)

