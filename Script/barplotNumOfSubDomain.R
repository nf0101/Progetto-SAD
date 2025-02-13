library(ggplot2)
library(dplyr)
library(tidyr)

# Creazione delle occorrenze per ogni valore di NoOfSubDomain e label
occurrences_df <- combined_dataset %>%
  group_by(NoOfSubDomain, label) %>%
  summarise(n = n(), .groups = 'drop') %>%
  mutate(label = factor(label, levels = c(1, 0), labels = c("Legittimi", "Phishing")))

# Creiamo tutte le combinazioni possibili di NoOfSubDomain e label
all_combinations <- expand.grid(NoOfSubDomain = unique(combined_dataset$NoOfSubDomain), 
                                label = c("Legittimi", "Phishing"))

# Uniamo i dati con le combinazioni per riempire i valori mancanti con 0
occurrences_df <- full_join(all_combinations, occurrences_df, by = c("NoOfSubDomain", "label")) %>%
  mutate(n = ifelse(is.na(n), 0, n))  # Riempiamo i valori mancanti con 0

occurrences_df$NoOfSubDomain <- factor(occurrences_df$NoOfSubDomain)

# Creazione del grafico
print(ggplot(occurrences_df, aes(x = NoOfSubDomain, y = n, fill = label)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.5) +
  geom_text(aes(label = ifelse(n > 0, n, "")),  # Mostra il numero solo se maggiore di 0
            position = position_dodge(width = 0.5), vjust = -0.5, size = 3) +  
  scale_y_continuous(
    trans = scales::pseudo_log_trans(base = 10),  
    breaks = c(0, 10, 100, 1000, 10000, 100000),       
    labels = c("0", "10", "100", "1000", "10000", "100000") 
  ) +
  scale_fill_manual(values = c("Legittimi" = "green", "Phishing" = "red")) +
  labs(
    title = "Distribuzione di NoOfSubDomain nei siti di phishing e legittimi",
    x = "Numero di Subdomain",
    y = "Occorrenze",
    fill = "Tipo di URL"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
)