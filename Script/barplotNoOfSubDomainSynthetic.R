library(ggplot2)
library(dplyr)
library(tidyr)

# Aggiungere una colonna per distinguere i dataset
combined_dataset$Dataset <- "Dataset Originale"
synthetic_dataframe$Dataset <- "Synthetic Dataset"
synthetic_dataframe_context$Dataset <- "Synthetic Context Dataset"

# Combinare i dataset
all_data <- bind_rows(combined_dataset, synthetic_dataframe, synthetic_dataframe_context)

# Raggruppare per TLDLength, label e Dataset
occurrences_df <- all_data %>%
  group_by(TLDLength, label, Dataset) %>%
  summarise(n = n(), .groups = 'drop') %>%
  mutate(label = factor(label, levels = c(1, 0), labels = c("Legittimi", "Phishing")))

# Creare tutte le combinazioni possibili di TLDLength, label e Dataset
all_combinations <- expand.grid(
  TLDLength = unique(all_data$TLDLength), 
  label = c("Legittimi", "Phishing"),
  Dataset = c("Dataset Originale", "Synthetic Dataset", "Synthetic Context Dataset")
)

# Unione con le combinazioni per riempire i valori mancanti con 0
occurrences_df <- full_join(all_combinations, occurrences_df, 
                            by = c("TLDLength", "label", "Dataset")) %>%
  mutate(n = ifelse(is.na(n), 0, n))

# Impostare l'ordine desiderato per il dataset
occurrences_df$Dataset <- factor(occurrences_df$Dataset, 
                                 levels = c("Dataset Originale", "Synthetic Dataset", "Synthetic Context Dataset"))

# Creare una nuova colonna per la legenda combinata e impostare l'ordine dei livelli
occurrences_df$Dataset_Label <- paste(occurrences_df$Dataset, occurrences_df$label, sep = " - ")
occurrences_df$Dataset_Label <- factor(occurrences_df$Dataset_Label, levels = c(
  "Dataset Originale - Legittimi",
  "Synthetic Dataset - Legittimi",
  "Synthetic Context Dataset - Legittimi",
  "Dataset Originale - Phishing",
  "Synthetic Dataset - Phishing",
  "Synthetic Context Dataset - Phishing"
))

# Calcolo del totale di ogni dataset e della percentuale
occurrences_df <- occurrences_df %>%
  group_by(Dataset) %>%
  mutate(Tot_Dataset = sum(n)) %>%
  ungroup() %>%
  mutate(perc = (n / Tot_Dataset) * 100)

# Creazione del grafico con percentuali
print(ggplot(occurrences_df, aes(x = as.factor(TLDLength), y = perc, 
                           fill = Dataset_Label, group = Dataset_Label)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.5) +
  geom_text(aes(
    label = ifelse(perc > 0, paste0(sprintf("%.3f", perc), "%"), "0,000%"),
    # Se perc è 0, posiziona l'etichetta ad un valore minimo (es. 1)
    y = ifelse(perc > 0, perc, 1)
  ), 
  position = position_dodge(width = 0.8), 
  vjust = 0.5, 
  hjust = -0.05,  
  size = 3.5, 
  angle = 90) +
  # Imposta l'asse delle y da 0 a 100
  scale_y_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, 20),
    labels = function(x) paste0(x, "%")
  ) +
  scale_fill_manual(
    limits = c(
      "Dataset Originale - Legittimi",
      "Synthetic Dataset - Legittimi",
      "Synthetic Context Dataset - Legittimi",
      "Dataset Originale - Phishing",
      "Synthetic Dataset - Phishing",
      "Synthetic Context Dataset - Phishing"
    ),
    values = c(
      "Dataset Originale - Legittimi" = "deepskyblue",
      "Synthetic Dataset - Legittimi" = "coral1",
      "Synthetic Context Dataset - Legittimi" = "lightgreen",
      "Dataset Originale - Phishing" = "blue",
      "Synthetic Dataset - Phishing" = "red",
      "Synthetic Context Dataset - Phishing" = "darkgreen"
    ),
    guide = guide_legend(
      ncol = 3,
      byrow = TRUE
    )
  ) +
  labs(
    title = "Confronto del Numero di Subdomain nei Tre Dataset (Percentuali)",
    x = "Numero di Subdomain",
    y = "Percentuale sul Totale del Dataset",
    fill = "Dataset e Tipo di URL"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, hjust = 0.5, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom",
    legend.text = element_text(size = 12)
  ) +
  coord_cartesian(clip = "off")
)