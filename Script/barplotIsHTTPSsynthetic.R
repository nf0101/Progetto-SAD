library(dplyr)
library(ggplot2)

# 1. Aggiungi una colonna identificativa e seleziona solo le colonne utili per l'analisi
combined_dataset <- combined_dataset %>%
  mutate(Dataset = "Combined") %>%
  select(label, IsHTTPS, Dataset)

synthetic_dataframe <- synthetic_dataframe %>%
  mutate(Dataset = "Synthetic") %>%
  select(label, IsHTTPS, Dataset)

synthetic_dataframe_context <- synthetic_dataframe_context %>%
  mutate(Dataset = "Synthetic_Context") %>%
  select(label, IsHTTPS, Dataset)

# 2. Unisci i tre dataset in uno solo
all_data <- bind_rows(combined_dataset, synthetic_dataframe, synthetic_dataframe_context)

# 3. Aggiungi una nuova categoria per valori anomali
all_data <- all_data %>%
  mutate(
    HTTPS_Category = case_when(
      IsHTTPS == 1 ~ "HTTPS",
      IsHTTPS == 0 ~ "Non HTTPS",
      TRUE ~ "Valori Anomali"
    )
  )

# 4. Raggruppa per Dataset, label e HTTPS_Category e calcola i conteggi e la proporzione
https_summary <- all_data %>%
  group_by(Dataset, label, HTTPS_Category) %>%
  summarise(Count = n(), .groups = "drop") %>%
  group_by(Dataset, label) %>%
  mutate(
    TotalInCategory = sum(Count),
    Proportion = Count / TotalInCategory
  ) %>%
  ungroup()

# Aggiunge etichette più leggibili per il grafico
https_summary <- https_summary %>%
  mutate(
    Type = ifelse(label == 0, "Phishing", "Legittimo"),
    HTTPS_Status = factor(HTTPS_Category, levels = c("HTTPS", "Non HTTPS", "Valori Anomali"))
  )

# 5. Crea il grafico con faceting per ciascun dataset, usando colori specifici
print(ggplot(https_summary, aes(x = Type, y = Proportion, fill = HTTPS_Status)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.6), width = 0.6) +
  geom_text(
    aes(label = Count),
    position = position_dodge(width = 0.6),
    vjust = -0.5,
    size = 5
  ) +
  scale_fill_manual(
    values = c("HTTPS" = "coral", "Non HTTPS" = "lightsteelblue4", "Valori Anomali" = "red")
  ) +
  labs(
    title = "Proporzione di siti HTTPS, Non HTTPS e Valori Anomali per categoria e dataset",
    x = "Tipo di URL",
    y = "Proporzione",
    fill = "Stato HTTPS"
  ) +
  facet_wrap(~ Dataset) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 15),
    axis.text.y = element_text(size = 15),
    axis.title.x = element_text(size = 17),
    axis.title.y = element_text(size = 17),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 15)
  )
)