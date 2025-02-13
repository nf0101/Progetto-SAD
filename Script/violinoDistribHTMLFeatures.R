no_of_empty_ref_data <- combined_dataset %>%
  select(label, URLSimilarityIndex) %>%
  mutate(label = factor(label, labels = c("Phishing", "Legittimi"))) %>%
  filter(label == "Phishing")  # Mantieni solo i dati di phishing

print(ggplot(no_of_empty_ref_data, aes(x = label, y = URLSimilarityIndex, fill = label)) +
        geom_violin(alpha = 0.7, color = "black", scale = "count", trim = TRUE) + # Grafico a violino
        geom_boxplot(width = 0.07, outlier.color = "black", outlier.size = 1, 
                     alpha = 0.8, outlier.alpha = 0.4, outlier.shape = 1, staplewidth = 1) + # Boxplot incorporato
        scale_fill_manual(values = c("red")) +  # Mantieni solo il colore per phishing
        labs(
          title = "Distribuzione di URLSimilarityIndex per siti di Phishing",
          x = "Tipo di URL",
          y = "URL Similarity Index"
        ) +
        theme_minimal() +
        theme(
          legend.position = "none", # Nasconde la legenda
          axis.text = element_text(size = 15),
          axis.title = element_text(size = 16),
          plot.title = element_text(size = 17, hjust = 0.5)
        )
)
