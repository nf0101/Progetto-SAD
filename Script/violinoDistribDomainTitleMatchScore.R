library(ggplot2)
library(dplyr)

# Filtrare solo i dati con HasTitle = 1
filtered_dataset <- combined_dataset %>% filter(HasTitle == 1)

# Creare il grafico a violino con boxplot sovrapposto
print(ggplot(filtered_dataset, aes(x = as.factor(label), y = DomainTitleMatchScore, fill = as.factor(label))) +
        geom_violin(alpha = 0.6) +  # Grafico a violino con trasparenza
        geom_boxplot(width = 0.03, outlier.color = "black", outlier.size = 1, alpha = 0.4, 
                     outlier.alpha = 0.4, outlier.shape = 1, staplewidth = 1) +  # Boxplot più sottile
        scale_fill_manual(values = c("red", "green"), labels = c("Phishing", "Legittimo")) +
        labs(
          title = "Distribuzione di DomainTitleMatchScore per URL con Titolo",
          x = "Tipo di URL",
          y = "DomainTitleMatchScore",
          fill = "Tipo di URL"
        ) +
        theme_minimal() +
        theme(
          legend.text = element_text(size = 15),
          legend.title = element_text(size = 15),
          axis.text.x = element_blank(),
          axis.text = element_text(size = 15),
          axis.title = element_text(size = 16),
          plot.title = element_text(size = 16, hjust = 0.5)
        )
)
