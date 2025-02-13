library(dplyr)
library(ggplot2)

# Filtra i dati per mantenere solo i valori validi (0-100)
filtered_dataset <- combined_dataset %>%
  filter(HasTitle == 1)

# Aggiungi una colonna per la leggibilità
filtered_dataset <- filtered_dataset %>%
  mutate(Type = ifelse(label == 0, "Phishing", "Legittimo"))
print(length(filtered_dataset$DomainTitleMatchScore))
# Crea il grafico a violino con mediana e quartili
print(ggplot(filtered_dataset, aes(x = Type, y = DomainTitleMatchScore, fill = Type)) +
        geom_violin(trim = FALSE, width = 0.8, alpha = 0.7) + # Grafico a violino
        scale_fill_manual(values = c("Phishing" = "red", "Legittimo" = "green")) +
        # Aggiungi mediana come punto
        stat_summary(
          fun = median,
          geom = "point",
          shape = 23, # Punto a forma di rombo
          size = 3,
          fill = "white",
          color = "black"
        ) +
        # Aggiungi primo e terzo quartile come barre di errore
        stat_summary(
          fun.min = function(y) quantile(y, 0.25),
          fun.max = function(y) quantile(y, 0.75),
          fun = median,
          geom = "errorbar",
          width = 0.2,
          color = "black"
        ) +
        labs(
          title = "Confronto del DomainTitleMatchScore tra URL legittimi e di phishing",
          x = "Tipo di URL",
          y = "DomainTitleMatchScore",
          fill = "Tipo di URL"
        ) +
        
        theme_minimal() +
        theme(
          axis.text.x = element_text(size = 15),  # Dimensione etichette asse X
          axis.text.y = element_text(size = 15),  # Dimensione etichette asse Y
          axis.title.x = element_text(size = 17), # Dimensione titolo asse X
          axis.title.y = element_text(size = 17), # Dimensione titolo asse Y
          legend.text = element_text(size = 14),
          legend.title = element_text(size = 15)
        )
)
