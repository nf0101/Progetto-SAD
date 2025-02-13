# Caricamento delle librerie necessarie
library(ggplot2)
library(tidyr)
library(dplyr)

# Ristrutturazione del dataset in formato lungo
long_data <- combined_dataset %>%
  select(label, HasExternalFormSubmit, HasSubmitButton, HasHiddenFields, HasPasswordField, 
         Bank, Pay, Crypto) %>%
  pivot_longer(cols = -label, names_to = "Feature", values_to = "Value")

# Riorganizzazione dei livelli di 'interaction(label, Value)'
long_data <- long_data %>%
  mutate(LabelValue = interaction(label, Value, lex.order = TRUE)) %>%
  mutate(LabelValue = factor(LabelValue, 
                             levels = c("1.0", "1.1", "0.0", "0.1"), 
                             labels = c("Legittimi senza feature", 
                                        "Legittimi con feature", 
                                        "Phishing senza feature", 
                                        "Phishing con feature")))

# Creazione del grafico
print(ggplot(long_data, aes(x = Feature, fill = LabelValue)) +
        geom_bar(position = "dodge", aes(y = ..count..)) +
        scale_y_continuous(labels = scales::comma) + 
        scale_fill_manual(
          values = c("darkgreen", "green", "darkred", "red"), 
          labels = c("Legittimi senza feature", "Legittimi con feature", 
                     "Phishing senza feature", "Phishing con feature")
        ) +
        labs(
          title = "Confronto tra URL di Phishing e Legittimi per variabili HTML binarie (Valore 0/1)",
          x = "Feature",
          y = "Conteggio",
          fill = "Tipo di URL e Valore"
        ) +
        theme_minimal() +
        theme(
          axis.text.x = element_text(angle = 35, hjust = 1, size = 14), # Rotazione delle etichette asse X
          axis.text.y = element_text(size = 14),
          axis.title.x = element_text(size = 14),
          axis.title.y = element_text(size = 14),
          legend.text = element_text(size = 14),
          legend.position = "top"                            # Legenda in alto
        )
)
