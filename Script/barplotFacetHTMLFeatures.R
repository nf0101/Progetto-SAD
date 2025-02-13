# Caricamento delle librerie necessarie
library(ggplot2)
library(tidyr)

# Seleziona le colonne rilevanti e ristruttura il dataset in formato lungo
long_data <- combined_dataset %>%
  select(label, HasFavicon, Robots, IsResponsive, HasDescription, HasExternalFormSubmit, 
         HasSocialNet, HasSubmitButton, HasHiddenFields) %>%
  pivot_longer(cols = -label, names_to = "Feature", values_to = "Value")

# Creazione del grafico
print(ggplot(long_data, aes(x = as.factor(Value), fill = as.factor(label))) +
  geom_bar(position = "dodge") +
  facet_wrap(~ Feature, scales = "free_y") + # Un pannello per ogni variabile
  scale_fill_manual(values = c("red", "green"), labels = c("Phishing", "Legittimi")) +
  labs(
    title = "Confronto tra URL di Phishing e Legittimi per Variabili Binari",
    x = "Valore (0 = No, 1 = Sì)",
    y = "Conteggio",
    fill = "Tipo di URL"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 10, face = "bold"), # Stile per i titoli dei facet
    axis.text.x = element_text(angle = 45, hjust = 1)    # Rotazione delle etichette asse X
  )
)