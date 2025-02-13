library(dplyr)
library(ggplot2)

# Raggruppa per `label` (0 = phishing, 1 = legittimo) e `IsHTTPS`, quindi calcola i totali
https_summary <- combined_dataset %>%
  group_by(label, IsHTTPS) %>%
  summarise(Count = n(), .groups = "drop") %>%
  group_by(label) %>% # Gruppo aggiuntivo per calcolare il totale per categoria
  mutate(
    TotalInCategory = sum(Count),               # Totale degli URL per ogni categoria (phishing o legittimo)
    Proportion = Count / TotalInCategory        # Proporzione rispetto alla categoria
  ) %>%
  ungroup()                                     # Rimuove il raggruppamento per chiarezza

# Aggiungi un'etichetta per la leggibilità
https_summary <- https_summary %>%
  mutate(
    Type = ifelse(label == 0, "Phishing", "Legittimo"),
    HTTPS_Status = ifelse(IsHTTPS == 1, "HTTPS", "Non HTTPS")
  )

# Stampa il risultato in formato tabellare
print(https_summary)

# Visualizzazione per categoria con un grafico
print(ggplot(https_summary, aes(x = Type, y = Proportion, fill = HTTPS_Status)) +
        geom_bar(stat = "identity", position = "dodge", width = 0.6) +
        geom_text(
          aes(
            label = paste0(Count) # Mostra il numero assoluto
          ),
          position = position_dodge(width = 0.6), 
          vjust = -0.5, # Posiziona il testo sopra la barra
          size = 5
        ) +
        labs(
          title = "Proporzione di siti HTTPS e non HTTPS per categoria",
          x = "Tipo di URL",
          y = "Proporzione",
          fill = "Stato HTTPS"
        ) +
        theme_minimal() +
        theme(
          axis.text.x = element_text(size = 15),  # Dimensione etichette asse X
          axis.text.y = element_text(size = 15),  # Dimensione etichette asse Y
          axis.title.x = element_text(size = 17), # Dimensione titolo asse X
          axis.title.y = element_text(size = 17),  # Dimensione titolo asse Y
          legend.text = element_text(size = 14),
          legend.title = element_text(size = 15)
        )
)
