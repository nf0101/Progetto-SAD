library(ggplot2)
library(dplyr)

# Dati basati sulla tabella
fasce_data <- data.frame(
  Fascia = c("0-10%", "10-20%", "20-30%", "30-40%", "40-50%", 
             "50-60%", "60-70%", "70-80%", "80-90%", "90-100%"),
  NumeroTLD = c(165, 42, 47, 29, 15, 50, 24, 26, 31, 140)
)

# Aggiungi un punto iniziale per la funzione di ripartizione (0, 0)
fasce_data <- fasce_data %>%
  mutate(
    FrequenzaCumulata = cumsum(NumeroTLD),          # Frequenze cumulate
    PercentualeCumulata = FrequenzaCumulata / sum(NumeroTLD) * 100  # Percentuale cumulata
  ) %>%
  add_row(Fascia = "", NumeroTLD = 0, FrequenzaCumulata = 0, PercentualeCumulata = 0, .before = 1)

# Creazione del grafico della funzione di ripartizione
# Filtra i dati per escludere il punto iniziale (0, 0)
fasce_data_filtered <- fasce_data %>% filter(PercentualeCumulata > 0)

# Creazione del grafico senza il punto e la label al valore 0
print(ggplot(fasce_data, aes(x = reorder(Fascia, FrequenzaCumulata), y = PercentualeCumulata, group = 1)) +
        geom_line(color = "red", size = 1) +            # Linea della ripartizione
        geom_point(data = fasce_data_filtered, color = "red", size = 3) +  # Punti senza (0, 0)
        geom_text(
          data = fasce_data_filtered, 
          aes(label = paste0(round(PercentualeCumulata, 2), "%")), 
          vjust = -1.2, size = 4
        ) +                                            # Etichette senza (0, 0)
        labs(
          title = "Funzione di Distribuzione Empirica per Fasce di Percentuale di Phishing",
          x = "Fasce Percentuali di URL di Phishing",
          y = "Percentuale Cumulata"
        ) +
        theme_minimal() +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
          axis.title = element_text(size = 14),
          plot.title = element_text(hjust = 0.5, size = 16, face = "bold")
        )
)


# Riduci le dimensioni del dispositivo grafico (opzionale, per garantire la leggibilità)
# ggsave("funzione_di_ripartizione_empirica.png", width = 10, height = 6, dpi = 300)
