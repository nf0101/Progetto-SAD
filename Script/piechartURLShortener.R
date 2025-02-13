library(ggplot2)
library(dplyr)
library(ggrepel)

# Assumendo che result_table contenga la colonna ShortenerDomain
# Calcolo delle frequenze dei domini Shortener
domain_frequency <- result_table %>%
  group_by(ShortenerDomain) %>%
  summarise(Frequency = n(), .groups = 'drop') %>%
  arrange(desc(Frequency))

# Somma totale di tutti gli URL shortener
total_urls <- sum(domain_frequency$Frequency)

# Creazione della categoria "Altro" per domini con pochi URL (es. < 1% del totale)
domain_frequency <- domain_frequency %>%
  mutate(
    Proportion = Frequency / total_urls,
    ShortenerDomain = ifelse(Proportion < 0.01, "Altro", ShortenerDomain) # Raggruppa i domini rari in "Altro"
  ) %>%
  group_by(ShortenerDomain) %>%
  summarise(Frequency = sum(Frequency), .groups = 'drop') %>%
  arrange(desc(Frequency))

# Aggiunta delle percentuali per il grafico
domain_frequency <- domain_frequency %>%
  mutate(
    Percentage = round(100 * Frequency / sum(Frequency), 2),
    ShortenerDomain = factor(ShortenerDomain, levels = ShortenerDomain) # Ordina i domini in ordine decrescente
  )

# Aggiunta di variabili per la posizione dell'etichetta
domain_frequency <- domain_frequency %>%
  mutate(
    csum = rev(cumsum(rev(Frequency))),
    pos = Frequency / 2 + lead(csum, 1),
    pos = if_else(is.na(pos), Frequency / 2, pos),
    Angle = 360 * csum - 180
  )

# Creazione del grafico a torta con etichette interne per fette più grandi e esterne per le piccole
print(ggplot(domain_frequency, aes(x = "", y = Frequency, fill = ShortenerDomain)) +
        geom_bar(stat = "identity", width = 1, color = "white") +
        coord_polar(theta = "y", start = 0) + # Inizia da zero per l'ordine corretto
        labs(
          title = "Distribuzione dei domini degli URL Shortener",
          fill = "Shortener Domain"
        ) +
        theme_void() +  # Rimuove gli elementi grafici inutili
        # Etichette interne per fette > 7%
        geom_text(
          aes(label = ifelse(Percentage > 7, paste0(Percentage, "%"), "")),
          position = position_stack(vjust = 0.5), size = 4, color = "white"
        ) +
        # Etichette esterne per fette <= 7%
        geom_text_repel(
          aes(
            y = pos,
            label = ifelse(Percentage <= 7, paste0(Percentage, "%"), ""),
            angle = 0
          ),
          size = 4,
          nudge_x = 0.7,         # Sposta le etichette esternamente
          segment.color = "grey50", # Linee di connessione
          segment.size = 0.5,
          force = 0.01,               # Aumenta la spinta per distanziare le etichette
          box.padding = 0.1,       # Aggiusta il padding della scatola
          direction = "y",      # Direzione della spinta
          show.legend = FALSE
        )
)
