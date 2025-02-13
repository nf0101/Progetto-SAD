library(ggplot2)
library(dplyr)
library(ggrepel)

# Calcolo delle frequenze degli URL per ciascun TLD
tld_frequency <- combined_dataset_clean %>%
  group_by(TLD) %>%
  summarise(Frequency = n(), .groups = 'drop') %>%
  arrange(desc(Frequency))

# Somma totale di tutti gli URL
total_urls <- sum(tld_frequency$Frequency)

# Creazione della categoria "Altro" per TLD con pochi URL (es. < 1% del totale)
tld_frequency <- tld_frequency %>%
  mutate(
    Proportion = Frequency / total_urls,
    TLD = ifelse(Proportion < 0.00791, "Altro", TLD) # Raggruppa i TLD rari in "Altro"
  ) %>%
  group_by(TLD) %>%
  summarise(Frequency = sum(Frequency), .groups = 'drop') %>%
  arrange(desc(Frequency))

# Aggiunta delle percentuali per il grafico
tld_frequency <- tld_frequency %>%
  mutate(
    Percentage = round(100 * Frequency / sum(Frequency), 2),
    TLD = factor(TLD, levels = TLD) # Ordina i TLD in ordine decrescente
  )

# Aggiunta di variabili per la posizione dell'etichetta
tld_frequency <- tld_frequency %>%

mutate(csum = rev(cumsum(rev(Frequency))), 
       pos = Frequency/2 + lead(csum, 1),
       pos = if_else(is.na(pos), Frequency/2, pos),
      Angle = 360 * csum - 180 )

# Creazione del grafico a torta con etichette interne per fette più grandi e esterne per le piccole
print(ggplot(tld_frequency, aes(x = "", y = Frequency, fill = TLD)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar(theta = "y", start = 0) + # Inizia da zero per l'ordine corretto
  labs(
    title = "Distribuzione dei primi 15 TLD tra gli URL + Altro",
    fill = "TLD"
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

# Filtra solo gli URL di phishing
phishing_tld_frequency <- combined_dataset_clean %>%
  filter(label == 0) %>% # Considera solo gli URL di phishing
  group_by(TLD) %>%
  summarise(Frequency = n(), .groups = 'drop') %>%
  arrange(desc(Frequency))

# Somma totale degli URL di phishing
total_phishing_urls <- sum(phishing_tld_frequency$Frequency)

# Creazione della categoria "Altro" per TLD con pochi URL (es. < 1% del totale)
phishing_tld_frequency <- phishing_tld_frequency %>%
  mutate(
    Proportion = Frequency / total_phishing_urls,
    TLD = ifelse(Proportion < 0.009, "Altro", TLD) # Raggruppa i TLD rari in "Altro"
  ) %>%
  group_by(TLD) %>%
  summarise(Frequency = sum(Frequency), .groups = 'drop') %>%
  arrange(desc(Frequency))

# Aggiunta delle percentuali per il grafico
phishing_tld_frequency <- phishing_tld_frequency %>%
  mutate(
    Percentage = round(100 * Frequency / sum(Frequency), 2),
    TLD = factor(TLD, levels = TLD) # Ordina i TLD in ordine decrescente
  )

# Aggiunta di variabili per la posizione dell'etichetta
phishing_tld_frequency <- phishing_tld_frequency %>%
  mutate(
    csum = rev(cumsum(rev(Frequency))),
    pos = Frequency / 2 + lead(csum, 1),
    pos = if_else(is.na(pos), Frequency / 2, pos),
    Angle = 360 * csum - 180
  )

# Creazione del grafico a torta con etichette interne per fette più grandi e esterne per le piccole
print(ggplot(phishing_tld_frequency, aes(x = "", y = Frequency, fill = TLD)) +
        geom_bar(stat = "identity", width = 1, color = "white") +
        coord_polar(theta = "y", start = 0) + # Inizia da zero per l'ordine corretto
        labs(
          title = "Distribuzione dei TLD tra gli URL di Phishing",
          fill = "TLD"
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