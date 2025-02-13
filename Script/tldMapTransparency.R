library(ggplot2)

library(dplyr)
library(tidyr)
# Aggiungi una colonna per trasparenza in base al numero di URL
world_map_sf <- world_map_sf %>%
  mutate(
    Transparency = ifelse(TotalURLs < 10, 0.0, 1), # Trasparenza per paesi con meno di 10 URL
    PhishingPercentage = ifelse(is.na(TotalURLs), NA, PhishingPercentage) # Garantiamo che i paesi senza dati siano NA
  )

# Plotta con ggplot2
print(# Plotta con ggplot2
  ggplot(world_map_sf) +
    # Layer con i paesi
    geom_sf(aes(fill = PhishingPercentage, alpha = Transparency), color = "black") +
    # Scala per il gradiente di colore
    scale_fill_gradient(
      low = "green", 
      high = "red", 
      na.value = "white", # Paesi senza dati in bianco
      name = "Percentuale di\nURL di phishing", # A capo nel titolo
      guide = guide_colorbar(
        title.position = "top", # Posiziona il titolo sopra la barra
        title.hjust = 0.5, # Centra il titolo
        barwidth = unit(20, "lines"), # Larghezza della barra
        barheight = unit(0.5, "lines") # Altezza della barra
      )
    ) +
    # Scala per l'alpha
    scale_alpha_continuous(range = c(0.1, 1), guide = "none") + # Solo per trasparenza, senza legenda
    # Tema e personalizzazioni
    theme(
      panel.background = element_rect(fill = "lightblue"), # Oceano azzurro
      panel.grid = element_blank(),
      plot.margin = margin(0, 0, 0, 0, "cm"),
      legend.position = "top",
      legend.title = element_text(size = 14),
      legend.text = element_text(size = 12),
      plot.caption.position = "panel"
    ) +
    labs(
      caption = "I paesi con meno di 20 URL totali nel dataset sono semi-trasparenti.\nI paesi senza dati sono in bianco."
    )
  
  
)