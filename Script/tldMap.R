library(sf)
library(ggplot2)

# Converti il mondo in formato sf
world_map_sf <- st_as_sf(world_map)

# Plotta con ggplot2
print(ggplot(world_map_sf) +
        # Layer con i paesi
        geom_sf(aes(fill = PhishingPercentage), color = "black") +
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
        
        # Tema e personalizzazioni
        theme(
          panel.background = element_rect(fill = "lightblue"), # Oceano azzurro
          panel.grid = element_blank(),
          plot.margin = margin(0, 0, 0, 0, "cm"),
          legend.position = "bottom",
          legend.title = element_text(size = 14),
          legend.text = element_text(size = 12)
        ) +
        labs(
          caption = "I paesi con meno di 10 URL totali nel dataset sono semi-trasparenti.\nI paesi senza dati sono in bianco."
        )
)