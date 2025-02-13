library(ggplot2)
library(dplyr)

# Numero totale di TLD unici
totale_TLD <- 569  

# Dati basati sulla tabella
fasce_data <- data.frame(
  Fascia = c("0-10%", "10-20%", "20-30%", "30-40%", "40-50%", 
             "50-60%", "60-70%", "70-80%", "80-90%", "90-100%"),
  NumeroTLD = c(165, 42, 47, 29, 15, 50, 24, 26, 31, 140)
)

# Calcolo delle frequenze relative
fasce_data <- fasce_data %>%
  mutate(FrequenzaRelativa = (NumeroTLD / totale_TLD) * 100) %>%
  arrange(desc(FrequenzaRelativa))  # Ordiniamo le barre in ordine decrescente

# Calcolo della frequenza cumulata (MANTENENDO L'ORDINE ORIGINALE!)
fasce_data$FrequenzaCumulata <- cumsum(fasce_data$FrequenzaRelativa)

# Creazione del grafico di Pareto corretto
print(ggplot(fasce_data, aes(x = reorder(Fascia, -FrequenzaRelativa))) +
  geom_bar(aes(y = FrequenzaRelativa), stat = "identity", fill = "steelblue", color = "black") +  # Barre ordinate
  geom_line(aes(x = Fascia, y = FrequenzaCumulata, group = 1), color = "red", size = 1) +  # Linea corretta
  geom_point(aes(x = Fascia, y = FrequenzaCumulata), color = "red", size = 3) +  # Punti sulla linea cumulata
  scale_y_continuous(
    name = "Frequenza Relativa (%)", 
    limits = c(0, 100),  
    sec.axis = sec_axis(~ ., name = "Percentuale Cumulata (%)")  # Asse secondario per la cumulata
  ) +
  labs(
    title = "Diagramma di Pareto delle fasce percentuali di URL di Phishing",
    x = "Fasce Percentuali di URL di Phishing nei TLD"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
    axis.text.y = element_text(size = 14),
    axis.title = element_text(size = 15),
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold")
  )
)