# Filtra solo righe con valori validi nelle colonne necessarie e verifica che abbiano variabilità
filtered_dataset <- combined_dataset %>%
  filter(!is.na(DomainTitleMatchScore) & !is.na(URLTitleMatchScore)) %>%
  filter(var(DomainTitleMatchScore) > 0 & var(URLTitleMatchScore) > 0)

# Se i dati sono sufficienti, crea il grafico; altrimenti segnala il problema
if (nrow(filtered_dataset) > 1) {
  density_plot <- ggplot(filtered_dataset, aes(x = DomainTitleMatchScore, y = URLTitleMatchScore, fill = label)) +
    geom_density_2d(aes(color = label)) +
    geom_density_2d_filled(alpha = 0.5) +
    labs(title = "Densità di DomainTitleMatchScore e URLTitleMatchScore per Tipo di Sito",
         x = "Domain Title Match Score",
         y = "URL Title Match Score",
         fill = "Tipo di Sito") +
    scale_fill_manual(values = c("Phishing" = "red", "Legittimo" = "green")) +
    scale_color_manual(values = c("Phishing" = "red", "Legittimo" = "green")) +
    theme_minimal()
  
  # Stampa il grafico
  print(density_plot)
} else {
  message("I dati non hanno variabilità sufficiente per un grafico a densità bidimensionale.")
}
