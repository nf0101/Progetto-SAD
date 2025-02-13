print(ggplot(phishing_entries_withTitle, aes(x = "", y = ScoreDifference)) +
  geom_violin(trim = TRUE, fill = "red", alpha = 0.7) + # Grafico a violino
  # Aggiungi la mediana come rombo bianco
  stat_summary(fun = median, geom = "point", shape = 23, size = 3, fill = "white", color = "black") +
  # Aggiungi le linee per il primo e terzo quartile
  stat_summary(
    fun.min = function(y) quantile(y, 0.25),
    fun.max = function(y) quantile(y, 0.75),
    fun = median, # Linea centrale opzionale
    geom = "errorbar",
    width = 0.05,
    color = "black"
  ) +
  labs(
    title = "Distribuzione della differenza tra URLTitleMatchScore e DomainTitleMatchScore",
    x = "",
    y = "Differenza (ScoreDifference)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_blank(),        # Nasconde l'etichetta sull'asse X
    axis.ticks.x = element_blank(),       # Nasconde i tick sull'asse X
    axis.text.y = element_text(size = 12), # Dimensione etichette sull'asse Y
    axis.title.y = element_text(size = 14), # Dimensione titolo asse Y
    plot.title = element_text(hjust = 0.5, size = 16) # Centra e dimensiona il titolo
  ))