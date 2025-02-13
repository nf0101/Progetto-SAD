# Creazione del dataset per NoOfURLRedirect
no_of_url_redirect_data <- combined_dataset %>%
  select(label, NoOfURLRedirect) %>%
  mutate(label = factor(label, labels = c("Phishing", "Legittimi")))

# Grafico a barre
print(ggplot(no_of_url_redirect_data, aes(x = factor(NoOfURLRedirect), fill = label)) +
        geom_bar(position = "dodge", color = "black", alpha = 0.8, width = 0.6) +
        scale_fill_manual(values = c("red", "green")) +
        scale_y_continuous(labels = scales::comma) + # Formattazione dell'asse y in notazione normale
        labs(
          title = "Distribuzione di NoOfURLRedirect per siti Legittimi e Phishing",
          x = "Valore di NoOfURLRedirect",
          y = "Conteggio",
          fill = "Tipo di URL"
        ) +
        theme_minimal() +
        theme(
          legend.position = "top",
          legend.text =  element_text(size = 15),
          legend.title =  element_text(size = 15),
          axis.text = element_text(size = 15),
          axis.title = element_text(size = 15),
          plot.title = element_text(size = 17, hjust = 0.5)
        )
)
