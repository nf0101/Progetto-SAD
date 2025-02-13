library(ggplot2)

# Filtra gli URL offuscati
url_offuscati <- combined_dataset[combined_dataset$HasObfuscation == 1, ]

# Definisci soglie di lunghezza
breaks <- c(0, 50, 100, 500, 1000, 5000)
labels <- c("0-50", "51-100", "101-500", "501-1000", "1001-5000")

# Tabella di frequenza
url_length_categories <- cut(url_offuscati$URLLength, breaks = breaks, labels = labels, right = FALSE)
tabella_frequenza <- table(url_length_categories)

# Stampa la tabella
print(tabella_frequenza)

# Converti in dataframe per ggplot
frequenza_df <- as.data.frame(tabella_frequenza)
colnames(frequenza_df) <- c("Categoria", "Frequenza")

# Crea il grafico con i valori sulle barre
print(ggplot(frequenza_df, aes(x = Categoria, y = Frequenza)) +
        geom_bar(stat = "identity", fill = "skyblue", width = 0.8) +
        geom_text(aes(label = Frequenza), vjust = -0.5, size = 4.5, color = "black") + # Aggiunge i valori
        labs(
          title = "Distribuzione della lunghezza degli URL offuscati",
          x = "Categoria di lunghezza",
          
          y = "Frequenza"
        ) +
        
        theme_minimal() +
      theme(
        axis.text.x = element_text(size = 14),  # Dimensione etichette asse X
        axis.text.y = element_text(size = 14),  # Dimensione etichette asse Y
        axis.title.x = element_text(size = 15), # Dimensione titolo asse X
        axis.title.y = element_text(size = 15)  # Dimensione titolo asse Y
      )
)
