library(dplyr)

# Funzione per calcolare numero di URL e percentuali phishing/legittimi
calcola_percentuali <- function(df, nome_dataset) {
  total <- nrow(df)
  phishing_count <- sum(df$label == 0)
  legit_count <- sum(df$label == 1)
  
  data.frame(
    Dataset = nome_dataset,
    `N# URL` = total,
    `Percentuale phishing (%)` = round(phishing_count / total * 100, 2),
    `Percentuale legittimi (%)` = round(legit_count / total * 100, 2)
  )
}

# Calcola i valori per ciascun dataset
tab_originale <- calcola_percentuali(combined_dataset, "Dataset Originale")
tab_sintetico <- calcola_percentuali(synthetic_dataframe, "Synthetic Dataset")
tab_sintetico_context <- calcola_percentuali(synthetic_dataframe_context, "Synthetic Context Dataset")

# Unisci i risultati in un'unica tabella
tabella_finale <- bind_rows(tab_originale, tab_sintetico, tab_sintetico_context)

# Visualizza la tabella
print(tabella_finale)
