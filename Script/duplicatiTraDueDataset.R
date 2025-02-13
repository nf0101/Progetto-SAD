library(dplyr)

# Funzione per trovare le righe duplicate
find_duplicates <- function(df) {
  duplicate_rows <- df %>%
    group_by(across(everything())) %>%  # Raggruppa per tutte le colonne
    filter(n() > 1) %>%                 # Trova le righe che appaiono più di una volta
    ungroup()
  
  return(duplicate_rows)
}

# Trova le righe duplicate nel dataset synthetic_dataframe_context
duplicate_rows_df <- find_duplicates(synthetic_dataframe_context)

# Conta e stampa il numero totale di righe duplicate
num_duplicates <- nrow(duplicate_rows_df)
cat("Numero totale di righe duplicate:", num_duplicates, "\n")

# Visualizza le prime 5 righe duplicate, se presenti
if (num_duplicates > 0) {
  cat("\nEsempi di righe duplicate:\n")
  print(head(duplicate_rows_df, 5))
}
