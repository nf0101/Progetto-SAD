library(dplyr)

# Funzione per normalizzare il dataset
normalize_dataset <- function(df) {
  df %>%
    mutate(across(where(is.character), ~ tolower(trimws(.)))) %>%  # Normalizza le colonne di tipo carattere
    mutate(across(where(is.numeric), ~ round(., digits = 6)))      # Arrotonda le colonne numeriche a 6 cifre decimali
}

# Funzione per contare le righe identiche tra due dataset normalizzati mantenendo i duplicati
count_equal_rows_with_duplicates <- function(df1, df2) {
  df1_norm <- normalize_dataset(df1)
  df2_norm <- normalize_dataset(df2)
  
  # Trova le righe comuni, mantenendo eventuali duplicati
  common_rows <- inner_join(df1_norm, df2_norm, by = names(df1_norm))
  
  # Conta il numero di righe uguali
  num_equal_rows <- nrow(common_rows)
  
  return(list(count = num_equal_rows, rows = common_rows))
}

# Esegui la funzione per confrontare due dataset
result <- count_equal_rows_with_duplicates(combined_dataset, synthetic_dataframe_context)

cat("Numero di righe uguali tra i due dataset (inclusi duplicati):", result$count, "\n")

# Salva le righe comuni (inclusi i duplicati) in un dataframe separato
common_rows_with_duplicates_df <- result$rows

# Visualizza le prime 5 righe comuni, se presenti
if (nrow(common_rows_with_duplicates_df) > 0) {
  cat("\nEsempi di righe uguali (inclusi duplicati):\n")
  print(head(common_rows_with_duplicates_df, 5))
}
