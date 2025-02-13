library(dplyr)

# Funzione per normalizzare il dataset
normalize_dataset <- function(df) {
  df %>%
    mutate(across(where(is.character), ~ tolower(trimws(.)))) %>%  # Normalizza le colonne di tipo carattere
    mutate(across(where(is.numeric), ~ round(., digits = 6)))      # Arrotonda le colonne numeriche a 6 cifre decimali
}

# Funzione per contare le righe identiche tra due dataset normalizzati e salvarle in un dataframe
count_equal_rows <- function(df1, df2) {
  df1_norm <- normalize_dataset(df1)
  df2_norm <- normalize_dataset(df2)
  
  # Trova le righe comuni
  common_rows <- semi_join(df1_norm, df2_norm, by = names(df1_norm))
  
  # Conta il numero di righe uguali
  num_equal_rows <- nrow(common_rows)
  
  # Salva le righe uguali in un dataframe
  common_rows_df <- common_rows
  
  return(list(count = num_equal_rows, rows = common_rows_df))
}

# Esegui la funzione per confrontare due dataset
result <- count_equal_rows(combined_dataset, synthetic_dataframe_context)

cat("Numero di righe uguali tra i due dataset:", result$count, "\n")

# Salva le righe uguali in un dataframe a parte
common_rows_df <- result$rows

# Visualizza le prime 5 righe comuni, se presenti
if (nrow(common_rows_df) > 0) {
  cat("\nEsempi di righe uguali:\n")
  print(head(common_rows_df, 5))
}

