library(dplyr)

# Funzione che processa un singolo dataset e restituisce le metriche in una lista
process_dataset <- function(dataset, dataset_name) {
  
  # Verifica che le colonne "URL" e "TLD" esistano
  if (!"URL" %in% colnames(dataset)) {
    stop("La colonna 'URL' non è presente nel dataset: ", dataset_name)
  }
  if (!"TLD" %in% colnames(dataset)) {
    stop("La colonna 'TLD' non è presente nel dataset: ", dataset_name)
  }
  
  # Calcola il numero totale di URL
  total_urls <- nrow(dataset)
  
  # Conta gli URL che usano un indirizzo IP (invece di un dominio)
  ip_count <- sum(
    grepl("^http://(\\d{1,3}\\.){3}\\d{1,3}([:/?]|$)", dataset$URL) |
      grepl("^https://(\\d{1,3}\\.){3}\\d{1,3}([:/?]|$)", dataset$URL)
  )
  ip_percent <- round((ip_count / total_urls) * 100, 2)
  
  # Conta i TLD che contengono una porta (ovvero che hanno un ':' nel TLD)
  port_count <- sum(grepl(":.*$", dataset$TLD))
  port_percent <- round((port_count / total_urls) * 100, 2)
  
  cat("\n=== ", dataset_name, " ===\n", sep = "")
  cat("Numero totale di URL: ", total_urls, "\n", sep = "")
  cat("URL con indirizzo IP: ", ip_count, " (", ip_percent, "%)\n", sep = "")
  cat("URL che presentano una porta nel TLD: ", port_count, " (", port_percent, "%)\n", sep = "")
  
  # Rimuove gli URL basati su IP
  dataset_clean <- dataset %>%
    filter(!grepl("^http://(\\d{1,3}\\.){3}\\d{1,3}([:/?]|$)", URL)) %>%
    filter(!grepl("^https://(\\d{1,3}\\.){3}\\d{1,3}([:/?]|$)", URL))
  
  # Rimuove le porte dalla colonna TLD (tutto ciò che segue il ':')
  dataset_clean <- dataset_clean %>%
    mutate(TLD = sub(":.*$", "", TLD))
  
  # Numero di TLD unici
  unique_tld_count <- n_distinct(dataset_clean$TLD)
  cat("Numero totale di TLD unici (senza porte): ", unique_tld_count, "\n", sep = "")
  
  # Calcola il conteggio dei TLD
  tld_counts <- dataset_clean %>%
    group_by(TLD) %>%
    summarise(Conteggio = n(), .groups = 'drop')
  
  # Trova i 5 TLD più frequenti
  top_5_tlds <- tld_counts %>%
    arrange(desc(Conteggio)) %>%
    slice_head(n = 5)
  
  cat("\nI 5 TLD più frequenti sono:\n")
  apply(top_5_tlds, 1, function(row) {
    cat(sprintf("{%s: %d}\n", row["TLD"], as.integer(row["Conteggio"])))
  })
  
  total_top5 <- sum(top_5_tlds$Conteggio)
  cat("\nNumero totale di URL nei 5 TLD più frequenti: ", total_top5, "\n", sep = "")
  
  # Restituisci le metriche in una lista
  return(list(
    dataset_name = dataset_name,
    total_urls = total_urls,
    ip_count = ip_count,
    ip_percent = ip_percent,
    port_count = port_count,
    port_percent = port_percent,
    unique_tld_count = unique_tld_count,
    total_top5 = total_top5,
    dataset_clean = dataset_clean
  ))
}

# Processa i tre dataset
result_original <- process_dataset(combined_dataset, "Dataset Originale")
result_synthetic <- process_dataset(synthetic_dataframe, "Synthetic Dataset")
result_context   <- process_dataset(synthetic_dataframe_context, "Synthetic Context Dataset")

# Crea un data frame di riepilogo con il confronto per ogni dataset
summary_df <- data.frame(
  Dataset           = c(result_original$dataset_name, result_synthetic$dataset_name, result_context$dataset_name),
  Total_URLs        = c(result_original$total_urls, result_synthetic$total_urls, result_context$total_urls),
  IP_Count          = c(result_original$ip_count, result_synthetic$ip_count, result_context$ip_count),
  IP_Percent        = c(result_original$ip_percent, result_synthetic$ip_percent, result_context$ip_percent),
  Port_Count        = c(result_original$port_count, result_synthetic$port_count, result_context$port_count),
  Port_Percent      = c(result_original$port_percent, result_synthetic$port_percent, result_context$port_percent),
  Unique_TLD_Count  = c(result_original$unique_tld_count, result_synthetic$unique_tld_count, result_context$unique_tld_count),
  Top5_URLs         = c(result_original$total_top5, result_synthetic$total_top5, result_context$total_top5)
)

cat("\n--- Riepilogo per dataset ---\n")
print(summary_df)
