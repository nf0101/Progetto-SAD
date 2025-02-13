library(dplyr)

# Funzione per ottenere 5 esempi di URL comuni
get_common_url_pairs <- function(synthetic_df, original_df, dataset_name, n_examples = 5) {
  # Seleziona gli URL del dataset sintetico che sono presenti anche nel dataset originale
  common <- synthetic_df %>%
    filter(URL %in% original_df$URL)
  
  # Prendi n_examples esempi (o tutti se ce ne sono meno di n_examples)
  common_sample <- common %>% slice_head(n = n_examples)
  
  # Poiché l'URL è uguale in entrambi i dataset, per chiarezza creiamo due colonne identiche
  common_sample <- common_sample %>%
    mutate(Original_URL = URL, Synthetic_URL = URL) %>%
    select(Original_URL, Synthetic_URL)
  
  cat("\nEsempi di URL comuni per", dataset_name, ":\n")
  print(common_sample)
  
  return(common_sample)
}

# Funzione per ottenere 5 esempi di Domain comuni (opzionale)
get_common_domain_pairs <- function(synthetic_df, original_df, dataset_name, n_examples = 5) {
  common <- synthetic_df %>%
    filter(Domain %in% original_df$Domain)
  
  common_sample <- common %>% slice_head(n = n_examples) %>%
    mutate(Original_Domain = Domain, Synthetic_Domain = Domain) %>%
    select(Original_Domain, Synthetic_Domain)
  
  cat("\nEsempi di Domain comuni per", dataset_name, ":\n")
  print(common_sample)
  
  return(common_sample)
}

# Applica la funzione per il primo dataset sintetico
common_urls_synth <- get_common_url_pairs(synthetic_dataframe, combined_dataset, "Synthetic Dataset", 5)
# (Opzionale) Esempi per i Domain comuni nel primo dataset sintetico:
common_domains_synth <- get_common_domain_pairs(synthetic_dataframe, combined_dataset, "Synthetic Dataset", 5)

# Applica la funzione per il secondo dataset sintetico
common_urls_context <- get_common_url_pairs(synthetic_dataframe_context, combined_dataset, "Synthetic Context Dataset", 5)
# (Opzionale) Esempi per i Domain comuni nel secondo dataset sintetico:
common_domains_context <- get_common_domain_pairs(synthetic_dataframe_context, combined_dataset, "Synthetic Context Dataset", 5)
