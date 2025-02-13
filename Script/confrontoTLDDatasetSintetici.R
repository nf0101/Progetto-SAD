library(dplyr)

# Funzione per ottenere 5 esempi del confronto tra Domain e TLD
get_domain_tld_examples <- function(dataset, dataset_name, n = 5) {
  # Verifica che le colonne "Domain" e "TLD" esistano
  if (!all(c("Domain", "TLD") %in% colnames(dataset))) {
    stop("Il dataset '", dataset_name, "' non contiene le colonne 'Domain' e 'TLD'.")
  }
  
  # Crea la colonna 'domain_ext' estraendo la sottostringa dopo l'ultimo punto in Domain
  dataset <- dataset %>%
    mutate(domain_ext = sub(".*\\.", "", Domain))
  
  # Estrae n esempi casuali
  examples <- dataset %>% sample_n(n)
  
  cat("\nEsempi per", dataset_name, ":\n")
  print(examples %>% select(Domain, domain_ext, TLD))
  
  return(examples)
}

# Applica la funzione ai tre dataset
examples_original  <- get_domain_tld_examples(combined_dataset, "Dataset Originale", 5)
examples_synthetic <- get_domain_tld_examples(synthetic_dataframe, "Synthetic Dataset", 5)
examples_context   <- get_domain_tld_examples(synthetic_dataframe_context, "Synthetic Context Dataset", 5)
