library(dplyr)

# Funzione per ottenere i primi 5 TLD (pre pulizia)
get_top5_pre <- function(dataset) {
  dataset %>%
    group_by(TLD) %>%
    summarise(Count = n(), .groups = "drop") %>%
    arrange(desc(Count)) %>%
    slice_head(n = 5)
}

# Funzione per ottenere i primi 5 TLD (post pulizia)
get_top5_post <- function(dataset) {
  dataset %>%
    # Rimuovi URL basati su indirizzo IP
    filter(!grepl("^http://(\\d{1,3}\\.){3}\\d{1,3}([:/?]|$)", URL)) %>%
    filter(!grepl("^https://(\\d{1,3}\\.){3}\\d{1,3}([:/?]|$)", URL)) %>%
    # Rimuovi le porte dalla colonna TLD
    mutate(TLD = sub(":.*$", "", TLD)) %>%
    group_by(TLD) %>%
    summarise(Count = n(), .groups = "drop") %>%
    arrange(desc(Count)) %>%
    slice_head(n = 5)
}

# Calcolo dei primi 5 TLD per ciascun dataset PRIMA della pulizia
pre_original  <- get_top5_pre(combined_dataset) %>% mutate(Dataset = "Dataset Originale")
pre_synthetic <- get_top5_pre(synthetic_dataframe) %>% mutate(Dataset = "Synthetic Dataset")
pre_context   <- get_top5_pre(synthetic_dataframe_context) %>% mutate(Dataset = "Synthetic Context Dataset")

pre_comparison <- bind_rows(pre_original, pre_synthetic, pre_context)

# Calcolo dei primi 5 TLD per ciascun dataset DOPO la pulizia
post_original  <- get_top5_post(combined_dataset) %>% mutate(Dataset = "Dataset Originale")
post_synthetic <- get_top5_post(synthetic_dataframe) %>% mutate(Dataset = "Synthetic Dataset")
post_context   <- get_top5_post(synthetic_dataframe_context) %>% mutate(Dataset = "Synthetic Context Dataset")

post_comparison <- bind_rows(post_original, post_synthetic, post_context)

# Stampa dei risultati
cat("----- Confronto tra i tre dataset PRE pulizia -----\n")
print(pre_comparison)

cat("\n----- Confronto tra i tre dataset POST pulizia -----\n")
print(post_comparison)
