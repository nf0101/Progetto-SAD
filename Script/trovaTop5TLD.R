library(dplyr)
#synthetic_dataframe_context
# Filtra il dataset sintetico per TLD uguale a "211" e seleziona le colonne URL e TLD,
# poi prende le prime 5 righe
top5_urls_211 <- synthetic_dataframe_context %>%
  filter(TLD == "240") %>%
  select(URL, TLD, Domain) %>%
  slice_head(n = 5)

# Visualizza il risultato
print(top5_urls_211)
