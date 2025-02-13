library(dplyr)
library(countrycode)
library(ggplot2)
library(rworldmap)
library(sf)

resolve_country <- function(tld) {
  tld <- toupper(trimws(tld)) 
  case_when(
    tld == "SU" ~ "Russia", # Associa SU all'ex Unione Sovietica (Russia)
    tld == "UK" ~ "United Kingdom",
    tld == "AG" ~ "Antigua and Barbuda",
    tld == "GI" ~ "Gibraltar",
    tld == "GL" ~ "Greenland",
    tld == "CZ" ~ "Czech Republic",
    tld == "BA" ~ "Bosnia and Herzegovina",
    tld == "GP" ~ "Guadeloupe",
    tld == "MK" ~ "Macedonia",
    tld == "MM" ~ "Myanmar",
    tld == "CD" ~ "Congo (Kinshasa)",
    tld == "CI" ~ "Ivory Coast",
    tld == "MYT" ~ "Mayotte",
    tld == "HK" ~ "Hong Kong",
    tld == "SZ" ~ "Swaziland",
    tld == "AD" ~ "Andorra",
    tld == "TT" ~ "Trinidad and Tobago",
    tld == "AG" ~ "Antigua and Barbuda",
    tld == "TKL" ~ "Tokelau",
    tld == "MO" ~ "Macau",
    tld == "GS" ~ "South Georgia and the South Sandwich Islands",
    tld == "PM" ~ "Saint Pierre and Miquelon",
    tld == "ST" ~ "Sao Tome and Principe",
    tld == "ZA" ~ "South Africa", # Aggiunto per esempio
    tld == "SX" ~ "Sint Maarten",
    tld == "TC" ~ "Turks and Caicos Islands",
    tld == "VI" ~ "United States Virgin Islands",
    tld == "WF" ~ "Wallis and Futuna",
    tld == "GH" ~ "Ghana",
    tld == "AX" ~ "Aland",
    tld == "VC" ~ "Saint Vincent and the Grenadines", # Aggiunto per esempio
    TRUE ~ countrycode(tld, origin = "iso2c", destination = "country.name") # Usa la funzione di default
  )
}

# Estrazione dei TLD a due lettere e associazione ai paesi
tld_with_countries <- combined_dataset_clean %>%
  filter(nchar(TLD) == 2) %>% # Filtra solo i TLD con due lettere
  mutate(Country = resolve_country(TLD)) %>% # Aggiunge la colonna del Paese
  group_by(Country) %>%
  summarise(
    TotalURLs = n(),
    PhishingURLs = sum(label == 0), # Conta gli URL di phishing
    PhishingPercentage = (PhishingURLs / TotalURLs) * 100
  ) %>%
  ungroup()

# Rimuovi eventuali valori NA (ad esempio, TLD non riconosciuti)
tld_with_countries <- tld_with_countries %>% filter(!is.na(Country))

# Creazione della mappa
# Ottieni i dati della mappa del mondo
world_map <- joinCountryData2Map(
  tld_with_countries,
  joinCode = "NAME",
  nameJoinColumn = "Country",
  verbose = TRUE,
  
)

# Disegna la mappa con un gradiente di colori
#layout(matrix(1), widths = 2, heights = 2)
par(mar = c(0, 0, 0, 0))
map <- mapCountryData(
  world_map,
  nameColumnToPlot = "PhishingPercentage",
  catMethod = "fixedWidth",
  mapTitle = "Percentuale di URL di Phishing per TLD Nazionale",
  colourPalette = colorRampPalette(c("green", "yellow", "red"))(7), # Gradiente con 7 colori
  oceanCol = "lightblue",
  missingCountryCol = "white", # Colore per i paesi senza dati,
  #aspect = 1.1
  
)
  
# Mostra il grafico
print(map)




