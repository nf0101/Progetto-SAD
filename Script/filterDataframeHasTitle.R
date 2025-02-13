filtered_dataset <- combined_dataset %>%
  filter(
    HasTitle == 1,                                # Entry con HasTitle = 1
    #DomainTitleMatchScore == 0,
    #URLTitleMatchScore >= 80# URLTitleMatchScore diverso da DomainTitleMatchScore
  )# %>%
 # select(URL, Domain, Title, URLTitleMatchScore, DomainTitleMatchScore, label) # Seleziona le colonne desiderate