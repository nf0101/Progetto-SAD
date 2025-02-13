# Filtrare e contare gli URL con NoOfQMarkInURL >= 1
count_Degits <- sum(combined_dataset$NoOfQMarkInURL >= 1, na.rm = TRUE)

# Filtrare e contare gli URL con HasObfuscation == 1
count_obfuscation <- sum(combined_dataset$HasObfuscation == 1, na.rm = TRUE)

# Filtrare e contare gli URL con entrambe le caratteristiche
count_both <- sum(combined_dataset$NoOfQMarkInURL >= 1 & combined_dataset$HasObfuscation == 1, na.rm = TRUE)

# Stampare i risultati
cat("Numero di URL con NoOfQMarkInURL >= 1:", count_Degits, "\n")
cat("Numero di URL con HasObfuscation = 1:", count_obfuscation, "\n")
cat("Numero di URL con entrambe le caratteristiche:", count_both, "\n")
