# Funzione di denormalizzazione: dato un valore normalizzato, il valore minimo e il valore massimo originali,
# restituisce il valore nella scala originale
denormalize <- function(x_norm, min_val, max_val) {
  return(x_norm * (max_val - min_val) + min_val)
}

# Supponiamo che, durante la normalizzazione, tu abbia salvato i valori minimi e massimi per le feature numeriche:
# (ad esempio, per le colonne utilizzate)
numeric_cols <- c("URLLength", "NoOfSubDomain", "CharContinuationRate", "URLCharProb", 
                  "NoOfOtherSpecialCharsInURL", "LineOfCode", "NoOfImage", "NoOfSelfRef", "NoOfJS")

# Esempio: calcolo di min e max (se non li hai già salvati)
mins <- sapply(numeric_features, min)
maxs <- sapply(numeric_features, max)

# Ora, supponiamo che "misclassified" sia un data frame contenente le entry misclassificate, 
# ottenute da un modello addestrato sul dataset normalizzato (ad esempio final_data o un sottoinsieme normalizzato).
# Per ogni colonna numerica, applichiamo la funzione di denormalizzazione:
original_misclassified_denormalize <- original_misclassified  # Copia del data frame

for(col in numeric_cols) {
  if(col %in% colnames(original_misclassified_denormalize)) {
    original_misclassified_denormalize[[col]] <- denormalize(original_misclassified_denormalize[[col]], mins[col], maxs[col])
  }
}

# Visualizza i dati denormalizzati per i misclassified
print(original_misclassified_denormalize)
