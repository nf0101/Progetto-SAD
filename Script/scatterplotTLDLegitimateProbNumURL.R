# Caricare le librerie necessarie
library(dplyr)
library(ggplot2)

# Controllo esistenza delle colonne
if (!"TLDLegitimateProb" %in% colnames(combined_dataset_clean) | !"TLD" %in% colnames(combined_dataset_clean)) {
  stop("Errore: Le colonne 'TLDLegitimateProb' o 'TLD' non esistono nel dataset.")
}

# Aggregare i dati per TLD
tld_summary <- combined_dataset_clean %>%
  group_by(TLD) %>%
  summarise(
    TLDLegitimateProb = mean(TLDLegitimateProb, na.rm = TRUE),
    PhishingRatio = mean(label == 0, na.rm = TRUE),  # Proporzione di URL phishing
    Count = n()
  )

# Filtrare per valori di TLDLegitimateProb < 0.09
tld_summary_filtered <- tld_summary %>%
  filter(TLDLegitimateProb < 0.09)

# Verifica la creazione del dataset
print(head(tld_summary_filtered))

# Creare lo scatterplot
print(ggplot(tld_summary_filtered, aes(x = TLDLegitimateProb, y = PhishingRatio)) +
        geom_point(color = "blue", alpha = 0.6) +  # Punti blu con trasparenza
        geom_smooth(method = "lm", color = "red", se = FALSE) +  # Regressione lineare
        labs(
          title = "Relazione tra TLDLegitimateProb e Proporzione di URL di Phishing (x < 0.09)",
          x = "TLDLegitimateProb",
          y = "Proporzione di URL di Phishing"
        ) +
        theme_minimal()
)

# Creare lo scatterplot con il numero totale di URL per ciascun TLD
print(ggplot(tld_summary_filtered, aes(x = TLDLegitimateProb, y = Count)) +
        geom_point(color = "blue", alpha = 0.6) +  # Punti blu con trasparenza
        geom_smooth(method = "lm", color = "red", se = FALSE) +  # Regressione lineare
        labs(
          title = "Relazione tra TLDLegitimateProb e Numero Totale di URL per TLD (x < 0.09)",
          x = "TLDLegitimateProb",
          y = "Numero Totale di URL"
        ) +
        theme_minimal()
)
cor_pearson <- cor(tld_summary_filtered$TLDLegitimateProb, tld_summary_filtered$Count, method = "pearson", use = "complete.obs")

# Calcolare la correlazione di Spearman (per dati non lineari)
cor_spearman <- cor(tld_summary_filtered$TLDLegitimateProb, tld_summary_filtered$Count, method = "spearman", use = "complete.obs")
print(cor.test(tld_summary_filtered$TLDLegitimateProb, tld_summary_filtered$Count, method = "pearson", use = "complete.obs"))

# Modello di regressione lineare tra TLDLegitimateProb e Count
lm_model <- lm(Count ~ TLDLegitimateProb, data = tld_summary_filtered)

# Estrarre il coefficiente di determinazione R^2
r_squared <- summary(lm_model)$r.squared
print(paste("R^2:", r_squared))
