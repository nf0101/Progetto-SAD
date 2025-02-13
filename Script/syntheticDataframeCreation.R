# Definisci il percorso dei file
file_synthetic_url <- "C:/Users/frugi/Desktop/Uni/Magistrale/SAD/Phishing_URL_Dataset/Dataset sintetici/synthetic_url_dataset_100k.csv"
file_synthetic_url_context <- "C:/Users/frugi/Desktop/Uni/Magistrale/SAD/Phishing_URL_Dataset/Dataset sintetici/synthetic_url_dataset_corrected.csv"

# Carica i dataset nei rispettivi dataframe
synthetic_dataframe <- read.csv(file_synthetic_url, stringsAsFactors = FALSE)
synthetic_dataframe_context <- read.csv(file_synthetic_url_context, stringsAsFactors = FALSE)

# Controlla le prime righe dei dataframe per verificare il caricamento
head(synthetic_dataframe)
head(synthetic_dataframe_context)
