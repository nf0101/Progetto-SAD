library(dplyr)
library(ggplot2)
library(e1071)

# due grafici su 500 dati del test dataset
set.seed(123)
sample_index <- sample(nrow(X_test), 1000)  

half <- length(sample_index) / 2
index1 <- sample_index[1:half]
index2 <- sample_index[(half + 1):length(sample_index)]

# I due subset sono stati creati per controllo
small_data <- X_test[index1, ]
small_labels <- y_test[index1]


# Applica PCA al subaset per poter mostrare i dati in 2D
pca_result <- prcomp(small_data, scale = TRUE)

pca_data <- data.frame(
  PC1 = pca_result$x[, 1],
  PC2 = pca_result$x[, 2],
  Label = factor(small_labels, labels = c("Phishing", "Legittimo"))
)

# Addestramento SVM sulle prime due componenti principali per poter mostare u dati
svm_pca_model <- svm(x = pca_result$x[, 1:2], y = as.factor(small_labels), kernel = "linear", cost = 1)

# Per confrontare le etichette, calcola le predizioni sui dati PCA
pca_data$Predicted <- predict(svm_pca_model, pca_data[, c("PC1", "PC2")])
# Conversione livelli
pca_data$Predicted <- factor(pca_data$Predicted, levels = c("0", "1"), labels = c("Phishing", "Legittimo"))

# Estrai le entry  in cui la predizione non corrisponde all'etichetta reale
misclassified <- pca_data %>% filter(as.character(Predicted) != as.character(Label))
print(misclassified)  
original_misclassified <- small_data[rownames(misclassified), ]
print(original_misclassified)
# griglia
grid <- expand.grid(
  PC1 = seq(min(pca_data$PC1), max(pca_data$PC1), length.out = 100),
  PC2 = seq(min(pca_data$PC2), max(pca_data$PC2), length.out = 100)
)
grid$Label <- predict(svm_pca_model, grid)
grid$Label <- factor(grid$Label, levels = c("0", "1"), labels = c("Phishing", "Legittimo"))

# Grafico PCA con confini di decisione e evidenziazione dei misclassificati
print(
  ggplot(pca_data, aes(x = PC1, y = PC2, color = Label)) +
    geom_tile(data = grid, aes(fill = Label), alpha = 0.2) +
    geom_point(alpha = 0.6, size = 1.5) +
    # Aggiungi un layer per cerchiare in rosso i punti misclassificati
    geom_point(data = misclassified, aes(x = PC1, y = PC2), 
               color = "red", shape = 1, size = 4, stroke = 0.5) +
    labs(title = "PCA con confini di decisione del modello SVM",
         x = "PC1",
         y = "PC2",
         color = "Classe reale",
         fill = "Classe assegnata") +
    theme_minimal() + 
    theme(legend.position = "bottom",
          legend.text = element_text(size = 14),
          legend.title = element_text(size = 16),
          
    ) +
    guides(
      fill = guide_legend(nrow = 2),
      color = guide_legend(nrow = 2)
    )
)

