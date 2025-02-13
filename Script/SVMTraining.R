library(dplyr)
library(caret)
library(e1071)

X <- final_data %>% select(-label)  # Tutte le feature tranne label per l'addestramento
y <- final_data$label               # la colonna label sarà la var target

set.seed(123)
train_index <- createDataPartition(y, p = 0.8, list = FALSE)
X_train <- X[train_index, ]
X_test <- X[-train_index, ]
y_train <- y[train_index]
y_test <- y[-train_index]

# Addestra il modello SVM sul ds di training con kernel lineare
svm_model <- svm(x = X_train, y = as.factor(y_train), kernel = "linear", cost = 1)

# Risultati della classificazione
predictions <- predict(svm_model, X_test)

# Matrice di confusione e accuracy, cm_df serve per i colori dei valori della matrice
conf_matrix <- confusionMatrix(as.factor(predictions), as.factor(y_test))
cm_df <- as.data.frame(conf_matrix$table)
print(conf_matrix)
cm_df <- cm_df %>% 
  mutate(text_color = ifelse(Freq > median(Freq), "white", "black"))

acc <- round(conf_matrix$overall["Accuracy"], 3)

print(
  ggplot(cm_df, aes(x = Reference, y = Prediction, fill = Freq)) +
    geom_tile(color = "white") +
    geom_text(aes(label = Freq, color = text_color), vjust = 1, size = 5) +
    scale_fill_gradient(low = "aliceblue", high = "deepskyblue4") +
    scale_color_identity() +
    labs(title = paste("Matrice di Confusione - Accuracy:", acc),
         x = "Valori Reali",
         y = "Valori Predetti") +
    theme_minimal() +
    theme(plot.title = element_text(size = 17, hjust = 0.5))
)
