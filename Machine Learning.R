# download packages
install.packages("caret")

# load library yang dibutuhkan
library(MASS)
library(caret)

# load dan siapkan data
data(Boston)
?Boston
set.seed(123)

# membagi data menjadi training dan testing
training_index <- createDataPartition(Boston$medv, p = 0.8, list = FALSE)
train_data <- Boston[training_index, ]
test_data <- Boston[-training_index, ]


## 1. membuat ketiga model
# model 1: regresi linear sederhana
model_sederhana <- lm(medv ~ rm, data = train_data)
summary(model_sederhana)

# model 2: regresi linear berganda
model_berganda <- lm(medv ~ rm + lstat + age + dis, data = train_data)
summary(model_berganda)

# model 3: regresi polinomial
model_poly <- lm(medv ~ rm + I(rm^2), data = train_data)
summary(model_poly)


## 2. fungsi untuk menghitung R2 dan RMSE
calculate_metrics <- function(model, test_data) {
  predictions <- predict(model, newdata = test_data)
  
  # menghitung R-Squared
  r2 <- 1 - sum((test_data$medv - predictions) ^2) /
    sum((test_data$medv - mean(test_data$medv)) ^2)
  
  # menghitung RMSE
  rmse <- sqrt(mean((test_data$medv - predictions) ^2))
  
  return(c(R_squared = r2, RMSE = rmse))
}


## 3. menghitung metrics untuk setiap model
metrics_sederhana <- calculate_metrics(model_sederhana, test_data)
metrics_berganda <- calculate_metrics(model_berganda, test_data)
metrics_poly <- calculate_metrics(model_poly, test_data)

# membuat tabel perbandingan
comparison_table <- rbind(
  "model sederhana" = metrics_sederhana,
  "model berganda" = metrics_berganda,
  "model polinomial" = metrics_poly
)

# print hasil perbandingan
cat("perbandingan model:\n")
print(round(comparison_table, 4))



## visualisasi perbandingan metrics
par(mfrow = c(1, 2))

# plot RMSE
barplot(comparison_table[,1],
        main = "perbandingan R-squared",
        ylim = c(0, 1),
        col = c("skyblue", "lightgreen", "pink"))
grid()

# plot RMSE
barplot(comparison_table[,2],
        main = "perbandingan RMSE",
        col = c("skyblue", "lightgreen", "pink"))
grid()


## 5. menentukan model terbaik
best_r2 <- which.max(comparison_table[,1])
best_rmse <- which.min(comparison_table[,2])

cat("\nBerdasarkan R-Squared tertinggi: ")
cat("\nModel terbaik adalah:", rownames(comparison_table)[best_r2])
cat("\nNilai R-Squared adalah: ", round(comparison_table[best_r2, 1], 4))

cat("\nBerdasarkan RMSE Terendah: ")
cat("\nModel terbaik adalah:", rownames(comparison_table)[best_rmse])
cat("\nNilai RMSE adalah: ", round(comparison_table[best_rmse, 2], 4))



## 6. visualisasi prediksi model terbaik
best_model <- if(best_rmse == best_r2) {
  model_name <- tolower(strsplit(rownames(comparison_table)[best_rmse], " ")[[1]][2])
  model_name <- ifelse(model_name == "polinomial", "poly", model_name)  # Koreksi nama
  list(model = get(paste0("model_", model_name)),
       name = rownames(comparison_table)[best_rmse])
} else {
  cat("\n\r Perhatian: R-squared dan RMSE menunjukan model terbaik yang berbeda.")
  cat("\nDisarankan untuk mempertimbangkan tujuan analisis dalam pemilihan model final.")
  model_name <- tolower(strsplit(rownames(comparison_table)[best_rmse], " ")[[1]][2])
  model_name <- ifelse(model_name == "polinomial", "poly", model_name)  # Koreksi nama
  list(model = get(paste0("model_", model_name)),
       name = rownames(comparison_table)[best_rmse])
}


# Plot actual vs predicted untuk model terbaik
predictions <- predict(best_model$model, newdata = test_data)

plot(test_data$medv, predictions,
     main = paste("Actual vs Predicted -", best_model$name),
     xlab = "Actual Values",
     ylab = "Predicted Values")

