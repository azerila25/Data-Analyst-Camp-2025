# Install dan load package yang dibutuhkan
install.packages("ggplot2")
install.packages("caret")
library(caret)

# Load data dari clipboard
student_data <- read.delim("clipboard")
str(student_data)

# Ubah Risk.Level menjadi numerik
student_data$Risk.Level <- as.numeric(factor(student_data$Risk.Level, levels = c("Low", "Medium", "High")))
set.seed(123)

# Membagi data menjadi training dan testing
training_index <- createDataPartition(student_data$Risk.Level, p = 0.8, list = FALSE)
train_data <- student_data[training_index, ]
test_data <- student_data[-training_index, ]



# milik Ali Reza Bahtiar
## 1. MEMBUAT KETIGA MODEL
# Model 1: Regresi Linear Sederhana
model_sederhana <- lm(Risk.Level ~ Stress.Level..GSR., data = train_data)

# Model 2: Regresi Linear Berganda
model_berganda <- lm(Risk.Level ~ Sleep.Hours + Stress.Level..GSR. + Anxiety.Level, data = train_data)

# Model 3: Regresi Polinomial
model_poly <- lm(Risk.Level ~ Sleep.Hours + I(Sleep.Hours^2), data = train_data)



## 2. Fungsi untuk Menghitung R-Squared dan RMSE
calculate_metrics <- function(model, test_data) {
  predictions <- predict(model, newdata = test_data)
  
  r2 <- 1 - sum((test_data$Risk.Level - predictions) ^2) /
    sum((test_data$Risk.Level - mean(test_data$Risk.Level)) ^2)
  
  rmse <- sqrt(mean((test_data$Risk.Level - predictions) ^2))
  
  return(c(R_squared = r2, RMSE = rmse))
}



## 3. Hitung Metrics untuk Setiap Model
metrics_sederhana <- calculate_metrics(model_sederhana, test_data)
metrics_berganda <- calculate_metrics(model_berganda, test_data)
metrics_poly <- calculate_metrics(model_poly, test_data)



## 4. Buat Tabel Perbandingan
comparison_table <- rbind(
  "model sederhana" = metrics_sederhana,
  "model berganda" = metrics_berganda,
  "model polinomial" = metrics_poly
)
print(round(comparison_table, 4))



## 5. Menentukan Model Terbaik
# Pilih Model Terbaik Berdasarkan RMSE
best_rmse <- which.min(comparison_table[,2])
best_model_name <- rownames(comparison_table)[best_rmse]



## 6. Visualisasi model terbaik
# Pilih model terbaik
best_model <- switch(best_model_name,
                     "model sederhana" = model_sederhana,
                     "model berganda" = model_berganda,
                     "model polinomial" = model_poly)

# Buat Prediksi
predictions <- predict(best_model, newdata = test_data)

# Sesuaikan panjang jika berbeda
if (length(predictions) != length(test_data$Risk.Level)) {
  predictions <- predictions[1:length(test_data$Risk.Level)]
}

# Scatter Plot: Nilai Aktual vs Prediksi
plot(test_data$Risk.Level, predictions,
     main = paste("Actual vs Predicted -", best_model_name),
     xlab = "Actual Values",
     ylab = "Predicted Values")

# Tambahkan Garis Referensi
abline(0, 1, col = "red")
