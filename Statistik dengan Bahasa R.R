install.packages(...)
install.packages("ggplot")
install.packages("ggplot2")
install.packages("ggpubr")
install.packages("moments")
install.packages("car")
library(tidyverse)
library(moments)
library(car)
library(nortest)
library(ggpubr)
library(ggplot2)


### ukuran pemusatan data
data <- c(23, 45, 67, 34, 89, 56, 78, 90, 45, 67, 200)

# mean
mean_value <- mean(data, na.rm = T)
mean_value

# median
median_value <- median(data, na.rm = T)
median_value

# modus
get_mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
mode_value <- get_mode(data)
print(mode_value)


### ukuran penyebaran data

# variance
var_value <- var(data)

# standard deviation
sd_value <- sd(data)

# range
range_value <- range(data)
IQR_value <- IQR(data)

# quantiles
quantiles <- quantile(data, probs = c(0.5))

# comprehensive summary
summary_stats <- summary(data)


### distribusi data

# skewness
skewness_value <- skewness(data)

# kurtosis
kurtosis_value <- kurtosis(data)



### visualisasi distribusi

# histrogram
ggplot(data.frame(x = data), aes(x = x)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "black") +
  theme_minimal() +
  labs(title="histogram of data")

# box plot
ggplot(data.frame(x = data), aes(y = x)) +
  geom_boxplot(fill = "green") +
  theme_minimal() +
  labs(title = "Box plot of data")


### uji hipotesis
# 0.05
# H0
# Ha
## independent sample t-test

# contoh data 2 kelompok
group1 <- c(23, 45, 67, 34, 89)
group2 <- c(56, 78, 90, 45, 67)

# t-test
t.test(group1, group2)

# dengan asumsi variansi sama
t.test(group1, group2, var.equal = TRUE)


## paired t-test

# data before after
before <- c(23, 45, 67, 34, 89)
after <- c(25, 48, 69, 37, 92)

# paired t-test
t.test(before, after, paired = TRUE)



### ANOVA
# one-way ANOVE

# contoh data
group <- factor(rep(c("A", "B", "C"), each = 5))
values <- c(23, 45, 67, 34, 89, 56, 78, 90, 45, 67, 34, 56, 78, 90, 23)
data_anova <- data.frame(group, values)

# ANOVA test
anova_result <- aov(values ~ group, data = data_anova)
summary(anova_result)

# post-hoc test (Tukey)
TukeyHSD(anova_result)


## two-way ANOVA
# contoh data dengan 2 faktor

Kelas <- factor(rep(c("A", "B"), each = 10))
Jenis_Kelamin <- factor(rep(c("X", "Y"), each = 5, times = 2))
response <- rnorm(20)
data_anova2 <- data.frame(Kelas, Jenis_Kelamin, response)

# two-way ANOVA

anova2_result <- aov(response ~ Kelas * Jenis_Kelamin, data = data_anova2)
summary(anova2_result)


### visualisasi statistik advance

# box plot dengan statistik

ggboxplot(data_anova, x = "group", y = "values",
          color = "group",
          add = "jitter") +
  stat_compare_means(method = "annova")


# violin plot
ggplot(data_anova, aes(x = group, y = values, fill = group)) +
  geom_violin() +
  geom_boxplot(width = 0.1) +
  theme_minimal()


### reporting statistik

# fungsi untukk melihat ringkasan statistik

get_summary_stats <- function(data) {
  list(
    mean = mean(data),
    median = median(data),
    sd = sd(data),
    var = var(data),
    skew = skewness(data),
    kurt = kurtosis(data),
    norm_test = shapiro.test(data)$p.value
  )
}
get_summary_stats

# aplikasi fungsi
summary_results <- get_summary_stats(data)
print(summary_results)



### regresi

# dataset bawaan
View(mtcars)
?mtcars
data <- mtcars

# scatter plot
plot(data$wt, data$mpg,
     main = "hubungan berat mobil dan mpg",
     xlab = "berat mobil (wt)",
     ylab = "miles per gallon (mpg)",
     pch = 19, col = "blue")


### membangun model regresi

# model regresi linear
model <- lm(mpg ~ wt, data = data)

# menampilkan ringkasan model
summary(model)



