install.packages("tidyverse")
install.packages("ggplot2")
library(tidyr)
library(ggplot2)
library(dplyr)

### Data Reshaping

# wide to long

data6 <- data3[, c("Region", "Most_Frequent_Category", "Purchase_Frequency")]
View(data6)

data7 <- pivot_wider(data6,
                     names_from = Most_Frequent_Category,
                     values_from = Purchase_Frequency)
View(data7)

# long to wide

### Visualization
?ggplot

# scatter plot
data3 %>% 
  ggplot(aes(x = Purchase_Frequency, y = Lifetime_Value)) +
  geom_point()


# line plot
class(data3$Launch_Date)
data %>% 
  ggplot(aes(Launch_Date, Lifetime_Value)) +
  geom_line()

library(dplyr)

data_daily <- data3 %>%
  group_by(Launch_Date) %>%
  summarize(Lifetime_Value = mean(Lifetime_Value, na.rm = TRUE))

ggplot(data_daily, aes(x = Launch_Date, y = Lifetime_Value)) +
  geom_line()


# bar
data3 %>% ggplot(aes(Most_Frequent_Category, Lifetime_Value)) +
  geom_bar(stat = "identity")

# histogram
data3 %>% ggplot(aes(x = Lifetime_Value)) +
  geom_histogram(bins = 40)
