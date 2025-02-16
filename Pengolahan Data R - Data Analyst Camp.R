install.packages("tidyverse")
library("tidyverse")
?tidyverse

view(data)
data1 <- read.delim("clipboard")
view(data1)

glimpse(data)

names(data)

class(data$Customer_ID)
class(data$Lifetime_Value)

data2 <- data[1:100, 1:4]
view(data2)

head(data, 10)
tail(data, 5)

sum(data$Purchase_Frequency)
mean(data$Purchase_Frequency)
var(data$Purchase_Frequency)