### Import data

library(readr)
data <- read_csv("C:\Users\alire\Downloads\archive\sales_and_customer_insights")
view(data)
names(data1)

## Data Manipulation

# Rename
data1 <- rename(data,
                "P_ID" = "Product_ID")
data1 <- rename(data,
                "AOV" = "Average_Order_Value")
view(data1)


# Select

data2 <- select(data1,
                1:5)


data2 <- select(data1,
                -P_ID)

data2 <- select(data1,
                c("Customer_ID",
                  "Region", 
                  "Season"))

view(data2)


# Filter "==", ">/<"

data2 <- filter(data1,
                Region == "Asia",
                Season == "Winter")
head(data1, 5)

view(data2)


# Sort

data3 <- arrange(data1,
                 Region,
                 AOV)

view(data3) 


# Mutate

data4 <- mutate(data1,
                "Percentage_PF" = Purchase_Frequency / 100)
view(data4)


# Pipe %>% ctrl + shift + M

datapipe <- data1 %>%
  rename("Wilayah" = "Region") %>%
  filter(Wilayah == "Asia") %>%
  arrange(AOV)

view(datapipe)


# Group by and Summaries

data1 %>%
  group_by(Region) %>%
  summarize(total_aov_region = sum(AOV),
            average_aov_region = mean(AOV))

### Data Cleaning

# Cek Missing Value

# Hapus Missing Value

# Replace Missing Value
