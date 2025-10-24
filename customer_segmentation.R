library(dplyr)
library(ggplot2)
library(factoextra)
library(readr)

# Read the uploaded CSV file into a data frame    
data <- read_csv("data/shopping_trends_updated.csv")

# Display the first few rows of the dataset   
head(data, show_col_types = FALSE)

# Check the structure of the dataset: column names and data types   
str(data)

# Get Summary statistics like mean, min, max for each column   
summary(data)

# Rename columns to meet tidy standards  
data <- data |> 
  rename(
    customer_id = `Customer ID`,
    age = Age,
    gender = Gender,
    item_purchased = `Item Purchased`,
    category = Category,
    purchase_amount_usd = `Purchase Amount (USD)`,
    location = Location,
    size = Size,
    color = Color,
    season = Season,
    review_rating = `Review Rating`,
    subscription_status = `Subscription Status`,
    shipping_type = `Shipping Type`,
    discount_applied = `Discount Applied`,
    promo_code_used = `Promo Code Used`,
    previous_purchases = `Previous Purchases`,
    payment_method = `Payment Method`,
    frequency_of_purchases = `Frequency of Purchases`
    
  )

head(data)


# Remove rows where customer_id is missing     
data <- data |> 
  filter(!is.na(customer_id))
glimpse(data)

# Group data by customer_id and calculate key feature:  
# - Total quantity purchased  
# - Total amount spent 
# - Average unit price per item   
# - Number of transacions made    
customer_data <- data |> 
  group_by(customer_id) |> 
  summarise(
    total_quantity = sum(previous_purchases, na.rm = TRUE),
    total_spent = sum(purchase_amount_usd, na.rm = TRUE),
    average_unit_price = mean(purchase_amount_usd, na.rm = TRUE),
    number_of_transactions = n()
  )

# View the first few rows of the summarized customer data   
head(customer_data)


# Remove customer_id as it's just an identifier and not useful for clustering       
customer_data_numeric <- customer_data |> 
  select(-customer_id, -number_of_transactions)

# Normalize the numeric features so they have mean = 0 and standard deviation = 1   
normalized_data <- scale(customer_data_numeric)

# View the first few rows of the normalized data
head(normalized_data)


# use the Elbow method to visualize how WSS changes with different k values     
fviz_nbclust(normalized_data, kmeans, method = "wss") +
  labs(title = "Elbow Method for Choosing K")


# Apply k-means clustering with 4 clusters 
# Ensure consistent results every time you run the code  
set.seed(123)
kmeans_result <- kmeans(normalized_data, centers = 4, nstart = 25)

# Add the cluster label(1 to 4) back to the original customer data     
customer_data$cluster <- as.factor(kmeans_result$cluster)

# View the first few rows of the customer data with cluster assignment   

head(customer_data)




