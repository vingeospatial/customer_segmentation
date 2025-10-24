library(dplyr)
library(ggplot2)
library(factoextra)
library(readr)


# load the iris data sets    
data(iris) 

# Remove the species factor variable (it is the true label we want to discover)    
df <- iris[, -5]

# Scale the data (standardize to mean 0 and standard deviation 1)    
df_scaled <- scale(df)   

# Check the first few scaled observations    
head(df_scaled)  

# Set a seed for reprocudibility of random starting point    
set.seed(123)

# Calculate WCSS for k = 1 to k = 10    
wss <- sapply(1:10,
              function(k){
                # nstart = 25 runs 25 different initializations and picks the best one   
                kmeans(df_scaled, centers = k, nstart = 25)$tot.withinss
              })

# Plot the elbow curve  
plot(1:10, wss, type = "b",
     xlab = "Number of clusters (K)",
     ylab = "Total within-clusters sum of squares(wcss)",
     main = "Elbow method for K-Means") 



# Run the k-means algorithm with K=3     
km.result <- kmeans(df_scaled,
                    centers = 3,
                    nstart = 25)

# Print the result object      
print(km.result)





