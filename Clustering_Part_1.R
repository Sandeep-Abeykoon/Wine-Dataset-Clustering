# w1912796/20210057
#Sandeep Abeykoon

# Load necessary libraries
library(ggplot2)
library(NbClust)
library(cluster)
library(fpc) 
library(MASS)
library(factoextra)

# Read the CSV file
wine_data <- read.csv("C:/Users/sande/Desktop/Machine Learning CW/Whitewine_v6.csv")

#-------- Data Preprocessing-------------------------------------
# Checking the structure of wine data set
str(wine_data)

# Checking number of null values
sum(is.na(wine_data))   # Gives the answer as 0

# Omitting any NA values
wine_data_clear_na <- na.omit(wine_data)

# Checking(Visualizing) the data 
boxplot(wine_data_clear_na)

# Scale the feature columns (excluding the last column)
scaled_data <- as.data.frame(scale(wine_data_clear_na[, -ncol(wine_data_clear_na)]))

# Printing the summary
summary(scaled_data)

# Print the first few rows of the scaled data
print(head(scaled_data))

# Checking(Visualizing) the data after standardizing
boxplot(scaled_data)

# Calculate the IQR for each column in scaled data
Q1 <- apply(scaled_data, 2, quantile, probs = 0.25)
Q3 <- apply(scaled_data, 2, quantile, probs = 0.75)
IQR <- Q3 - Q1

# Find rows where any column value is an outlier
outliers <- apply(scaled_data, 1, function(row) any(row < Q1 - 1.5 * IQR | row > Q3 + 1.5 * IQR))

# Print the rows that are outliers
print(scaled_data[outliers, ])

# Count the number of outliers
print(sum(outliers))

# Omitting the outlier rows
scaled_data_no_outliers <- scaled_data[!outliers, ]

# Number of rows after getting rid of the outliers
print(nrow(scaled_data_no_outliers))

# Checking(Visualizing) the data after removing the outliers
boxplot(scaled_data_no_outliers)

#----------------------------------------------------------------------------------
# Determining the number of cluster centers
#-------------------------------------------------------------------------

# NbClust Method
set.seed(10)
nb <- NbClust(scaled_data_no_outliers, min.nc = 2, max.nc = 15, method = "kmeans")

table(nb$Best.n[1,])

# Provide bar chart
barplot(table(nb$Best.n[1,]), 
        xlab = "Number of Clusters",
        ylab = "Number of Criteria",
        main = "Number of Clusters Chosen by 30 Criteria")

#------------------------------------------------------------------------

# Elbow Method
# Calculate WSS for different number of clusters

k_values <- 2:10
WSS <- sapply(k_values, function(k) {kmeans(scaled_data_no_outliers, centers = k)$tot.withinss})

# Plot the elbow curve
plot(k_values, WSS, type = "b", xlab = "Number of Clusters (k)", ylab = "Within Sum of Squares (WSS)", main = "Elbow Method")

# Find the elbow point
diffs <- c(0, diff(WSS))
elbow <- which(diffs < mean(diffs))[1]

# Add a point for the elbow
points(elbow, WSS[elbow], col = "red", cex = 2, pch = 19)

# Annotate the elbow point
text(elbow, WSS[elbow], labels = paste("Elbow at k =", elbow), pos = 4, col = "red")
#---------------------------------------------------------------------------------------

# Gap Statistic method
# Calculate the gap statistic
gap_stat <- clusGap(scaled_data_no_outliers, FUN = kmeans, nstart = 25, K.max = 10, B = 50)

# Plot the gap statistic
plot(gap_stat, main = "Gap Statistic", xlab = "Number of Clusters", ylab = "Gap")

# Find the optimal number of clusters using the gap statistic
optimal_k <- maxSE(gap_stat$Tab[, "gap"], gap_stat$Tab[, "SE.sim"], method = "Tibs2001SEmax")

# Print the optimal number of clusters
print(optimal_k)

#---------------------------------------------------------------------------------------

# Silhouette Method
fviz_nbclust(scaled_data_no_outliers, kmeans, method = 'silhouette')
#---------------------------------------------------------------------------------

# Fitting the k-means clustering

# Perform kmeans analysis using the most favored "k" from automated methods
set.seed(1234)
# Use the optimal k from the silhouette method
fit.km <- kmeans(scaled_data_no_outliers, 2)

# Output related to kmeans analysis
fit.km

# Plot clustered results
plotcluster(scaled_data_no_outliers, fit.km$cluster)

# Parallel coordinates plot
parcoord(scaled_data_no_outliers, fit.km$cluster)

fviz_cluster(fit.km, data = scaled_data_no_outliers)

# Calculate the ratio of BSS over TSS
TSS <- sum(fit.km$withinss) + sum(fit.km$betweenss)
BSS_over_TSS <- sum(fit.km$betweenss) / TSS

# Output the ratio of BSS over TSS
BSS_over_TSS

# Calculate and output BSS and WSS indices
BSS <- fit.km$betweenss
WSS <- fit.km$tot.withinss
BSS
WSS

#-------------------------------------------------------------------------------
# Calculate the silhouette width for each point
sil <- silhouette(fit.km$cluster, dist(scaled_data_no_outliers))

# Plot the silhouette plot
fviz_silhouette(sil)






