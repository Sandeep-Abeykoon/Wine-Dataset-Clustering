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

# Apply PCA to the standardized data
pca_data = prcomp(scaled_data_no_outliers, center = TRUE, scale = FALSE) 
summary(pca_data)

# Create new transformed dataset with the extracted PCA components (first 7 PCs)
wine_transform <- as.data.frame(-pca_data$x[, 1:7]) 
head(wine_transform)

# Potting the new dataset
boxplot(wine_transform)


#----------------------------------------------------------------------------------
# Determining the number of cluster centers using PCA-transformed data
#-------------------------------------------------------------------------

# NbClust Method with PCA-transformed data
set.seed(10)
nb_pca <- NbClust(wine_transform, min.nc = 2, max.nc = 15, method = "kmeans")

table(nb_pca$Best.n[1,])

# Provide bar chart for NbClust with PCA-transformed data
barplot(table(nb_pca$Best.n[1,]), 
        xlab = "Number of Clusters",
        ylab = "Number of Criteria",
        main = "Number of Clusters Chosen by 30 Criteria (PCA)")

#------------------------------------------------------------------------

# Elbow Method with PCA-transformed data
# Calculate WSS for different number of clusters using PCA-transformed data

k_values_pca <- 2:10
WSS_pca <- sapply(k_values_pca, function(k) {kmeans(wine_transform, centers = k)$tot.withinss})

# Plot the elbow curve for PCA-transformed data
plot(k_values_pca, WSS_pca, type = "b", xlab = "Number of Clusters (k)", ylab = "Within Sum of Squares (WSS)", main = "Elbow Method (PCA)")

# Find the elbow point for PCA-transformed data
diffs_pca <- c(0, diff(WSS_pca))
elbow_pca <- which(diffs_pca < mean(diffs_pca))[1]

# Add a point for the elbow for PCA-transformed data
points(elbow_pca, WSS_pca[elbow_pca], col = "red", cex = 2, pch = 19)

# Annotate the elbow point for PCA-transformed data
text(elbow_pca, WSS_pca[elbow_pca], labels = paste("Elbow at k =", elbow_pca), pos = 4, col = "red")
#---------------------------------------------------------------------------------------

# Gap Statistic method with PCA-transformed data
# Calculate the gap statistic
gap_stat_pca <- clusGap(wine_transform, FUN = kmeans, nstart = 25, K.max = 10, B = 50)

# Plot the gap statistic for PCA-transformed data
plot(gap_stat_pca, main = "Gap Statistic (PCA)", xlab = "Number of Clusters", ylab = "Gap")

# Find the optimal number of clusters using the gap statistic for PCA-transformed data
optimal_k_pca <- maxSE(gap_stat_pca$Tab[, "gap"], gap_stat_pca$Tab[, "SE.sim"], method = "Tibs2001SEmax")

# Print the optimal number of clusters for PCA-transformed data
print(optimal_k_pca)

#---------------------------------------------------------------------------------------

# Silhouette Method with PCA-transformed data
fviz_nbclust(wine_transform, kmeans, method = 'silhouette')
#----------------------------------------------------------------------------------------

# Fitting the k-means clustering with PCA-transformed data
kmeans_wine <- kmeans(wine_transform, centers = 2, nstart = 10)
kmeans_wine

# Plot clustered results for PCA-transformed data
plotcluster(wine_transform, kmeans_wine$cluster)

# Parallel coordinates plot for PCA-transformed data
parcoord(wine_transform, kmeans_wine$cluster)

# Visualize the clustered results
fviz_cluster(kmeans_wine, data = wine_transform)
#--------------------------------------------------------------------------------

# Calculate the ratio of BSS over TSS for PCA-transformed data
TSS_pca <- sum(kmeans_wine$withinss) + sum(kmeans_wine$betweenss)
BSS_over_TSS_pca <- sum(kmeans_wine$betweenss) / TSS_pca

# Output the ratio of BSS over TSS for PCA-transformed data
BSS_over_TSS_pca

# Calculate and output BSS and WSS indices for PCA-transformed data
BSS_pca <- kmeans_wine$betweenss
WSS_pca <- kmeans_wine$tot.withinss
BSS_pca
WSS_pca

#-------------------------------------------------------------------------------
# Calculate the silhouette width for each point for PCA-transformed data
sil_pca <- silhouette(kmeans_wine$cluster, dist(wine_transform))

# Plot the silhouette plot for PCA-transformed data
fviz_silhouette(sil_pca)

#-------------------------------------------------------------------------------
# Calculate the Calinski-Harabasz Index for PCA-transformed data
calinski_harabasz <- cluster.stats(wine_transform, kmeans_wine$cluster)$ch

# Output the Calinski-Harabasz Index for PCA-transformed data
calinski_harabasz

