# centroids
Centroids for anomaly detection

Below is a basic implementation of an anomaly detection algorythm that uses the concept of centroids


# Generate samples and detection function

```{r}

library(ggplot2)
library(reshape2)

set.seed(123)
n_samples <- 100
n_features <- 5000

# Generate non-anomalous data sets
data_set1 <- matrix(rnorm(n_samples * n_features), nrow = n_samples, ncol = n_features)
data_set2 <- matrix(runif(n_samples * n_features, -2, 2), nrow = n_samples, ncol = n_features)
data_set3 <- matrix(rexp(n_samples * n_features, 1), nrow = n_samples, ncol = n_features)

# Heatmap for Data Set 1
heatmap(data_set1, main = "Heatmap of Data Set 1", Colv = NA, Rowv = NA)

# Histogram for Data Set 1
hist_data_set1 <- melt(data_set1)
ggplot(hist_data_set1, aes(x = value)) +
  geom_histogram(bins = 30, fill = "blue", alpha = 0.7) +
  theme_minimal() +
  labs(title = "Histogram of Data Set 1", x = "Values", y = "Frequency")

# Heatmap for Data Set 2
heatmap(data_set2, main = "Heatmap of Data Set 2", Colv = NA, Rowv = NA)

# Histogram for Data Set 2
hist_data_set2 <- melt(data_set2)
ggplot(hist_data_set2, aes(x = value)) +
  geom_histogram(bins = 30, fill = "green", alpha = 0.7) +
  theme_minimal() +
  labs(title = "Histogram of Data Set 2", x = "Values", y = "Frequency")

# Heatmap for Data Set 3
heatmap(data_set3, main = "Heatmap of Data Set 3", Colv = NA, Rowv = NA)

# Histogram for Data Set 3
hist_data_set3 <- melt(data_set3)
ggplot(hist_data_set3, aes(x = value)) +
  geom_histogram(bins = 30, fill = "red", alpha = 0.7) +
  theme_minimal() +
  labs(title = "Histogram of Data Set 3", x = "Values", y = "Frequency")

# Combine data sets
combined_data <- rbind(data_set1, data_set2, data_set3)

# Heatmap for Combined Data
heatmap(combined_data, main = "Heatmap of Combined Data", Colv = NA, Rowv = NA)

# Histogram for Combined Data
hist_combined_data <- melt(combined_data)
ggplot(hist_combined_data, aes(x = value)) +
  geom_histogram(bins = 50, fill = "purple", alpha = 0.7) +
  theme_minimal() +
  labs(title = "Histogram of Combined Data", x = "Values", y = "Frequency")

# Split data into Corpus and Validation Set
validation_set_size <- round(0.2 * n_samples)  # 20% of the data as validation set
corpus_set_size <- n_samples - validation_set_size

# Randomly sample indices for validation set
set.seed(123)
validation_indices <- sample(1:(3 * n_samples), validation_set_size)  # 3 times n_samples because of 3 data sets

# Create Corpus and Validation Sets
validation_set <- combined_data[validation_indices, ]
corpus_set <- combined_data[-validation_indices, ]

# Visualize Validation Set
heatmap(validation_set, main = "Heatmap of Validation Set", Colv = NA, Rowv = NA)
hist_validation_set <- melt(validation_set)
ggplot(hist_validation_set, aes(x = value)) +
  geom_histogram(bins = 30, fill = "orange", alpha = 0.7) +
  theme_minimal() +
  labs(title = "Histogram of Validation Set", x = "Values", y = "Frequency")

# Visualize Corpus Set
heatmap(corpus_set, main = "Heatmap of Corpus Set", Colv = NA, Rowv = NA)
hist_corpus_set <- melt(corpus_set)
ggplot(hist_corpus_set, aes(x = value)) +
  geom_histogram(bins = 30, fill = "cyan", alpha = 0.7) +
  theme_minimal() +
  labs(title = "Histogram of Corpus Set", x = "Values", y = "Frequency")


# Generate anomalous data (based on data_set1 with added noise)
anomalous_data <- data_set1 + matrix(rnorm(n_samples * n_features, mean = 5, sd = 2), nrow = n_samples, ncol = n_features)

# Visualize Anomalous Data
heatmap(anomalous_data, main = "Heatmap of Anomalous Data", Colv = NA, Rowv = NA)
hist_anomalous_data <- melt(anomalous_data)
ggplot(hist_anomalous_data, aes(x = value)) +
  geom_histogram(bins = 30, fill = "pink", alpha = 0.7) +
  theme_minimal() +
  labs(title = "Histogram of Anomalous Data", x = "Values", y = "Frequency")


# Split anomalous data into Corpus and Validation Set
anomalous_validation_set_size <- round(0.2 * n_samples)  # 20% as validation set
anomalous_corpus_set_size <- n_samples - anomalous_validation_set_size

# Randomly sample indices for anomalous validation set
set.seed(123)
anomalous_validation_indices <- sample(1:n_samples, anomalous_validation_set_size)

# Create Anomalous Corpus and Validation Sets
anomalous_validation_set <- anomalous_data[anomalous_validation_indices, ]
anomalous_corpus_set <- anomalous_data[-anomalous_validation_indices, ]

# Visualize Anomalous Validation Set
heatmap(anomalous_validation_set, main = "Heatmap of Anomalous Validation Set", Colv = NA, Rowv = NA)
hist_anomalous_validation_set <- melt(anomalous_validation_set)
ggplot(hist_anomalous_validation_set, aes(x = value)) +
  geom_histogram(bins = 30, fill = "lightblue", alpha = 0.7) +
  theme_minimal() +
  labs(title = "Histogram of Anomalous Validation Set", x = "Values", y = "Frequency")

# Visualize Anomalous Corpus Set
heatmap(anomalous_corpus_set, main = "Heatmap of Anomalous Corpus Set", Colv = NA, Rowv = NA)
hist_anomalous_corpus_set <- melt(anomalous_corpus_set)
ggplot(hist_anomalous_corpus_set, aes(x = value)) +
  geom_histogram(bins = 30, fill = "lightgreen", alpha = 0.7) +
  theme_minimal() +
  labs(title = "Histogram of Anomalous Corpus Set", x = "Values", y = "Frequency")


library(ggplot2)
library(cluster)
library(factoextra)

set.seed(123)  # Ensure reproducibility

optimal_k_values <- numeric(3)  # Store optimal k values for each outer loop iteration
anomalous_distances <- numeric(3)   # Store anomalous distances for each iteration

# Outer loop - run 3 times
for (i in 1:3) {
  sampled_data_set1 <- data_set1[sample(1:nrow(data_set1), 33), ]
  sampled_data_set2 <- data_set2[sample(1:nrow(data_set2), 33), ]
  sampled_data_set3 <- data_set3[sample(1:nrow(data_set3), 33), ]
  
  combined_sampled_data <- rbind(sampled_data_set1, sampled_data_set2, sampled_data_set3)
  distance_matrix_non_anomalous <- dist(combined_sampled_data, method = "manhattan")
  pca_results_non_anomalous <- prcomp(as.matrix(distance_matrix_non_anomalous), center = TRUE, scale. = TRUE)
  
  #gap_stat <- clusGap(pca_results_non_anomalous$x, FUN = kmeans, nstart = 25, K.max = 10, B = 50)
  #optimal_k <- which.max(gap_stat$Tab[, "gap"])
  optimal_k = 3
  optimal_k_values[i] <- optimal_k
  
  kmeans_non_anomalous <- kmeans(combined_sampled_data, centers = optimal_k, nstart = 10)
  cluster_assignments <- kmeans_non_anomalous$cluster
  print(cluster_assignments)
  
  distances_inner_loop <- numeric(nrow(anomalous_validation_set))
  min_dists <- numeric(nrow(anomalous_validation_set))
  
  for (j in 1:nrow(anomalous_validation_set)) {
    current_anomalous_sample <- matrix(anomalous_validation_set[j, ], nrow = 1)
    combined_data_with_anomalous <- rbind(combined_sampled_data, current_anomalous_sample)
    
    pca_with_anomalous <- prcomp(as.matrix(dist(combined_data_with_anomalous, method = "manhattan")), center = TRUE, scale. = TRUE)
    
    # Plotting
    df <- data.frame(PC1 = pca_with_anomalous$x[,1], PC2 = pca_with_anomalous$x[,2])
    df$cluster <- c(cluster_assignments, "anomalous")
    #print(ggplot(df, aes(x = PC1, y = PC2, color = cluster)) +
    #        geom_point() +
    #        labs(title = "PCA plot with cluster assignments and anomalous sample", x = "PC1", y = "PC2") +
    #        theme_minimal())
    
    # Find the center for each cluster
    centroids <- aggregate(cbind(PC1, PC2) ~ cluster, df, mean)
    print("centroid")
    print(centroids)
    
    # Find the squared variance of each cluster...
    std_devs <- aggregate(cbind(PC1, PC2) ~ cluster, df, sd)
    print("stdev")
    print(std_devs)
    
    anomalous_centroid <- centroids[centroids$cluster == "anomalous", c("PC1", "PC2")]
    
    distances_in_std_devs <- numeric(optimal_k)
    
    # Loop over the first optimal_k clusters
    for (k in 1:optimal_k) {
      # Get the centroid and standard deviation of the current cluster
      current_cluster_centroid <- centroids[centroids$cluster == k, c("PC1", "PC2")]
      print(current_cluster_centroid)
      current_cluster_std_dev <- std_devs[std_devs$cluster == k, c("PC1", "PC2")]
      print(current_cluster_std_dev)
      
      # Compute the distance in standard deviations
      distances_in_std_devs[k] <- abs(anomalous_centroid - current_cluster_centroid) / current_cluster_std_dev
    }
    
    # Print the distances
    print("distance in std devs")
    print(distances_in_std_devs)
    print(min(unlist(distances_in_std_devs)))
    
    min_dists[j] <- min(unlist(distances_in_std_devs))
  }
  
  print("min dist")
  print(min_dists)
  
  anomalous_distances[i] <- mean(min_dists)
}

# Print the optimal k values and anomalous distances
print(optimal_k_values)
print(anomalous_distances)
print(mean(anomalous_distances))


# detect if a value is good or not.

detect <- function(data_sets, num_iterations, current_anomalous_sample, optimal_k = 3) {

  set.seed(123)  # Ensure reproducibility
  
  optimal_k_values <- numeric(num_iterations)  # Store optimal k values for each outer loop iteration
  anomalous_distances <- numeric(num_iterations)   # Store anomalous distances for each iteration
  
  # Outer loop - run num_iterations times
  for (i in 1:num_iterations) {
    sampled_data_sets <- lapply(data_sets, function(data_set) data_set[sample(1:nrow(data_set), 33), ])
    combined_sampled_data <- do.call(rbind, sampled_data_sets)
    
    distance_matrix_non_anomalous <- dist(combined_sampled_data, method = "manhattan")
    pca_results_non_anomalous <- prcomp(as.matrix(distance_matrix_non_anomalous), center = TRUE, scale. = TRUE)
    
    if (optimal_k == 0) {
      gap_stat <- clusGap(pca_results_non_anomalous$x, FUN = kmeans, nstart = 25, K.max = 10, B = 50)
      optimal_k <- which.max(gap_stat$Tab[, "gap"])
    }
    
    optimal_k_values[i] <- optimal_k
    
    kmeans_non_anomalous <- kmeans(combined_sampled_data, centers = optimal_k, nstart = 10)
    cluster_assignments <- kmeans_non_anomalous$cluster
    
    min_dists <- numeric(nrow(current_anomalous_sample))
    
    for (j in 1:nrow(current_anomalous_sample)) {
      current_sample <- matrix(current_anomalous_sample[j, ], nrow = 1)
      combined_data_with_anomalous <- rbind(combined_sampled_data, current_sample)
      
      pca_with_anomalous <- prcomp(as.matrix(dist(combined_data_with_anomalous, method = "manhattan")), center = TRUE, scale. = TRUE)
      
      df <- data.frame(PC1 = pca_with_anomalous$x[,1], PC2 = pca_with_anomalous$x[,2])
      df$cluster <- c(cluster_assignments, "anomalous")
      
      centroids <- aggregate(cbind(PC1, PC2) ~ cluster, df, mean)
      std_devs <- aggregate(cbind(PC1, PC2) ~ cluster, df, sd)
      
      anomalous_centroid <- centroids[centroids$cluster == "anomalous", c("PC1", "PC2")]
      
      distances_in_std_devs <- numeric(optimal_k)
      
      for (k in 1:optimal_k) {
        current_cluster_centroid <- centroids[centroids$cluster == k, c("PC1", "PC2")]
        current_cluster_std_dev <- std_devs[std_devs$cluster == k, c("PC1", "PC2")]
        
        distances_in_std_devs[k] <- abs(anomalous_centroid - current_cluster_centroid) / current_cluster_std_dev
      }
      
      min_dists[j] <- min(unlist(distances_in_std_devs))
    }
    
    anomalous_distances[i] <- mean(min_dists)
  }
  
  return(mean(anomalous_distances))
}

data_sets <- list(data_set1, data_set2, data_set3)
num_iterations <- 3
current_anomalous_sample <- anomalous_validation_set  # replace this with your actual anomalous sample

# Use 0 to force a single rebuild of the gap analysis
result <- detect(data_sets, num_iterations, current_anomalous_sample, 0)
print(result)




```
