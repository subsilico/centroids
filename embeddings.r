# Creating 30 lines of hypothetical log data
set.seed(123)  # For reproducibility
log_data <- vector("list", 41)

# Generate some random log lines with varying characters
for (i in 1:30) {
  random_length <- sample(10:20, 1)  # Random length of each log line
  random_string <- paste0(sample(c(LETTERS, letters, 0:9, c(" ", "!", "@", "#")), random_length, replace = TRUE), collapse = "")
  log_data[[i]] <- random_string
}

# Add in some hand made synthetic logs.
log_data[[31]] <- "This is a randdom line of text."
log_data[[32]] <- "Error in grtx module: LIne 45 -- 404 ERROR."
log_data[[33]] <- "INFO: Systemd is now shutting down."
log_data[[34]] <- "Data error in users summitted matrix: 677675:09e-08"
log_data[[35]] <- "Restart required. Please save all work and restart your system ASAP."
log_data[[36]] <- "Subsequent attempts aborted, 543 times."
log_data[[37]] <- "Subsequent attempts aborted, 1544 times."
log_data[[38]] <- "Subsequent attempts aborted, 43 times."
log_data[[39]] <- "Subsequent attempts aborted, 144 times."
log_data[[40]] <- "Subsequent attempts aborted, 430 times."
log_data[[41]] <- "Subsequent attempts aborted, 1440 times."

# Print the first few lines to check
print(log_data[1:5])

#install.packages("devtools")
#library(devtools)
devtools::install_github("oscarkjell/text")

library(text)

# Install text required python packages in a conda environment (with defaults).
textrpp_install()

textrpp_initialize(save_profile = TRUE)

log_data[[1]]
embeddings1 <- textEmbed(log_data[[1]])
embeddings1

embeddings2 <- textEmbed(log_data[[2]])
embeddings2


library(ggplot2)

# Assuming the data is stored in a variable named embeddings2
# and that the embeddings for tokens are in embeddings2$tokens$texts[[1]]
# Convert the data to a data frame
embeddings_df <- as.data.frame(embeddings2$tokens$texts[[1]])

# Select the first three dimensions for simplicity (you can choose more or different dimensions)
embeddings_df_selected <- embeddings_df[, c("Dim1", "Dim2", "Dim3")]

# Create a long format data frame for ggplot
embeddings_long <- tidyr::gather(embeddings_df_selected, key = "Dimension", value = "Value")

# Plotting
ggplot(embeddings_long, aes(x = Dimension, y = Value)) +
  geom_boxplot() +
  labs(title = "Distribution of Embedding Dimensions",
       x = "Dimension",
       y = "Value") +
  theme_minimal()


# Convert to numeric matrix
fiveD1 = as.matrix(sapply(embeddings1$texts$texts, as.numeric))
fiveD2 = as.matrix(sapply(embeddings2$texts$texts, as.numeric))

# Now add them
result = fiveD1 + fiveD2

# Print the result
#print(result)
plot(result)



# Load necessary libraries
library(ggplot2)
library(dplyr)
library(tidyr)

# Assuming log_data is your list of log lines
num_log_lines <- length(log_data)

# Initialize an empty matrix to store embeddings
embedding_matrix <- matrix(nrow = num_log_lines, ncol = 768)  # Assuming we are using first x dimensions

# Compute embeddings for each log line and store in the matrix
for (i in 1:num_log_lines) {
  embeddings <- textEmbed(log_data[[i]])
  # Select the first five dimensions of the embedding
  embedding_matrix[i, ] <- sapply(embeddings$texts$texts, as.numeric)[1:768]
}

# Compute the Manhattan distance matrix
distance_matrix <- as.matrix(dist(embedding_matrix, method = "manhattan"))

# Print the distance matrix
#print(distance_matrix)

pca_of_logs <- prcomp(distance_matrix, center = TRUE, scale. = TRUE)

summary(pca_of_logs)


library(ggplot2)

# Extract the first two principal components
pca_data <- data.frame(pca_of_logs$x[, 1:2])
colnames(pca_data) <- c("PC1", "PC2")

# Plotting PC1 vs PC2 using ggplot2
ggplot(pca_data, aes(x = PC1, y = PC2)) +
  geom_point(aes(color = PC1), size = 3) +  # Color by PC1 values and adjust the size of points
  labs(title = "PCA of Log Data - First Two Principal Components",
       x = "Principal Component 1 (PC1)",
       y = "Principal Component 2 (PC2)") +
  theme_minimal() +
  scale_color_gradient(low = "blue", high = "red")  # Gradient color for better visualization
