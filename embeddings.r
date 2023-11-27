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



# Function to encode alphabetic category with character values
encode_alpha_cat <- function(char) {
  if (char %in% LETTERS) {
    return(c(1, match(char, LETTERS)))
  } else if (char %in% letters) {
    return(c(2, match(char, letters)))
  } else if (char %in% 0:9) {
    return(c(3, as.numeric(char) + 1))  # Shift numbers by 1
  } else {
    return(c(4, NA))  # Special characters
  }
}

# Function to encode numerical axis
encode_numerical_axis <- function(char) {
  if (char %in% 0:9) {
    return(as.numeric(char))
  } else {
    return(0)  # Non-numbers as zero
  }
}

# Function to encode formatting category
encode_formatting_cat <- function(char) {
  if (char %in% LETTERS) {
    return(1)  # Uppercase
  } else if (char %in% letters) {
    return(2)  # Lowercase
  } else if (char %in% c("!", "@", "#", "$", "%", "^", "&", "*", "(", ")", "-", "+")) {
    return(3)  # Punctuation and special characters
  } else {
    return(4)  # Whitespace and others
  }
}

# Function to encode sequential position with resets
encode_seq_pos <- function(string) {
  pos <- 0
  sapply(strsplit(string, "")[[1]], function(char) {
    if (!grepl("[a-zA-Z0-9]", char)) {
      pos <<- 0  # Reset at non-alphanumeric
      return(NA)
    }
    pos <<- pos + 1
    return(pos)
  })
}

# Applying the encoding functions to the log data
encoded_log_data <- lapply(log_data, function(line) {
  chars <- strsplit(line, "")[[1]]
  alpha_cat <- sapply(chars, encode_alpha_cat)
  num_axis <- sapply(chars, encode_numerical_axis)
  format_cat <- sapply(chars, encode_formatting_cat)
  seq_pos <- encode_seq_pos(line)
  return(list(alpha_cat = alpha_cat, num_axis = num_axis, format_cat = format_cat, seq_pos = seq_pos))
})

# Assuming 'encoded_log_data' is your list of encoded log data
for (i in seq_along(encoded_log_data)) {
  # Replace NA with 0 in each subcomponent
  encoded_log_data[[i]]$alpha_cat[is.na(encoded_log_data[[i]]$alpha_cat)] <- 0
  encoded_log_data[[i]]$num_axis[is.na(encoded_log_data[[i]]$num_axis)] <- 0
  encoded_log_data[[i]]$format_cat[is.na(encoded_log_data[[i]]$format_cat)] <- 0
  encoded_log_data[[i]]$seq_pos[is.na(encoded_log_data[[i]]$seq_pos)] <- 0
}


# Print encoded data of the first line
print(encoded_log_data[[1]]$alpha_cat[2,])
print(encoded_log_data[[2]]$alpha_cat[2,])


# Assuming 'encoded_log_data' is your list of encoded log data
library(ggplot2)
library(tidyr)

# Prepare the data for plotting
plot_data <- do.call(rbind, lapply(seq_along(encoded_log_data), function(i) {
  data <- data.frame(Line = rep(i, each = length(encoded_log_data[[i]]$alpha_cat[2,])),
                     CharIndex = 1:length(encoded_log_data[[i]]$alpha_cat[2,]),
                     AlphaCatValue = encoded_log_data[[i]]$alpha_cat[2,],
                     NumAxisValue = encoded_log_data[[i]]$num_axis,
                     FormatCatValue = encoded_log_data[[i]]$format_cat,
                     SeqPosValue = encoded_log_data[[i]]$seq_pos)
  data$Line <- as.factor(data$Line) # Convert Line to a factor for better plotting
  return(data)
}))

# Reshape data to long format for plotting
long_data <- gather(plot_data, key = "Category", value = "Value", -Line, -CharIndex)

# Plot using ggplot2
ggplot(long_data, aes(x = CharIndex, y = Value, group = interaction(Line, Category), color = Category)) +
  geom_line() +
  facet_wrap(~Line, scales = "free_y") + # Separate plots for each line
  labs(title = "Encoded Log Data Analysis",
       x = "Character Index in Line",
       y = "Value",
       color = "Category") +
  theme_minimal()




# Assuming 'encoded_log_data' is your list of encoded log data
library(ggplot2)

# Prepare the data for plotting
plot_data <- do.call(rbind, lapply(seq_along(encoded_log_data), function(i) {
  data <- data.frame(Line = i,
                     CharIndex = 1:length(encoded_log_data[[i]]$alpha_cat[2,]),
                     AlphaCatValue = encoded_log_data[[i]]$alpha_cat[2,])
}))

# Plot using ggplot2
ggplot(plot_data, aes(x = CharIndex, y = AlphaCatValue, group = Line, color = as.factor(Line))) +
  geom_line() +
  labs(title = "Alpha Category Values in Log Data",
       x = "Character Index in Line",
       y = "Alpha Category Value",
       color = "Log Line Number") +
  theme_minimal()



### Now plot each line against the envelop created by all the other samples taken as background.

library(ggplot2)
library(dplyr)

# Compute the envelope (min and max) for each character index
envelope <- plot_data %>%
  group_by(CharIndex) %>%
  summarize(MinValue = min(AlphaCatValue), MaxValue = max(AlphaCatValue))

# Function to create a plot for each line with the envelope as background
plot_each_line <- function(line_number, plot_data, envelope) {
  single_line_data <- plot_data %>% filter(Line == line_number)
  
  ggplot() +
    geom_ribbon(data = envelope, aes(x = CharIndex, ymin = MinValue, ymax = MaxValue), fill = "gray", alpha = 0.3) +
    geom_line(data = single_line_data, aes(x = CharIndex, y = AlphaCatValue, color = as.factor(Line))) +
    labs(title = paste("Log Line", line_number),
         x = "Character Index in Line",
         y = "Alpha Category Value") +
    theme_minimal() +
    theme(legend.position = "none")
}

# Plot for each line and store in a list
plot_list <- lapply(unique(plot_data$Line), function(line) plot_each_line(line, plot_data, envelope))
print(plot_list)

# If you want to arrange all plots in a grid, use the following:
# If you have patchwork installed, you can use it to arrange plots.
# install.packages("patchwork")
library(patchwork)
wrap_plots(plot_list)


library(ggplot2)
library(tidyr)

# Assuming 'encoded_log_data' is your list of encoded log data
# Convert the list of dictionaries to a DataFrame
# ... [Code to convert encoded_log_data to DataFrame 'plot_data'] ...

# Reshape data to long format for plotting
long_data <- gather(plot_data, key = "Category", value = "Value", -Line, -CharIndex)

# Plot using ggplot2
ggplot(long_data, aes(x = CharIndex, y = Value, group = interaction(Line, Category), color = Category)) +
  geom_line() +
  facet_wrap(~Line, scales = "free_y") + # Separate plots for each line
  labs(title = "Encoded Log Data Analysis",
       x = "Character Index in Line",
       y = "Value",
       color = "Category") +
  theme_minimal()


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
