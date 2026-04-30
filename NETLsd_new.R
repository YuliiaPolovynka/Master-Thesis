library(tidyverse)
library(httr)
library(igraph)
library(proxy)

# PART 1 NETLsd
folder_path <- "/Users/uliapolovinka/Desktop/diplomova praca 2/networks"
file_list <- list.files(folder_path, pattern = "\\.csv$", full.names = TRUE)

# Function to load a graph from a CSV file
load_network <- function(file) {
  my_data <- read_csv(file)
  my_data <- my_data[, c("Source", "Target", "Weight")]
  g <- graph_from_data_frame(my_data, directed = FALSE)
  return(g)
}

# Load all networks into a list of igraph objects
network_graphs <- lapply(file_list, load_network)

# Check if each network is connected
connected_status <- sapply(network_graphs, is.connected)

# Count the number of components in each network
comps <- sapply(network_graphs, count_components)

# Function to compute NetLSD signature vector for a graph
compute_netlsd <- function(g) {
  # Create weighted adjacency matrix
  A <- as_adjacency_matrix(g, attr = "Weight", sparse = FALSE)
  
  # Degree matrix
  D <- diag(rowSums(A))
  
  # Normalized Laplacian: L = I - D^(-1/2) * A * D^(-1/2)
  D_sqrt_inv <- diag(1 / sqrt(diag(D)))
  I <- diag(nrow(A))
  L_norm <- I - D_sqrt_inv %*% A %*% D_sqrt_inv
  
  # Eigenvalues of normalized Laplacian
  eigenvalues <- sort(eigen(L_norm, only.values = TRUE)$values)
  
  # Define diffusion time values (log scale)
  t_values <- exp(seq(log(0.01), log(10), length.out = 100))
  
  # Compute NetLSD signature (heat trace)
  netlsd_vector <- sapply(t_values, function(t) sum(exp(-t * eigenvalues)))
  
  return(netlsd_vector)
}

t_values1 <- exp(seq(log(0.01), log(10), length.out = 100))

# computing NetLSD signature for each graph
netlsd_results <- lapply(network_graphs, compute_netlsd)
netlsd_matrix <- do.call(rbind, netlsd_results)
print(dim(netlsd_matrix))
rownames(netlsd_matrix) <- basename(file_list)
colnames(netlsd_matrix) <- paste0("t_", seq_along(t_values1))

# Plot NetLSD signatures of all plays
graphics.off()

par(mar = c(5, 6, 4, 2))

plot(
  x = t_values1,
  y = netlsd_matrix[1, ],
  type = "n",
  log = "x",
  xlim = c(0.01, 10),
  ylim = c(0, max(netlsd_matrix)),
  xaxt = "n",
  yaxt = "n",
  xlab = "logaritmic scale",
  ylab = expression(h[t]))

matlines(
  x = t_values1,
  y = t(netlsd_matrix),
  lty = 1)

axis(
  side = 1,
  at = c(0.01, 0.1, 1, 10),
  labels = expression(10^{-2}, 10^{-1}, 10^{0}, 10^{1}))

axis(
  side = 2,
  at = c(20, 40, 60),
  labels = c("20", "40", "60"),
  las = 1)

segments(0.01, 1, 10, 1, col = "grey60", lty = 2)
segments(0.01, 2, 10, 2, col = "grey60", lty = 2)
text(0.012, 1 - 1, expression(h[t] == 1),
     pos = 4, cex = 0.8, col = "grey40")

text(0.012, 2 + 1, expression(h[t] == 2),
     pos = 4, cex = 0.8, col = "grey40")

# Function to convert file names to readable play titles
clean_play_name <- function(x) {
  x <- basename(x)                              
  x <- sub("^shake\\d+-", "", x)               # remove prefix shake000015-
  x <- sub("\\.network\\.csv$", "", x)         # remove suffix
  x <- gsub("-", " ", x)                       # replace - with space
  x <- tools::toTitleCase(x)                   # title case
  
  # manual fixes for Roman numerals and common words
  x <- gsub("Iv", "IV", x)
  x <- gsub("Vi", "VI", x)
  x <- gsub("Viii", "VIII", x)
  x <- gsub("Ii", "II", x)
  x <- gsub("Iii", "III", x)
  
  return(x)
}

names <- sapply(file_list, clean_play_name)
# Compute distance matrix between plays based on NetLSD vectors
distance_matrix <- as.matrix(dist(netlsd_matrix, method = "euclidean"))
rownames(distance_matrix) <- names
colnames(distance_matrix) <- names

#Hierarchical clustering for all plays
all_methods <- c("single", "complete", "average", "ward.D2")
all_clusters <- list()

for (m in all_methods) {
  hc <- hclust(as.dist(distance_matrix), method = m)
  plot(hc,
       labels = names,
       main = paste("All plays - method:", m),
       cex = 0.7,
       las = 2)
  k <- 4  # Can be dynamically selected later
  cl <- cutree(hc, k)
  all_clusters[[m]] <- data.frame(Play = rownames(distance_matrix), Cluster = cl)
}
saveRDS(all_clusters, file = "~/Desktop/clustering_all_methods.rds")

# cluster plays with 1 or 2 components separately

# Indexes of 1- and 2-component networks
idx1comp <- which(comps == 1)
idx2comp <- which(comps == 2)

cluster_by_group <- function(idx_vec, group_label, k_values, file_out) {
  dist_group <- as.dist(distance_matrix[idx_vec, idx_vec])
  results_list <- list()
  
  for (m in all_methods) {
    hc <- hclust(dist_group, method = m)
    plot(hc, main = paste(group_label, "- method:", m), cex = 0.7, las = 2)
    k <- k_values[[m]]
    cl <- cutree(hc, k = k)
    df <- data.frame(
      Play = rownames(distance_matrix)[idx_vec],
      Cluster = cl
    )
    results_list[[m]] <- df
  }
  
  #write_xlsx(results_list, path = file_out)
  saveRDS(results_list, file = file_out)
  
}

# Define number of clusters for each method
k_values_1comp <- list(single = 5, complete = 6, average = 5, ward.D2 = 5)
k_values_2comp <- list(single = 3, complete = 3, average = 3, ward.D2 = 3)

cluster_by_group(idx1comp, "1-component", k_values_1comp,
                 file = "~/Desktop/clusterings1.rds")

cluster_by_group(idx2comp, "2-component", k_values_2comp,
                 file = "~/Desktop/clusterings2.rds")


