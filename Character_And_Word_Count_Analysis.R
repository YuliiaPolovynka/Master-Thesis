
library(tidyverse)
library(httr)
library(igraph)
library(proxy)
library(stringr)
library(dplyr)
library(dbscan)
# PART 2

# CHARACTER AND WORD COUNT ANALYSIS
folder_path2 <- "/Users/uliapolovinka/Desktop/diplomova praca/spoken words"
files <- list.files(folder_path2, pattern = "\\.txt$", full.names = TRUE)
all_clusters <- readRDS("/Users/uliapolovinka/Desktop/clustering_all_methods.rds")
cluster_df <- all_clusters$ward.D2  

# Function to compute number of characters and words in a text file
get_text_stats <- function(file) {
  text <- readLines(file, encoding = "UTF-8", warn = FALSE)
  full_text <- paste(text, collapse = " ")
  
  n_chars <- nchar(full_text)
  n_words <- length(unlist(strsplit(full_text, "\\s+")))
  
  return(data.frame(
    File = basename(file),
    Characters = n_chars,
    Words = n_words,
    stringsAsFactors = FALSE
  ))
}

results <- do.call(rbind, lapply(files, get_text_stats))
results$OriginalID <- gsub("\\.spoken-text\\.txt$", "", results$File)

# shake000001-the-tempest.spoken-text.txt -> the-tempest
results$ID <- results$OriginalID
results$ID <- gsub("^shake[0-9]+-", "", results$ID)

# The Tempest -> the-tempest
cluster_df$ID <- tolower(cluster_df$Play)
cluster_df$ID <- gsub("[’']", "", cluster_df$ID)
cluster_df$ID <- gsub("'", "", cluster_df$ID)
cluster_df$ID <- gsub("[[:space:]]+", "-", cluster_df$ID)

# merging text statistics with clustering results
merged_df <- merge(
  results[, c("ID", "OriginalID", "Characters", "Words")],
  cluster_df[, c("ID", "Cluster")],
  by = "ID"
)

merged_df$ID <- merged_df$OriginalID
merged_df$OriginalID <- NULL
merged_df$Cluster <- as.factor(merged_df$Cluster)

final_df <- merged_df[, c("ID", "Cluster", "Characters", "Words")]
print(head(final_df))

saveRDS(final_df, file = "/Users/uliapolovinka/Desktop/text_analysis_clustering.rds")


summary_stats <- merged_df %>%
  group_by(Cluster) %>%
  summarise(
    mean_chars = mean(Characters),
    min_chars = min(Characters),
    max_chars = max(Characters),
    n = n(),
    mean_words = mean(Words),
    min_words = min(Words),
    max_words = max(Words) )
print(summary_stats)

# Boxplot: Characters per cluster
boxplot(Characters ~ Cluster, data = merged_df,
        xlab = "Cluster", ylab = "Characters", col = "lightblue")

# Boxplot: Words per cluster
boxplot(Words ~ Cluster, data = merged_df,
        main = "Number of Words by Cluster",
        xlab = "Cluster", ylab = "Words", col = "lightblue")

# One-way ANOVA: do clusters differ significantly in character counts?
anova_result <- aov(Characters ~ Cluster, data = merged_df)
summary(anova_result)  # p =0.00311 < 0.05 indicates statistically significant differences

# CLUSTERING: 1-COMPONENT PLAYS

ward1_list <- readRDS("C:/Users/Юля/Desktop/clusterings1.rds")
ward1_df <- ward1_list$ward.D2
ward1_df$ID <- gsub("\\.network\\.csv$", "", ward1_df$Play)

merged1 <- merge(results, ward1_df[, c("ID", "Cluster")], by = "ID")
merged1$Cluster <- as.factor(merged1$Cluster)

# Boxplot: Characters per cluster (1-component)
boxplot(Characters ~ Cluster, data = merged1,
        main = "Character Count (1-component plays)",
        xlab = "Cluster", ylab = "Characters", col = "lightgreen")

# ANOVA: test differences in character counts
anova_chars1 <- aov(Characters ~ Cluster, data = merged1)
summary(anova_chars1)  # p =0.00238  < 0.05  statistically significant


# CLUSTERING: 2-COMPONENT PLAYS

ward2_list <- readRDS("C:/Users/Юля/Desktop/clusterings2.rds")
ward2_df <- ward2_list$ward.D2
ward2_df$ID <- gsub("\\.network\\.csv$", "", ward2_df$Play)


merged2 <- merge(results, ward2_df[, c("ID", "Cluster")], by = "ID")
merged2$Cluster <- as.factor(merged2$Cluster)

# Boxplot: Characters per cluster (2-component)
boxplot(Characters ~ Cluster, data = merged2,
        main = "Character Count (2-component plays)",
        xlab = "Cluster", ylab = "Characters", col = "lightgreen")

# ANOVA: test differences in character counts
anova_chars2 <- aov(Characters ~ Cluster, data = merged2)
summary(anova_chars2)  # p=0.548 > 0.05 not significant


# GRAPH STRUCTURE STATS

graph_stats <- data.frame(
  ID = gsub("\\.network\\.csv$", "", basename(file_list)),
  Nodes = sapply(network_graphs, vcount),
  Edges = sapply(network_graphs, ecount),
  Density = sapply(network_graphs, edge_density))

# Combine with cluster assignments (from merged_df from earlier NetLSD clustering)
merged_all <- merge(merged_df, graph_stats, by = "ID")

# Boxplot: Graph density by cluster
boxplot(Density ~ Cluster, data = merged_all,
        main = "Graph Density by Cluster",
        xlab = "Cluster", ylab = "Density", col = "lightgray")

# ANOVA: test differences in density
anova_density <- aov(Density ~ Cluster, data = merged_all)
summary(anova_density)  # p =3.22e-08 < 0.05 significant

# MANOVA: test effect of cluster on multiple graph metrics
manova_result <- manova(cbind(Density, Nodes) ~ Cluster, data = merged_all)
summary(manova_result)  # p=5.875e-13 < 0.05  significant multivariate effect


# k-means on Characters + Words
set.seed(42)
kmeans_result <- kmeans(results[, c("Characters", "Words")], centers = 4, nstart = 25)
results$Cluster <- as.factor(kmeans_result$cluster)

drama_summary_kmeans_k4 <- results[, c("ID", "Cluster", "Characters", "Words")]

print(head(drama_summary_kmeans_k4))

saveRDS(
  drama_summary_kmeans_k4,
  file = "/Users/uliapolovinka/Desktop/drama_summary_kmeans_k4.rds")
