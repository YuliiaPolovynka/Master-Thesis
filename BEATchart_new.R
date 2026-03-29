library(tidyverse)
library(httr)
library(igraph)
library(proxy)
library(stringr)
library(dplyr)
library(dbscan)
library(dtw)

#PART 3 BEAT CHARTS
speech_data <- read_csv("~/Desktop/diplomova praca/speech_distr2.csv")
speech_data <- speech_data %>%
  separate(
    col = 1,
    into = c(
      "play_name", 
      "play_scene", 
      "scene_number", 
      "play_character_list", 
      "play_non_speaking_characters", 
      "play_character_count"
    ),
    sep = ";")

speech_data$scene_number <- as.numeric(speech_data$scene_number)
speech_data$play_character_count <- as.numeric(speech_data$play_character_count)

# Order of scenes
scene_levels <- c(
  "i", "ii", "iii", "iv", "v", "vi", "vii", "viii", "ix", "x",
  "xi", "xii", "xiii", "xiv", "xv", "xvi", "xvii", "xviii", "xix", "xx")

speech_data <- speech_data %>%
  mutate(
    scene_raw = gsub(".*\\.", "", play_scene),       # "i", "ii", ...
    act_raw = gsub("\\..*", "", gsub(".* ", "", play_scene))  # "I", "II", ...
  )

speech_data$act_num <- as.numeric(as.roman(speech_data$act_raw))

speech_data$scene_num <- factor(speech_data$scene_raw, levels = scene_levels)
speech_data$scene_num <- as.numeric(speech_data$scene_num)  

speech_data <- speech_data %>%
  arrange(play_name, act_num, scene_num)

speech_data <- speech_data %>%
  select(-scene_raw, -act_raw, -act_num, -scene_num)
speech_data <- speech_data %>%
  select(-scene_number)
speech_data <- speech_data %>%
  filter(
    !grepl("Rumour", play_character_list, ignore.case = TRUE),
    !grepl("Chorus", play_character_list, ignore.case = TRUE),
    !grepl("Prologue", play_character_list, ignore.case = TRUE) )

View(speech_data)

unique_plays <- unique(speech_data$play_name)

# Function to compute change rate between two consecutive scenes
segment_change <- function(characters1, characters2) {
  set1 <- unique(trimws(unlist(strsplit(characters1, ","))))
  set2 <- unique(trimws(unlist(strsplit(characters2, ","))))
  
  additions <- length(setdiff(set2, set1))     # characters added in scene 2
  deletions <- length(setdiff(set1, set2))     # characters removed in scene 2
  union_size <- length(union(set1, set2))      # total unique characters involved
  
  change_rate <- (additions + deletions) / union_size
  return(change_rate)
}

plot_beat_chart <- function(play_name_input) {
  df <- speech_data[speech_data$play_name == play_name_input, ]
    df <- df[order(df$play_scene), ]
    changes <- numeric(length = nrow(df) - 1)
  for (i in 1:(nrow(df) - 1)) {
    changes[i] <- segment_change(df$play_character_list[i], df$play_character_list[i + 1])
  }
  
  # calculation of mean and standard deviation of change rates
  mean_val <- mean(changes)
  sd_val <- sd(changes)
  
  plot(1:length(changes), changes, type = "o", col = "blue",
       main = paste("Beat Chart:", play_name_input),
       xlab = "Segment transition index", ylab = "Character change rate")
  
  # line showing average rate of change ("drama rate")
  abline(h = mean_val, col = "red")
  text(x = length(changes) - 1, y = mean_val + 0.05,
       labels = paste("Drama rate:", round(mean_val, 3)), col = "red")
  
  return(list(
    play_name = play_name_input,
    mean_change = mean_val,
    sd_change = sd_val,
    changes = changes))
}

# Plot Beat Chart for a specific play (for testing)
plot_beat_chart("The Tempest")

for (play in unique_plays) {
  plot_beat_chart(play)
}

drama_summary <- data.frame(
  play_name = character(),
  mean_change = numeric(),
  sd_change = numeric(),
  stringsAsFactors = FALSE)

# mean and standard deviation of character change rate per play
for (play in unique_plays) {
  result <- plot_beat_chart(play)
  drama_summary <- rbind(
    drama_summary,
    data.frame(
      play_name = play,
      mean_change = result$mean_change,
      sd_change = result$sd_change
    )
  )
}

# Global average standard deviation across all plays
global_mean_sd <- mean(drama_summary$sd_change)

# classifying plays into dynamic types based on variability in change rate
drama_summary$category <- ifelse(
  drama_summary$sd_change > global_mean_sd + 0.06, "high-dynamic",
  ifelse(
    drama_summary$sd_change < global_mean_sd - 0.06, "low-dynamic",
    "normal-type"
  )
)
table(drama_summary$category)

cat("HIGH-DYNAMIC:\n")
print(drama_summary$play_name[drama_summary$category == "high-dynamic"])

cat("NORMAL-TYPE:\n")
print(drama_summary$play_name[drama_summary$category == "normal-type"])

cat("LOW-DYNAMIC:\n")
print(drama_summary$play_name[drama_summary$category == "low-dynamic"])

ggplot(drama_summary, aes(x = mean_change, y = sd_change, color = category)) +
  geom_point(size = 2.5, alpha = 0.6) +
  labs(
    title = "Drama Types by Segment Change Rate",
    x = "Mean Segment-Change Rate",
    y = "Standard Deviation",
    color = "Type"
  ) +
  theme_classic() +
  scale_color_manual(values = c(
    "high-dynamic" = "red",
    "normal-type" = "green",
    "low-dynamic" = "blue"
  ))

# K-MEANS CLUSTERING BASED ON BEAT CHART METRICS
beat_features <- drama_summary %>%
  select(mean_change, sd_change) %>%
  scale() 


# k=4
kmeans_result_4 <- kmeans(beat_features, centers = 4, nstart = 25)
drama_summary$kmeans_cluster_4 <- factor(kmeans_result_4$cluster)

saveRDS(drama_summary, "drama_summary_kmeans_k4.rds")
ggplot(drama_summary, aes(x = mean_change, y = sd_change, color = kmeans_cluster_4)) +
  geom_point(size = 2.5, alpha = 0.7) +
  labs(
    title = "K-means Clustering of Plays (k = 4)",
    x = "Mean Segment-Change Rate",
    y = "Standard Deviation",
    color = "Cluster"
  ) +
  theme_minimal()

split(drama_summary$play_name, drama_summary$kmeans_cluster_4)



#DTW
library(dtw)
cymbeline_series <- plot_beat_chart("Cymbeline")$changes #low
macbeth_series <- plot_beat_chart("Macbeth")$changes #normal
hamlet_series <- plot_beat_chart("Hamlet")$changes #high
tempest_series <- plot_beat_chart("The Tempest")$changes #normal

# High vs Low-dynamic comparison
dtw_result <- dtw(hamlet_series, cymbeline_series, keep = TRUE)
print(dtw_result$distance)  # 4.235601

# Normal vs Low-dynamic
dtw_result <- dtw(macbeth_series, cymbeline_series, keep = TRUE)
print(dtw_result$distance) # 1.907792

# Normal vs Normal-dynamic
dtw_result <- dtw(macbeth_series, tempest_series, keep = TRUE)
print(dtw_result$distance)  # 2.042352


plays <- unique_plays  
beat_series_list <- list()
for (play in plays) {
  beat_series_list[[play]] <- plot_beat_chart(play)$changes
}
plays <- unique_plays  
n <- length(plays)
dtw_matrix <- matrix(0, nrow = n, ncol = n)
rownames(dtw_matrix) <- plays
colnames(dtw_matrix) <- plays
# Fill symmetric DTW matrix
for (i in 1:n) {
  for (j in i:n) {
    play_i <- plays[i]
    play_j <- plays[j]
    
    series1 <- beat_series_list[[play_i]]
    series2 <- beat_series_list[[play_j]]
    
    distance <- dtw(series1, series2)$distance
    
    dtw_matrix[play_i, play_j] <- distance
    dtw_matrix[play_j, play_i] <- distance  
  }
}

View(dtw_matrix)

max(dtw_matrix)  # 8.23754
min(dtw_matrix[dtw_matrix > 0])  #0.5253497

dtw_dist <- as.dist(dtw_matrix)
hc_dtw <- hclust(dtw_dist, method = "ward.D2")

plot(hc_dtw,
     main = "Hierarchical Clustering based on DTW distances",
     xlab = "", ylab = "DTW Distance",
     cex = 0.7)
k <- 4
dtw_clusters <- cutree(hc_dtw, k = k)

dtw_cluster_df <- data.frame(
  Play = names(dtw_clusters),
  Cluster = dtw_clusters)

saveRDS(dtw_cluster_df, file = "~/Desktop/dtw_clusters.rds")
print(dtw_cluster_df %>% arrange(Cluster))
saveRDS(speech_data, file = "~/Desktop/speech_data.rds")
