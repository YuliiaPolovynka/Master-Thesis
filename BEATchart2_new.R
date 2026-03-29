# SPEECH DETECTION AND BEAT CHART - next part 
library(readr)
library(seewave)
library(ggplot2)
library(purrr)              
library(dplyr) 
install.packages("emdist")
library(emdist)  
install.packages("CVXR")
library(CVXR)
install.packages("archetypes")
install.packages("Anthropometry")
library(archetypes)
library(Anthropometry)

#speech_data <- readRDS("C:/Users/Юля/Desktop/speech_data.rds")
speech_data <- readRDS("~/Desktop/speech_data.rds")
unique_plays <- unique(speech_data$play_name)

segment_change <- function(characters1, characters2) {
  set1 <- unique(trimws(unlist(strsplit(characters1, ","))))
  set2 <- unique(trimws(unlist(strsplit(characters2, ","))))
  
  additions <- length(setdiff(set2, set1))     # characters added in scene 2
  deletions <- length(setdiff(set1, set2))     # characters removed in scene 2
  union_size <- length(union(set1, set2))      # total unique characters involved
  
  change_rate <- (additions + deletions) / union_size
  return(change_rate)
}

# Same as in the previous script
plot_beat_chart <- function(play_name_input) {
  df <- speech_data[speech_data$play_name == play_name_input, ]
  df <- df[order(df$play_scene), ]
  
  changes <- numeric(length = nrow(df) - 1)
  
  for (i in 1:(nrow(df) - 1)) {
    changes[i] <- segment_change(df$play_character_list[i], df$play_character_list[i + 1])
  }
  
  mean_val <- mean(changes)
  sd_val <- sd(changes)
  
  # BEAT CHART
  plot(1:length(changes), changes, type = "o", col = "blue",
       main = paste("Beat Chart:", play_name_input),
       xlab = "Segment transition no.", ylab = "Change rate")
  
  abline(h = mean_val, col = "red")
  
  text(x = length(changes) - 1, y = mean_val + 0.05,
       labels = paste("Drama rate:", round(mean_val, 3)), col = "red")
  
  return(list(
    play_name = play_name_input,
    mean_change = mean_val,
    sd_change = sd_val,
    changes = changes
  ))
}

plot_beat_chart("The Tempest") 

for (play in unique_plays) {
  plot_beat_chart(play)}

# Roughness

# Linear rescaling series to fixed length (100 points)
interpolated_series_new_length <- function(x, new_length = 100) {
  x_old <- seq(0, 1, length.out = length(x))
  x_new <- seq(0, 1, length.out = new_length)
  approx(x = x_old, y = x, xout = x_new)$y
}

interpolated_series_list <- list()
roughness_values <- data.frame(play_name = character(),
                               roughness = numeric(),
                               stringsAsFactors = FALSE)

# rerscaling each series and compute roughness
for (play in unique_plays) {
  original <- plot_beat_chart(play)$changes
  scaled <- interpolated_series_new_length(original, new_length = 100)
  interpolated_series_list[[play]] <- scaled
  
  rough <- roughness(scaled)
  
  roughness_values <- rbind(roughness_values, data.frame(
    play_name = play,
    roughness = rough
  ))
}

for (play in unique_plays) {
  plot(interpolated_series_list[[play]], type = "o", col = "green",
       main = paste(play, "(Interpolated)"),
       ylab = "Change rate", xlab = "Normalized time")
  abline(h = mean(interpolated_series_list[[play]]), col = "red", lty = 2)
}

roughness_values <- data.frame(
  play_name = unique_plays,
  roughness = sapply(unique_plays, function(play) roughness(interpolated_series_list[[play]])),
  stringsAsFactors = FALSE
)
grep("Henry", roughness_values$play_name, value = TRUE)
View(roughness_values)
# Historical order from older to newer
play_order <- c(
  "The Taming of the Shrew",
  "Henry VI Part 2",
  "Henry VI Part 3",
  "The Two Gentlemen of Verona",
  "Titus Andronicus",
  "Henry VI Part 1",
  "Richard III",
  "The Comedy of Errors",
  "Love's Labour's Lost",
  "A Midsummer Night's Dream",
  "Romeo and Juliet",
  "Richard II",
  "King John",
  "The Merchant of Venice",
  "Henry IV Part 1",
  "Henry IV Part 2",
  "Much Ado About Nothing",
  "Henry V",
  "As You Like It",
  "Julius Caesar",
  "Hamlet",
  "The Merry Wives of Windsor",
  "Twelfth Night",
  "Troilus and Cressida",
  "Othello",
  "Measure for Measure",
  "All's Well That Ends Well",
  "Timon of Athens",
  "King Lear",
  "Macbeth",
  "Antony and Cleopatra",
  "Coriolanus",
  "Pericles",
  "Cymbeline",
  "The Winter's Tale",
  "The Tempest",
  "Henry VIII")

roughness_values$play_name <- factor(
  roughness_values$play_name,
  levels = play_order)

roughness_values <- roughness_values[order(roughness_values$play_name), ]

plot(
  x = 1:nrow(roughness_values),
  y = roughness_values$roughness,
  pch = 19, col = "blue",
  xlab = "Play index (historical order)",
  ylab = "Roughness",
  main = "Roughness values across plays")

text(
  x = 1:nrow(roughness_values),
  y = roughness_values$roughness + 0.002,
  labels = roughness_values$play_name,
  cex = 0.6,
  pos = 3)



set.seed(123)

# k=4
kmeans_result_4 <- kmeans(roughness_values$roughness, centers = 4)
roughness_values$cluster_4 <- as.factor(kmeans_result_4$cluster)

saveRDS(roughness_values, file = "~/Desktop/roughness_clusters_4.rds")

# Plot in historical order

plot(
  x = 1:nrow(roughness_values),
  y = roughness_values$roughness,
  col = c("red", "blue", "pink", "black")[as.numeric(roughness_values$cluster_4)],
  pch = 19,
  cex = 1.3,
  xlab = "Play index (historical order)",
  ylab = "Roughness",
  ylim = c(0, max(roughness_values$roughness) + 0.05),
  xlim = c(0, nrow(roughness_values) + 1))

text(
  x = 1:nrow(roughness_values),
  y = roughness_values$roughness + 0.004,
  labels = roughness_values$play_name,
  cex = 0.5,
  pos = 3,
  xpd = NA)

# Spectral Entropy Analysis and Clustering

spectral_entropy_values <- data.frame(play_name = character(),
  spectral_entropy = numeric(),
  stringsAsFactors = FALSE)

# Compute spectral entropy for each interpolated beat sequence
for (play in unique_plays) {
  series <- interpolated_series_list[[play]]
  spectrum <- seewave::specprop(seewave::spec(series, f = 1, plot = FALSE), plot = FALSE)
  se <- spectrum$sh  # spectral entropy
  
  spectral_entropy_values <- rbind(spectral_entropy_values, data.frame(
    play_name = play,
    spectral_entropy = se ))
}

plot(
  x = 1:nrow(spectral_entropy_values),
  y = spectral_entropy_values$spectral_entropy,
  pch = 19,
  col = "purple",
  xlab = "Play index (historical order)",
  ylab = "Spectral entropy",
  ylim = c(0, max(spectral_entropy_values$spectral_entropy) + 0.05),
  xlim = c(0, nrow(spectral_entropy_values) + 1))

text(
  x = 1:nrow(spectral_entropy_values),
  y = spectral_entropy_values$spectral_entropy + 0.004,
  labels = spectral_entropy_values$play_name,
  cex = 0.6,
  pos = 3,
  xpd = NA)

# K-means clustering based on spectral entropy
set.seed(42)

# k=4
kmeans_entropy_4 <- kmeans(spectral_entropy_values$spectral_entropy, centers = 4)
spectral_entropy_values$cluster_4 <- as.factor(kmeans_entropy_4$cluster)

# Save clustering results (k = 4)
saveRDS(spectral_entropy_values, "~/Desktop/data_spectral_entropy_clusters_4.rds")
plot(
  x = 1:nrow(spectral_entropy_values),
  y = spectral_entropy_values$spectral_entropy,
  col = spectral_entropy_values$cluster_4,
  pch = 19,
  xlab = "Play index (historical order)",
  ylab = "Spectral Entropy",
  xlim = c(-1, nrow(spectral_entropy_values) + 2),  
  ylim = c(0, max(spectral_entropy_values$spectral_entropy) + 0.06))

text(
  x = 1:nrow(spectral_entropy_values),
  y = spectral_entropy_values$spectral_entropy + 0.012,
  labels = spectral_entropy_values$play_name,
  cex = 0.6,
  pos = 3,
  xpd = NA)

# Rough + Entripy
combined_df <- merge(
  roughness_values[, c("play_name", "roughness")],
  spectral_entropy_values[, c("play_name", "spectral_entropy")],
  by = "play_name")
scaled_data <- scale(combined_df[, c("roughness", "spectral_entropy")])

#k=4
kmeans_combined_4 <- kmeans(scaled_data, centers = 4)
combined_df$cluster_4 <- as.factor(kmeans_combined_4$cluster)
saveRDS(combined_df, "~/Desktop/data_roughness_entropy_clusters_4.rds")

# EMD
# Function for computing EMD using convex optimization
install.packages("ECOSolveR")
library(CVXR)
kantorovich_CVX <- function(
    mu, nu, dist=NULL, solution=FALSE, stop_if_fail=TRUE, solver = "ECOS", ...
){
  m <- length(mu)
  n <- length(nu)
  if(sum(mu)!=1 || sum(nu)!=1 || any(mu<0) || any(nu<0)){
    message("Warning: mu and/or nu are not probability measures")
  }
  if(is.null(dist)) dist <- 1 - diag(m)
  
  obj <- c(t(dist))
  A <- rbind(
    t(model.matrix(~0+gl(m, n)))[,],
    t(model.matrix(~0+factor(rep(1:n, m))))[,]
  )
  
  x <- Variable(m * n)
  objective <- Minimize(t(obj) %*% x)
  constraints <- list(x >= 0, A %*% x == c(mu, nu))
  problem <- Problem(objective, constraints)
  
  kanto <- solve(problem, solver = solver, ...)
  
  if(kanto$status != "optimal"){
    if(stop_if_fail){
      stop(sprintf("No optimal solution found: status %s \n", kanto$status))
    }else{
      warning(sprintf("No optimal solution found: status %s \n", kanto$status))
      return(kanto)
    }
  }
  
  out <- kanto$value
  if(solution) attr(out, "solution") <- matrix(kanto$getValue(x), nrow = m, byrow = TRUE)
  out
}

n <- length(unique_plays)
EMD_kant <- matrix(0, n, n)
EMD_classic <- matrix(0, n, n)
rownames(EMD_kant) <- colnames(EMD_kant) <- unique_plays
rownames(EMD_classic) <- colnames(EMD_classic) <- unique_plays

changes_list <- list()
for (play in unique_plays) {
  changes_list[[play]] <- plot_beat_chart(play)$changes
}
get_signature <- function(series) {
  tbl <- table(series)
  pos <- as.numeric(names(tbl))
  w <- as.numeric(tbl) / sum(tbl)
  list(pos = pos, w = w)
}



for (i in 1:n) {
  for (j in i:n) {
    play_i <- unique_plays[i]
    play_j <- unique_plays[j]
    
    s1 <- changes_list[[play_i]]
    s2 <- changes_list[[play_j]]
    
    if (length(s1) == 0 || length(s2) == 0) next
    
    sig1 <- get_signature(s1)
    sig2 <- get_signature(s2)
    
    M <- outer(sig1$pos, sig2$pos, FUN = function(a, b) abs(a - b))
    
    if (all(M == 0)) {
      cat("Matrix of distances is all zeros for pair:", play_i, "vs", play_j, "\n")
      print(sig1$pos)
      print(sig2$pos)
    }
    
    emd_k <- kantorovich_CVX(sig1$w, sig2$w, dist = M)
    
    mat1 <- cbind(sig1$w, sig1$pos)
    mat2 <- cbind(sig2$w, sig2$pos)
    emd_c <- emd(mat1, mat2)
    
    EMD_kant[i, j] <- emd_k
    EMD_kant[j, i] <- emd_k
    EMD_classic[i, j] <- emd_c
    EMD_classic[j, i] <- emd_c
  }
}


View(EMD_kant)
View(EMD_classic)
check <- round(EMD_kant - EMD_classic, 4)
View(check)

emd_dist <- as.dist(EMD_classic)
hcl <- hclust(emd_dist, method = "ward.D2")
plot(hcl, cex = 0.8)



cut_clusters <- cutree(hcl, k = 4)

cluster_df_emd <- data.frame(
  Play = names(cut_clusters),
  Cluster = cut_clusters)
saveRDS(cluster_df_emd, file = "~/Desktop/emd_wardD2_clusters.rds")

# beat chart to histogram 
plot_beat_chart("Hamlet") 
s <- changes_list[["Hamlet"]]
sig <- get_signature(s)
pos <- sig$pos
w   <- sig$w

lab_pos <- format(round(pos, 2), nsmall = 2, trim = TRUE)

par(mar = c(5, 4, 3, 1) + 0.2)
barplot(
  height = w,
  names.arg = lab_pos,
  border = NA,
  col = "#5DA5DA",              
  xlab = "Beat position (change rate)",
  ylab = "Relative weight",
  las = 2,                      
  cex.names = 0.8               
)
box(bty = "l")


# Archetypes
# Create matrix from rescaled series
interpolated_matrix <- do.call(rbind, interpolated_series_list)
rownames(interpolated_matrix) <- names(interpolated_series_list)  # Assign play names
View(interpolated_matrix)
# Fit archetypal models for k = 1 to 10 (5 repetitions each)
set.seed(123)
aa <- stepArchetypes(interpolated_matrix, k = 1:10, nrep = 5)

# screeplot to choose number of archetypes - 4
screeplot(aa)

aa_4 <- bestModel(aa[[4]])

archetypes_mat <- parameters(aa_4)

matplot(t(archetypes_mat), type = "l", lty = 1, col = 1:4, lwd =2,
        main = "Archetypes for Interpolated Beat Charts",
        ylab = "Change Rate", xlab = "Normalized Time")
legend("topright", legend = paste("Archetype", 1:4), col = 1:4, lty = 1)

# archetype coefficients (how each play is composed of archetypes)
alpha_mat <- coef(aa_4)
beta <- coef(aa_4, "betas")
rowSums(beta)
# assignin each play to the most dominant archetype (highest weight)
cluster_assignment <- max.col(alpha_mat)
table(cluster_assignment)

barplot(table(cluster_assignment),
        col = rainbow(4),
        main = "Number of Plays per Archetype Cluster",
        xlab = "Archetype Cluster", ylab = "Number of Plays")

# play closest to each archetype
closest_plays <- apply(alpha_mat, 2, which.max)
rownames(interpolated_matrix)[closest_plays]
#[1] "Othello"                    "The Merry Wives of Windsor"
#[3] "Henry V"                    "Pericles"  

cluster_df_interpolated_beat_charts <- data.frame(
  play = rownames(interpolated_matrix),
  archetype_cluster = cluster_assignment)

# Save results to RDS file
saveRDS(cluster_df_interpolated_beat_charts, file = "~/Desktop/archetype_clusters.rds")
# For archetypes from beat charts
alpha_matrix <- coef(aa_4)
rowSums(alpha_matrix)  #all ones
colSums(alpha_matrix)  

# Archetypal Analysis: Roughness + Spectral Entropy

# Combine roughness and spectral entropy data
combined_df <- merge(roughness_values, spectral_entropy_values, by = "play_name")

combined_matrix <- as.matrix(combined_df[, c("roughness", "spectral_entropy")])
rownames(combined_matrix) <- combined_df$play_name

set.seed(123)
arch_models <- stepArchetypes(combined_matrix, k = 1:10, nrep = 5)

screeplot(arch_models)

# Select model with 3 archetypes
best_arch <- bestModel(arch_models[[3]])

xyplot(best_arch, combined_matrix, chull = chull(combined_matrix))  
xyplot(best_arch, combined_matrix, adata.show = TRUE)
alpha_matrix <- coef(best_arch)
assigned_cluster <- max.col(alpha_matrix) #assigning by dominant coefficient

barplot(table(assigned_cluster),
        col = rainbow(length(unique(assigned_cluster))),
        main = "Number of Plays per Archetype Cluster",
        xlab = "Archetype Cluster", ylab = "Number of Plays")

plot(combined_matrix, col = assigned_cluster, pch = 19,
     xlab = "Roughness", ylab = "Spectral Entropy",
     main = "Archetypal Clustering (Roughness + Entropy)")
legend("topright", legend = unique(assigned_cluster), col = unique(assigned_cluster), pch = 19)

arch_cluster_df_rough_entr <- data.frame(
  play_name = rownames(combined_matrix),
  cluster = assigned_cluster)

saveRDS(arch_cluster_df_rough_entr, file = "roughness_entropy_archetypes.rds")

#  Archetypal analysis with 4 archetypes
best_arch_4 <- bestModel(arch_models[[4]])
alpha_matrix_4 <- coef(best_arch_4)
assigned_cluster_4 <- max.col(alpha_matrix_4)
arch_cluster_df_rough_entr_4 <- data.frame(
  play_name = rownames(combined_matrix),
  cluster_4 = assigned_cluster_4)

saveRDS(arch_cluster_df_rough_entr_4, file = "roughness_entropy_archetypes_4.rds")


# Archetypoids with Anthropometry 

interpolated_matrix <- do.call(rbind, interpolated_series_list)
rownames(interpolated_matrix) <- names(interpolated_series_list)

set.seed(123)
numArch <- 4  
numRep <- 5   
install.packages("Anthropometry")
options(rgl.useNULL = TRUE)
library("Anthropometry")
lass <- stepArchetypesRawData(data = interpolated_matrix, numArch = 1:numArch, numRep = numRep)


res_archetypoids <- stepArchetypoids(numArchoid = 4,
                                     nearest = "cand_ns",
                                     data = interpolated_matrix,
                                     ArchObj = lass)

selected_ids <- res_archetypoids$cases
selected_plays <- rownames(interpolated_matrix)[selected_ids]
print(selected_plays)


alpha_matrix <- res_archetypoids$alphas 
clusters_archetypoids <- max.col(t(alpha_matrix))

cluster_df_archetypoids <- data.frame(
  play_name = rownames(interpolated_matrix),
  cluster = clusters_archetypoids)

saveRDS(cluster_df_archetypoids, "~/Desktop/beat_chart_archetypoids_clusters.rds")
selected_series <- interpolated_series_list[selected_plays]

data_mat <- do.call(rbind, selected_series)

matplot(t(data_mat), type = "l", lty = 1, lwd = 2, col = 1:4,
        xlab = "Normalized Time", ylab = "Change Rate")

legend("bottomright", legend = selected_plays, col = 1:4, lty = 1, lwd = 2)
cluster_assignment2 <- max.col(t(alpha_matrix))
table(cluster_assignment2) 
table(cluster_assignment)
table(cluster_assignment,cluster_assignment2 )
