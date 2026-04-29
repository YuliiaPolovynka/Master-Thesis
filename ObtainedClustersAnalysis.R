library(dplyr)
library(mclust)
library(reshape2)
library(aricode)

list.files(path = "/Users/uliapolovinka/Desktop/", pattern = "\\.rds$", full.names = TRUE)

clustering_all_methods <- readRDS("/Users/uliapolovinka/Desktop/clustering_all_methods.rds")
clusterings1 <- readRDS("/Users/uliapolovinka/Desktop/clusterings1.rds")
clusterings2 <- readRDS("/Users/uliapolovinka/Desktop/clusterings2.rds")

dtw_clusters <- readRDS("/Users/uliapolovinka/Desktop/dtw_clusters.rds")

speech_data <- readRDS("/Users/uliapolovinka/Desktop/speech_data.rds")


getwd()
list.files(pattern = "\\.rds$")
archetype_clusters <- readRDS("/Users/uliapolovinka/Desktop/archetype_clusters.rds")
emd_wardD2_clusters <- readRDS("/Users/uliapolovinka/Desktop/emd_wardD2_clusters.rds")
archetypoids <- readRDS("/Users/uliapolovinka/Desktop/beat_chart_archetypoids_clusters.rds")
drama_summary_kmeans_k4 <- readRDS("/Users/uliapolovinka/Desktop/drama_summary_kmeans_k4.rds")
roughness_clusters_4 <- readRDS("/Users/uliapolovinka/Desktop/roughness_clusters_4.rds")
data_spectral_entropy_clusters_4 <- readRDS("/Users/uliapolovinka/Desktop/data_spectral_entropy_clusters_4.rds")
data_roughness_entropy_clusters_4 <- readRDS("/Users/uliapolovinka/Desktop/data_roughness_entropy_clusters_4.rds")

# test
clustering_all_methods

convert_filename_to_title <- function(filename) {
  name_clean <- gsub("shake[0-9]+-", "", filename)
  name_clean <- gsub("\\.network\\.csv$", "", name_clean)
  words <- unlist(strsplit(name_clean, "-"))
  
  title <- paste(toupper(substring(words, 1, 1)), substring(words, 2), sep = "")
  title <- paste(title, collapse = " ")
  
  title <- gsub("\\bIi\\b", "II", title)
  title <- gsub("\\bIii\\b", "III", title)
  title <- gsub("\\bIv\\b", "IV", title)
  title <- gsub("\\bV\\b", "V", title)
  title <- gsub("\\bVi\\b", "VI", title)
  title <- gsub("\\bVii\\b", "VII", title)
  title <- gsub("\\bViii\\b", "VIII", title)
  title <- gsub("\\bIx\\b", "IX", title)
  title <- gsub("\\bX\\b", "X", title)
  
  return(title)
}

clustering_all_methods_clean <- lapply(clustering_all_methods, function(df) {
  df$play_name <- sapply(df$Play, convert_filename_to_title)  
  df <- df[, c("play_name", "Cluster")]                       
  colnames(df) <- c("play_name", "cluster")                  
  return(df)
})
text_analysis_clustering <- text_analysis_clustering %>%
  mutate(play_name = sapply(ID, convert_filename_to_title)) %>%
  select(play_name, cluster = Cluster)
head(clustering_all_methods_clean$single)

netlsd_average <- clustering_all_methods$average
netlsd_wardD2  <- clustering_all_methods$wardD2
netlsd_single  <- clustering_all_methods$single
netlsd_complete <- clustering_all_methods$complete



df1 <- clustering_all_methods_clean$average
df2 <- data_spectral_entropy_clusters_4
comparison_df <- inner_join(df1, df2, by = "play_name")

ari_score <- adjustedRandIndex(comparison_df$cluster, comparison_df$cluster_4)
print(paste("Adjusted Rand Index =", round(ari_score, 3)))

# ARI for clusters
#cluster list with 4 clusters
cluster_list4 <- list(
  netlsd_wardD2 = clustering_all_methods_clean$ward.D2,
  statistics = drama_summary_kmeans_k4,
  dtw = dtw_clusters,
  archetype = archetype_clusters,
  roughness_entropy4 = data_roughness_entropy_clusters_4,
  spectral_entropy4 = data_spectral_entropy_clusters_4,
  emd = emd_wardD2_clusters,
  roughness4 = roughness_clusters_4,
  archetypoid =archetypoids )
cluster_list4$statistics <- cluster_list4$statistics %>%
  select(play_name, cluster = kmeans_cluster_4)

cluster_list4$statistics$play_name_match <- normalize_play_name(cluster_list4$statistics$play_name)


cluster_list4 <- lapply(cluster_list4, function(df) {
  df <- df %>% rename(cluster = tidyselect::any_of(c("Cluster", "archetype_cluster")))
  df
})

cluster_list4 <- lapply(cluster_list4, function(df) {
  name_cols <- names(df)
  
  if ("play_name" %in% name_cols) {
    play_col <- "play_name"
  } else if ("Play" %in% name_cols) {
    play_col <- "Play"
  } else if ("play" %in% name_cols) {
    play_col <- "play"
  } else if ("ID" %in% name_cols) {
    play_col <- "ID"
  } else {
    play_col <- NA
  }
  
  if (!is.na(play_col) && play_col != "play_name") {
    names(df)[names(df) == play_col] <- "play_name"
  }
  
  if ("cluster" %in% names(df)) {
    cluster_col <- "cluster"
  } else if ("Cluster" %in% names(df)) {
    cluster_col <- "Cluster"
  } else if ("cluster_4" %in% names(df)) {
    cluster_col <- "cluster_4"
  } else if ("archetype_cluster" %in% names(df)) {
    cluster_col <- "archetype_cluster"
  } else {
    cluster_col <- NA
  }
  
  if (!is.na(cluster_col) && cluster_col != "cluster") {
    names(df)[names(df) == cluster_col] <- "cluster"
  }
  
  df
})

normalize_play_name <- function(x) {
  x <- tolower(as.character(x))
  x <- gsub("’", "", x)
  x <- gsub("'", "", x)
  x <- gsub("\\.network\\.csv$", "", x)
  x <- gsub("^shake[0-9]+-", "", x)
  x <- gsub("[[:space:]]+", "-", x)
    x <- gsub("labors", "labours", x)
  x <- gsub("^the-two-gentlemen-of-verona$", "two-gentlemen-of-verona", x)
  
  x <- trimws(x)
  x
}

cluster_list4 <- lapply(cluster_list4, function(df) {
  df$play_name_match <- normalize_play_name(df$play_name)
  df
})

#fixing full list main data 37 plays
cluster_list4_full <- cluster_list4

methods <- names(cluster_list4)




compute_similarity_matrix <- function(metric_fun) {
  mat <- matrix(NA, nrow = length(methods), ncol = length(methods),
                dimnames = list(methods, methods))
  
  for (i in 1:(length(methods) - 1)) {
    for (j in (i + 1):length(methods)) {
      df1 <- cluster_list4[[methods[i]]]
      df2 <- cluster_list4[[methods[j]]]
      
      merged_df <- inner_join(df1, df2, by = "play_name_match", suffix = c(".x", ".y"))
      
      score <- metric_fun(merged_df$cluster.x, merged_df$cluster.y)
      mat[i, j] <- mat[j, i] <- score
    }
  }
  
  diag(mat) <- 1
  return(round(mat, 3))
}


ari_matrix4 <- compute_similarity_matrix(adjustedRandIndex)

print("=== Adjusted Rand Index (ARI, k=4) ===")
print(ari_matrix4)

View(ari_matrix4)

library(pheatmap)

pheatmap(ari_matrix4,
         display_numbers = TRUE,
         cluster_rows = FALSE,
         cluster_cols = FALSE,
         color = colorRampPalette(c("darkblue", "blue", "lightblue"))(100),
         main = "ARI between clustering methods (k = 4)",
         angle_col = 45)


standardize_name <- function(name) {
  name <- gsub("’", "'", name)
  name <- gsub(" +", " ", name)
  name <- trimws(name)
  return(name)
}

cluster_list4 <- lapply(cluster_list4, function(df) {
  df$play_name <- standardize_name(df$play_name)
  return(df)
})

play_sets <- lapply(cluster_list4, function(df) unique(df$play_name))
common_plays <- Reduce(intersect, play_sets)

cluster_list4 <- lapply(cluster_list4, function(df) {
  df %>% filter(play_name %in% common_plays)
})

play_names <- sort(unique(unlist(lapply(cluster_list4, function(df) df$play_name))))
n <- length(play_names)

# in total 9 clustering methods
threshold <- 8
co_occurrence <- matrix(0, nrow = n, ncol = n, dimnames = list(play_names, play_names))

for (method_df in cluster_list4) {
  method_df <- method_df %>% filter(play_name %in% play_names)
  
  for (clust_id in unique(method_df$cluster)) {
    in_cluster <- method_df$play_name[method_df$cluster == clust_id]
    
    if (length(in_cluster) < 2) next
    
    pairs <- combn(in_cluster, 2, simplify = FALSE)
    
    for (pair in pairs) {
      co_occurrence[pair[1], pair[2]] <- co_occurrence[pair[1], pair[2]] + 1
      co_occurrence[pair[2], pair[1]] <- co_occurrence[pair[2], pair[1]] + 1
    }
  }
}

edge_df <- as.data.frame(as.table(co_occurrence)) %>%
  filter(Freq >= threshold & Var1 != Var2)
edge_df <- edge_df %>%
  rowwise() %>%
  mutate(pair = paste(sort(c(Var1, Var2)), collapse = "_")) %>%
  distinct(pair, .keep_all = TRUE) %>%
  ungroup()
g <- graph_from_data_frame(d = edge_df[, c("Var1", "Var2")], directed = FALSE)
E(g)$weight <- edge_df$Freq
plot(g,
     vertex.label.cex = 0.8,
     edge.width = E(g)$weight,
     main = paste("Graph of co-clustered plays (threshold >=", threshold, ")"))
vcount(g)
sort(unique(V(g)$name))

# COOCCURRENCE ANALYSIS

all_play_names <- sort(unique(unlist(
  lapply(cluster_list4_full, function(df) df$play_name_match)
)))

cluster_matrix <- data.frame(
  play_name_match = all_play_names,
  stringsAsFactors = FALSE
)

for (method_name in names(cluster_list4_full)) {
  
  df <- cluster_list4_full[[method_name]]
  
  cluster_col <- names(df)[grepl("cluster", names(df), ignore.case = TRUE)][1]
  
  df <- df[, c("play_name_match", cluster_col)]
  names(df) <- c("play_name_match", method_name)
  
  cluster_matrix <- left_join(cluster_matrix, df, by = "play_name_match")
}

cat("unique plays =", length(unique(cluster_matrix$play_name_match)), "\n")

View(cluster_matrix)
cluster_df <- cluster_matrix[, -1]
cluster_df[] <- lapply(cluster_df, as.numeric)

methods <- colnames(cluster_df)
n_methods <- length(methods)

ari_matrix <- matrix(0, n_methods, n_methods)
rownames(ari_matrix) <- colnames(ari_matrix) <- methods

for (i in 1:n_methods) {
  for (j in 1:n_methods) {
    ari_matrix[i, j] <- adjustedRandIndex(cluster_df[[i]], cluster_df[[j]])
  }
}

View(ari_matrix)

my_palette <- colorRampPalette(c("white", "yellow", "orange"))(100)

pheatmap(
  ari_matrix,
  main = "Adjusted Rand Index (ARI)",
  display_numbers = TRUE,
  cluster_rows = FALSE,
  cluster_cols = FALSE,
  color = my_palette,
  fontsize_number = 10,
  fontsize = 10,
  angle_col = 90,
  border_color = NA
)


cluster_only <- cluster_matrix[, -which(names(cluster_matrix) == "play_name_match")]

n <- nrow(cluster_matrix)

cooccurrence_matrix <- matrix(0, nrow = n, ncol = n)
rownames(cooccurrence_matrix) <- cluster_matrix$play_name_match
colnames(cooccurrence_matrix) <- cluster_matrix$play_name_match

for (method in names(cluster_only)) {
  clusters <- cluster_only[[method]]
  
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      if (!is.na(clusters[i]) && !is.na(clusters[j]) && clusters[i] == clusters[j]) {
        cooccurrence_matrix[i, j] <- cooccurrence_matrix[i, j] + 1
        cooccurrence_matrix[j, i] <- cooccurrence_matrix[j, i] + 1
      }
    }
  }
}

View(cooccurrence_matrix)


props <- read_excel("/Users/uliapolovinka/Desktop/diplomova praca/plays_prop_dataset (version 2).xlsx")


# ANALYSIS OF GROUPS 

co_graph <- graph_from_adjacency_matrix(
  cooccurrence_matrix,
  mode = "undirected",
  diag = FALSE,
  weighted = TRUE)

V(co_graph)$name  <- rownames(cooccurrence_matrix)
V(co_graph)$label <- V(co_graph)$name

set.seed(42)
lay <- layout_with_fr(co_graph, weights = E(co_graph)$weight)

E(co_graph)$width <- 0.5 + 0.2 * E(co_graph)$weight
V(co_graph)$size  <- 6 + log1p(strength(co_graph))

plot(
  co_graph,
  layout = lay,
  vertex.label = V(co_graph)$label,
  edge.width = E(co_graph)$width,
  vertex.frame.color = NA)

w <- E(co_graph)$weight

if (max(w) == min(w)) {
  a <- rep(1, length(w))
} else {
  a <- (w - min(w)) / (max(w) - min(w))
}

pal <- colorRampPalette(c("#fff7bc", "#fdae6b", "#e6550d", "#a63603"))

bin <- as.integer(cut(w, breaks = 6, include.lowest = TRUE))
edge_cols <- pal(6)[bin]

stopifnot(is.character(edge_cols), is.numeric(a), length(edge_cols) == length(a))

E(co_graph)$color <- mapply(
  function(col, alp) grDevices::adjustcolor(col, alpha.f = alp),
  col = edge_cols,
  alp = 0.3 + 0.7 * a,
  SIMPLIFY = TRUE,
  USE.NAMES = FALSE)

dev.new()
plot(
  co_graph,
  layout = lay,
  vertex.label = V(co_graph)$label,
  edge.width = E(co_graph)$width,
  edge.color = E(co_graph)$color,
  vertex.frame.color = NA)
set.seed(42)

E(co_graph)$weight <- as.numeric(E(co_graph)$weight)
w <- E(co_graph)$weight

lay <- layout_with_fr(co_graph, weights = w)

E(co_graph)$width <- 0.5 + 0.2 * w

st <- igraph::strength(
  co_graph,
  vids    = V(co_graph),
  mode    = "all",
  loops   = TRUE,
  weights = w)

st_num <- unname(as.numeric(st))
st_num[is.na(st_num)] <- 0

lay <- layout_with_fr(co_graph, niter = 2000)

V(co_graph)$size <- 10 + 2 * log1p(st_num)
par(bg = "white")

lay <- layout_with_fr(co_graph, niter = 2000)

par(bg = "white")

dev.new(width = 10, height = 8)
w_norm <- E(co_graph)$weight / max(E(co_graph)$weight)

edge_cols <- rgb(
  red = 213/255,
  green = 94/255,
  blue = 0/255,
  alpha = 0.05 + 0.5 * w_norm
)

plot(
  co_graph,
  layout = lay,
  vertex.label = V(co_graph)$name,
  vertex.label.cex = 0.7,
  vertex.label.color = "navy",
  vertex.label.family = "serif",
  vertex.label.dist = 0.3,
  
  vertex.size = 6,
  vertex.color = "#E69F00",
  vertex.frame.color = NA,
  
  edge.width = 0.2 + 0.6 * w_norm,
  edge.color = edge_cols,
  
  margin = 0.05
)
g8 <- delete_edges(co_graph, E(co_graph)[weight < 8])

g8 <- delete_edges(co_graph, E(co_graph)[weight < 8])

w_norm_g8 <- E(g8)$weight / max(E(g8)$weight)

edge_cols_g8 <- rgb(
  red   = 213/255,
  green = 94/255,
  blue  = 0/255,
  alpha = 0.08 + 0.65 * w_norm_g8
)

dev.new(width = 10, height = 8)

plot(
  g8,
  layout = lay,
  vertex.label = V(g8)$name,
  vertex.label.cex = 0.7,
  vertex.label.color = "navy",
  vertex.label.family = "serif",
  vertex.label.dist = 0.3,
  
  vertex.size = 6,
  vertex.color = "#E69F00",
  vertex.frame.color = NA,
  
  edge.width = 0.3 + 0.9 * w_norm_g8,
  edge.color = edge_cols_g8,
  
  margin = 0.05)



w8 <- as.numeric(E(g8)$weight)

a8 <- if (length(w8) > 0 && diff(range(w8)) > 0) {
  (w8 - min(w8)) / (max(w8) - min(w8))
} else {
  rep(1, length(w8))
}

pal <- colorRampPalette(c("#ffffb2", "#fecc5c", "#fd8d3c", "#f03b20", "#bd0026"))

bin <- if (length(unique(w8)) > 1) {
  as.integer(cut(w8, breaks = 6, include.lowest = TRUE))
} else {
  rep(6, length(w8))
}

edge_cols <- pal(6)[bin]

edge_cols <- mapply(
  function(col, alp) grDevices::adjustcolor(col, alpha.f = 0.4 + 0.6 * alp),
  edge_cols,
  a8,
  USE.NAMES = FALSE)

E(g8)$color <- edge_cols
E(g8)$width <- 0.6 + 0.25 * w8

plot(g8,
     layout = lay,
     vertex.label = V(g8)$name,
     edge.width = E(g8)$width,
     edge.color = E(g8)$color,
     vertex.frame.color = NA)


analyze_threshold <- function(threshold, min_clique_size = 3) {
  
  g <- delete_edges(co_graph, E(co_graph)[E(co_graph)$weight < threshold])
  
  comps <- components(g)
  
  comp_sizes <- sizes(comps)
  keep_ids   <- as.integer(names(comp_sizes[comp_sizes >= 2]))
  
  comp_list  <- lapply(keep_ids, function(cid) {
    V(g)$name[comps$membership == cid]
  })
  
  names(comp_list) <- paste0("C", keep_ids)
  
  clqs <- max_cliques(g, min = min_clique_size)
  clqs <- lapply(clqs, function(vs) V(g)$name[vs])
  
  if (length(clqs) > 0) {
    names(clqs) <- paste0("Q", seq_along(clqs))
  }
  
  comp_stats <- NULL
  
  if (length(comp_list)) {
    comp_stats <- do.call(rbind, lapply(names(comp_list), function(nm){
      
      vs <- comp_list[[nm]]
      sg <- induced_subgraph(g, vids = vs)
      
      data.frame(
        threshold = threshold,
        component = nm,
        size = vcount(sg),
        edges = ecount(sg),
        density = edge_density(sg, loops = FALSE),
        members = paste(vs, collapse = " I "),
        stringsAsFactors = FALSE
      )
    }))
  }
  
  list(
    threshold = threshold,
    graph = g,
    components = comp_list,
    comp_stats = comp_stats,
    cliques = clqs)
}

thresholds <- c(6, 7, 8, 9)

res_list <- lapply(thresholds, analyze_threshold)

comp_summary <- do.call(rbind, lapply(res_list, function(res) res$comp_stats))
comp_summary <- comp_summary[order(comp_summary$threshold, -comp_summary$size), ]

rownames(comp_summary) <- NULL
res_list[[which(thresholds == 8)]]$components

# max cliques
clique_summary <- do.call(rbind, lapply(res_list, function(res) {
  if (!length(res$cliques)) return(NULL)
  
  do.call(rbind, lapply(names(res$cliques), function(nm) {
    mem <- res$cliques[[nm]]
    data.frame(
      threshold = res$threshold,
      clique = nm,
      size = length(mem),
      members = paste(mem, collapse = " | "),
      stringsAsFactors = FALSE)
  }))
}))

if (!is.null(clique_summary)) {
  clique_summary <- clique_summary[order(clique_summary$threshold, -clique_summary$size), ]
  rownames(clique_summary) <- NULL
}

print(head(comp_summary, 20))
print(head(clique_summary, 20))

# visualization for threshold = 8
thr_show <- 8
g_show <- res_list[[which(thresholds == thr_show)]]$graph

plot(
  g_show,
  layout = lay,
  vertex.size = 5,
  vertex.label = V(g_show)$name,
  main = paste("Graph of Play Co-occurrence (threshold ≥", thr_show, ")"))

sums_cooccurrence <- rowSums(cooccurrence_matrix)
print(sort(sums_cooccurrence))


# analysis by cliques at threshold = 8
res_t8 <- res_list[[which(thresholds == 8)]]

props$play_name_match <- normalize_play_name(props$Play)
head(props[, c("Play", "play_name_match")])

cliques_to_check <- list(
  Q3 = res_t8$cliques[["Q3"]],
  Q2 = res_t8$cliques[["Q2"]],
  Q1 = res_t8$cliques[["Q1"]])

for (nm in names(cliques_to_check)) {
  cat("\n=== Clique", nm, "===\n")
  
  clq <- cliques_to_check[[nm]]
  
  if (is.null(clq)) {
    cat("Clique not found.\n")
    next
  }
  
  print(clq)
  
  info <- props %>% filter(play_name_match %in% clq)
  print(info)
  
  cat("Genres:\n")
  print(table(info$Genre))
  
  cat("Periods:\n")
  print(table(info$Period))
  
  cat("Geography:\n")
  print(table(info$Geography))
}


# Isolated plays for each threshold 

get_isolated_plays <- function(graph, threshold) {
  
  g_t <- delete_edges(graph, E(graph)[weight < threshold])
  
  isolated_plays <- V(g_t)$name[degree(g_t) == 0]
  
  return(isolated_plays)
}

get_isolated_plays(co_graph, 6)
get_isolated_plays(co_graph, 7)
get_isolated_plays(co_graph, 8)
get_isolated_plays(co_graph, 9)



# WALKTRAP 

wt4 <- cluster_walktrap(
  co_graph,
  steps = 4,
  weights = E(co_graph)$weight)

set.seed(42)
lay <- layout_with_fr(co_graph)

# 1) Optimal Walktrap partition

mems_wt_opt <- membership(wt4)
mod_wt_opt  <- modularity(wt4)

table_wt_opt <- table(mems_wt_opt)

V(co_graph)$comm <- factor(mems_wt_opt)

graphics.off()

plot(
  co_graph,
  layout = lay,
  vertex.color = V(co_graph)$comm,
  vertex.label = NA,
  vertex.size = 9,
  edge.width = 0.4,
  edge.color = "grey80")

print(table_wt_opt)
print(mod_wt_opt)

plays <- V(co_graph)$name

walktrap_opt_df <- data.frame(
  Play = plays,
  Cluster = unname(mems_wt_opt),
  stringsAsFactors = FALSE)

walktrap_opt_df <- walktrap_opt_df[
  order(walktrap_opt_df$Cluster, walktrap_opt_df$Play),]

saveRDS(walktrap_opt_df, "walktrap_cooc_optimal.rds")



# 2) Walktrap partition cut into 3 communities

mems_k3 <- cut_at(wt4, no = 3)

mod_k3 <- modularity(
  co_graph,
  mems_k3,
  weights = E(co_graph)$weight)

table_k3 <- table(mems_k3)

V(co_graph)$comm <- factor(mems_k3)

plot(
  co_graph,
  layout = lay,
  vertex.color = V(co_graph)$comm,
  vertex.label = NA,
  vertex.size = 9,
  edge.width = 0.4,
  edge.color = "grey80")

print(table_k3)
print(mod_k3)

walktrap_k3_df <- data.frame(
  Play = plays,
  Cluster = unname(mems_k3),
  stringsAsFactors = FALSE)

walktrap_k3_df <- walktrap_k3_df[
  order(walktrap_k3_df$Cluster, walktrap_k3_df$Play),]

saveRDS(walktrap_k3_df, "walktrap_cooc_k3.rds")


# 3) Walktrap partition cut into 4 communities

mems_k4 <- cut_at(wt4, no = 4)

mod_k4 <- modularity(
  co_graph,
  mems_k4,
  weights = E(co_graph)$weight)

table_k4 <- table(mems_k4)

V(co_graph)$comm <- factor(mems_k4)

plot(
  co_graph,
  layout = lay,
  vertex.color = V(co_graph)$comm,
  vertex.label = NA,
  vertex.size = 9,
  edge.width = 0.4,
  edge.color = "grey80")

print(table_k4)
print(mod_k4)

walktrap_k4_df <- data.frame(
  Play = plays,
  Cluster = unname(mems_k4),
  stringsAsFactors = FALSE)

walktrap_k4_df <- walktrap_k4_df[
  order(walktrap_k4_df$Cluster, walktrap_k4_df$Play),]

saveRDS(walktrap_k4_df, "walktrap_cooc_k4.rds")




# CLUSTER OPTIMAl
opt <- cluster_optimal(co_graph, weights = E(co_graph)$weight)

mems_opt <- membership(opt)
mod_opt  <- modularity(opt)

V(co_graph)$comm_opt <- factor(mems_opt)

plot(
  co_graph,
  layout = lay,
  vertex.color = V(co_graph)$comm_opt,
  vertex.label = NA,
  vertex.size = 9,
  edge.width=0.3,
  edge.color = "grey80")

optimal_df <- data.frame(
  Play = V(co_graph)$name,
  Cluster = unname(mems_opt),
  stringsAsFactors = FALSE)

optimal_df <- optimal_df[
  order(optimal_df$Cluster, optimal_df$Play), ]

saveRDS(optimal_df, "optimal_cooc.rds")



# Computing ARI for Cluster_optimal and Walktrap 
library(mclust)
ari_value <- adjustedRandIndex(
  optimal_df$Cluster,
  walktrap_opt_df$Cluster)

print(ari_value)
#0.3510451 for walktrap_3, 0.6662859 for walktrap_4, 0.6141642 for walktrap_optimal


# Calculating similarity for Walktrap_4 and our individual methods with ARI

wt_df <- walktrap_k4_df %>%
  rename(
    play_name_match = Play,
    cluster_wt = Cluster)

uniquemethod <- cluster_list4_full$emd %>%
  select(play_name_match, cluster)
merged_df <- inner_join(wt_df, uniquemethod, by = "play_name_match")

ari_value <- adjustedRandIndex(
  merged_df$cluster_wt,
  merged_df$cluster)


#archetypoid: 0.4218384.           roughness: 0.2511371
#netlsd:-0.03135001                spectral entropy: 0.2819386
#statistics: 0.4439401             rough+entropy: 0.4494451
#dtw: 0.1120898                    emd:0.473226
#archetype: 0.1103817







