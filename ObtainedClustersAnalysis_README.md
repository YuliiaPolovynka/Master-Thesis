# ObtainedClustersAnalysis.R - script description

This script compares clustering results obtained from different methods used in the thesis and containes code for Chapter 4 Clustering and Comparison: Investigation of Groups
of Similar Plays.

## Overview

- Loads clustering results from NetLSD, mean +SD, DTW, EMD, roughness, spectral entropy, archetypal analysis and archetypoid analysis

- Standardizes play names across different datasets

- Computes similarity between clustering methods using Adjusted Rand Index (ARI)

- Creates ARI heatmaps for comparing clustering methods

- Builds a co-occurrence matrix showing how often pairs of plays appear in the same cluster

- Constructs a co-occurrence graph of plays from this matrix

- Analyzes stable groups of plays using higher thresholds, connected components and cliques

- Compares detected groups with play metadata such as genre, period and geography

- Applies community detection methods to the co-occurrence graph:
  - Walktrap (2 communities optimal and 4 communities for further comparison)
  - optimal clustering (2 communities)
  - ARI between Waktrap with 4 communities and individual methods and Walktrap with optimal modularity 

## Input

- clustering results saved as `.rds` files
- play metadata file 
- results from previous analysis scripts

## Output

- ARI matrices and heatmaps
- co-occurrence matrix
- co-occurrence graph
- community detection results:
  - `walktrap_cooc_optimal.rds`
  - `walktrap_cooc_k4.rds`
  - `optimal_cooc.rds`
