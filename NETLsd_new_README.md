# NetLSD Clustering of Shakespeare's Plays

This script computes NetLSD signatures for character networks of Shakespeare's plays and performs clustering based on these signatures, described in Chapter 2.2 Network Laplacian 
Spectral Descriptor.

## Overview

The code performs the following steps:

1. Load character interaction networks from CSV files
2. Convert them into graph objects using `igraph`
3. Compute heat trace for each network
4. Measure similarity between plays using Euclidean distance
5. Apply hierarchical clustering with different methods
6. Group the plays:
   - all together
   - and separately for networks with 1 or 2 connected components (these groupings were created as extra analysis and are not part of the thesis)

## Input

- Folder with `.csv` files containing networks (`Source`, `Target`, `Weight`)
- Each file represents one play

## Output

- NetLSD signature matrix
- Distance matrix between plays with zero diagonal
- Dendrogram plots for clustering
- Saved clustering results:
  - `clustering_all_methods.rds`
