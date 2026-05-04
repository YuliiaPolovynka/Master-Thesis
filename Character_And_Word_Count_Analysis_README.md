# Character_And_Word_Count_Analysis.R - script description

This script computes basic text statistics for Shakespeare's plays and compares them with clustering results. This whole code is additional analysis of plays which is not used and described in the thesis.

## Overview

The code includes the following steps:

- Loads spoken text files for all plays

- Computes the number of characters and words for each play

- Merges text statistics with NetLSD clustering results

- Compares character and word counts across clusters

- Performs ANOVA tests for selected clustering results

- Computes additional graph statistics such as number of nodes, number of edges and density

- Applies k-means clustering based on characters and words

## Input

- Spoken text files (`.txt`) from folder `spoken words`
- NetLSD clustering results (`clustering_all_methods.rds`)
- Character networks

## Output

- `text_analysis_clustering.rds`
- `text_kmeans_k4.rds`
