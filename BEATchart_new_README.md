# BEATchart_new 

## Overview

The script performs the following steps:

1. Data preparation for beat charts (ordering scenes and formatting data) based on file `speech_distr2.csv`.

2. Computation of segment change rate and construction of beat charts for every play

3. Calculation of mean and standard deviation of change rates  
   (classification into low, normal and high dynamic types — extra analysis not discussed in the thesis)  
   + k-means clustering

4. Application of DTW to compare beat chart sequences  
   + hierarchical clustering based on DTW distances

## Input

- `speech_distr2.csv` – scene-level character data

## Output

- `speech_data.rds`
- `drama_summary_kmeans_k4.rds`
- `dtw_clusters.rds`
