# BEATchart2_new - script description

This script continues the beat chart analysis based on the previosuly prepared file `speech_data.rds`.

## Overview

The script performs the following steps:

- Loads preprocessed beat chart data and constructs beat charts

- Rescales beat chart sequences to a fixed length (100 points) for further analysis

- Computes roughness and spectral entropy on rescaled beat charts and applies k-means clustering both separately and together in 2D  
  (Chapter 3.3 Roughness and Spectral Entropy)

- Computes EMD distances between (classic) beat chart distributions and applies hierarchical clustering  
  (Chapter 3.1 Earth Mover's Distance)

- Applies archetypal analysis and archetypoid analysis to rescaled beat charts  
  (Chapter 3.4 Archetypal Analysis)  
  (additional archetype analysis on roughness + entropy is part of extra analysis and not included in the thesis)

## Input

- `speech_data.rds` – previous data used for beat chart construction

## Output

- `roughness_clusters_4.rds`
- `data_spectral_entropy_clusters_4.rds`
- `data_roughness_entropy_clusters_4.rds`
- `emd_wardD2_clusters.rds`
- `archetype_clusters.rds`
- `beat_chart_archetypoids_clusters.rds`

