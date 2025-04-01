# Building Owners' Decision-Making Profile Analysis

This repository contains the analysis of building owners' decision-making profiles using various clustering techniques and dimension reduction methods. The analysis aims to identify distinct groups of building owners based on their organizational characteristics and decision-making patterns.

## Analysis Overview

The analysis consists of four main sections:

1. **Data Preparation and Initial Clustering Analysis**
   - Hierarchical clustering using Ward's method
   - Dendrogram visualization with PDM type coloring
   - Cluster assignment analysis
   - Contingency table analysis of PDM types by cluster

2. **K-Prototypes Clustering Analysis**
   - Mixed data type handling (numerical and ordinal variables)
   - Optimal cluster number determination using silhouette analysis
   - PAM clustering implementation
   - Cluster visualization through boxplots and heatmaps

3. **Detailed Cluster Analysis and Visualization**
   - K-prototypes clustering with k=2 and k=3
   - Numerical variable distribution analysis
   - Cluster characteristics heatmap
   - Silhouette plot analysis

4. **Dimension Reduction Analysis**
   - Principal Component Analysis (PCA)
   - UMAP visualization
   - Scree plot analysis
   - Variable loadings analysis

## Data Variables

### Numerical Variables
- Organization Structure:
  - Number of Employees
  - Number of Locations
  - Number of Departments
  - Number of Layers

### Ordinal Variables
- Distribution Characteristics:
  - Centralization
  - Formalization
- Style Characteristics:
  - Technocracy
  - Participation
  - Organicity
  - Coercion
- Culture Characteristics:
  - Command
  - Symbolic
  - Rationale
  - Generative
  - Transactive
- Flexibility Characteristics:
  - Openness
  - Recursiveness
- Risk
- Environment Characteristics:
  - Growth
  - Hostile
  - Stable

## Output Files

The analysis generates several output files:

1. **Visualization Files**
   - `BOAT2_Dendrogram_PDM.pdf`: Hierarchical clustering dendrogram colored by PDM type
   - `BOAT2_Dendrogram_Clusters.pdf`: Hierarchical clustering dendrogram with cluster coloring
   - `BOAT2_Clustering_Visualizations.pdf`: Boxplots and heatmaps of cluster characteristics
   - `Cluster_Analysis_Visualizations.pdf`: Detailed cluster analysis visualizations
   - `Dimension_Reduction_Analysis.pdf`: PCA and UMAP visualizations

2. **Data Files**
   - `BOAT2_Cluster_Assignments.csv`: Cluster assignments for each observation
   - `BOAT2_PDM_Cluster_Table.csv`: Contingency table of PDM types by cluster
   - `BOAT2_Clustering_Results.csv`: Detailed clustering results with original values

## Dependencies

The analysis requires the following R packages:
- Core data manipulation: tidyverse, dplyr
- Visualization: ggplot2, gridExtra, scales
- Clustering: cluster, clustMixType, factoextra, NbClust
- Dimension reduction: umap
- Visualization enhancements: dendextend, viridis, RColorBrewer, ggrepel

## Usage

To run the analysis:
1. Ensure all required R packages are installed
2. Place the input data file (`BOAT2_Data.csv`) in the working directory
3. Run the R Markdown file (`0320.Rmd`)
4. Check the generated output files for results and visualizations

## Notes

- The analysis includes outlier handling (removing observations with 3700 employees)
- Missing values are handled using median imputation
- Variables are scaled before clustering analysis
- Random seeds are set for reproducibility 