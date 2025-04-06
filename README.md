# BOAT2 Cluster and Factor Analysis

## Project Overview

This project analyzes building owners' decision-making characteristics, identifies owner decision-making patterns through cluster analysis, and explores the relationship between these patterns and project delivery methods (PDM). The study employs multiple clustering methods and dimensionality reduction techniques for visualization to identify groups of owners with similar decision-making characteristics.

## Research Questions

1. Are there distinct grouping patterns in building owners' decision-making characteristics?
2. How do different clustering methods perform in distinguishing owner decision-making characteristics?
3. Is there an association between owner decision-making characteristics and project delivery methods?
4. Which decision-making characteristics are most important in distinguishing different owner groups?

## Project Structure

This project has been restructured to use a modular approach with separate R scripts for different analyses:

```
BOAT2-Cluster-and-Factor/
├── data/                      # Data files
│   └── BOAT2_Data.csv         # Main dataset
├── R/                         # R scripts
│   ├── 00_setup.R             # Library loading and common utilities
│   ├── 01_data_preparation.R  # Data preprocessing and cleaning
│   ├── 02_exploration.R       # Exploratory data analysis
│   ├── 03_dimensionality.R    # PCA and UMAP dimensionality reduction
│   ├── 04_clustering.R        # Main clustering analysis
│   ├── 05_fuzzy_clustering.R  # Fuzzy clustering analysis
│   ├── 06_pdm_analysis.R      # Project delivery method relationship analysis
│   └── 07_visualizations.R    # Common visualization functions
├── results/                   # Analysis outputs
│   ├── tables/                # CSV output files
│   └── figures/               # Generated visualizations
└── README.md                  # Project description
```

## Analysis Methods

This study employs multiple clustering methods:
- Hierarchical Clustering
- Partitioning Around Medoids (PAM)
- K-Prototypes Clustering (for handling mixed data types)
- Fuzzy C-means Clustering (for soft clustering)

The optimal number of clusters is determined through:
- Silhouette Analysis
- Elbow Method
- Gap Statistic
- Cluster Stability Analysis

## How to Use

1. Clone this repository
2. Ensure R and all required packages are installed (run `R/00_setup.R` to check and install needed packages)
3. Run the scripts in numerical order, or use the provided main script to run the complete analysis

## Main Outputs

- Cluster membership assignments for building owners
- Visualizations of cluster characteristics
- Analysis of relationship between clusters and PDM choices
- Dimensionality reduction plots for data exploration

## Required Packages

- tidyverse, dplyr, readr (data processing)
- ggplot2, gridExtra, viridis, RColorBrewer (visualization)
- cluster, clustMixType, NbClust (clustering)
- e1071 (fuzzy clustering)
- factoextra, umap (dimensionality reduction)
- fmsb (radar charts)

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