# BOAT2 Cluster and Factor Analysis

## Project Overview

This project analyzes building owners' decision-making characteristics, identifies owner decision-making patterns through cluster analysis, and explores the relationship between these patterns and project delivery methods (PDM). The study employs multiple clustering methods and dimensionality reduction techniques for visualization to identify groups of owners with similar decision-making characteristics.

## Research Questions

1. Are there distinct grouping patterns in building owners' decision-making characteristics?
2. How do different clustering methods perform in distinguishing owner decision-making characteristics?
3. Is there an association between owner decision-making characteristics and project delivery methods?
4. Which decision-making characteristics are most important in distinguishing different owner groups?

## Project Structure

This project uses a modular approach with separate R scripts for different analyses:

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
│   ├── kprototype_analysis.R  # K-prototype clustering for mixed data types
│   ├── run_all.R              # Main script to run the complete analysis
│   └── modules/               # Modular components
│       ├── clustering/        # Specialized clustering functions
│       ├── fuzzy_clustering.R # Fuzzy clustering implementation
│       ├── fuzzy_visualization.R # Fuzzy clustering visualization
│       ├── validation.R       # Cluster validation methods
│       └── visualization.R    # Visualization functions
├── Results/                   # Analysis outputs
│   ├── tables/                # CSV output files
│   └── figures/               # Generated visualizations
├── BOAT2-Cluster-and-Factor.Rmd  # R Markdown document for analysis
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
3. Run the scripts in numerical order, or use the provided `run_all.R` script to run the complete analysis
4. Alternatively, you can use the R Markdown file `BOAT2-Cluster-and-Factor.Rmd` for an interactive analysis

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

The analysis generates several output files in the Results directory:

1. **Visualization Files** (in Results/figures/)
   - Dendrograms of hierarchical clustering
   - Boxplots and heatmaps of cluster characteristics
   - Dimensionality reduction visualizations (PCA, UMAP)
   - Radar charts of cluster profiles

2. **Data Files** (in Results/tables/)
   - Cluster assignments for each observation
   - Contingency tables of PDM types by cluster
   - Detailed clustering results with original values

## Dependencies

The analysis requires the following R packages:
- Core data manipulation: tidyverse, dplyr
- Visualization: ggplot2, gridExtra, scales
- Clustering: cluster, clustMixType, factoextra, NbClust
- Dimension reduction: umap
- Visualization enhancements: dendextend, viridis, RColorBrewer, ggrepel

## Usage

To run the complete analysis:
1. Ensure all required R packages are installed
2. Run the `R/run_all.R` script which will execute all analysis steps in sequence
3. Check the generated output files in the Results directory

Alternatively, for an interactive analysis:
1. Open the R Markdown file `BOAT2-Cluster-and-Factor.Rmd`
2. Execute the chunks in sequence

## Notes

- The analysis includes outlier handling (removing observations with 3700 employees)
- Missing values are handled using median imputation
- Variables are scaled before clustering analysis
- Random seeds are set for reproducibility 