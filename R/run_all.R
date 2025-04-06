# BOAT2 Cluster and Factor Analysis - Main Script
# This script runs all analysis steps in sequence

# Record start time
start_time <- Sys.time()
cat("Starting BOAT2 Cluster and Factor Analysis...\n")
cat("Start time:", format(start_time), "\n\n")

# 1. Setup ----------------------------------------------------------------
cat("1. Loading required packages and initializing environment...\n")
source("R/00_setup.R")

# 2. Data Preparation -----------------------------------------------------
cat("\n2. Preparing and cleaning data...\n")
source("R/01_data_preparation.R")

# 3. Exploratory Data Analysis --------------------------------------------
cat("\n3. Performing exploratory data analysis...\n")
source("R/02_exploration.R")

# 4. Dimensionality Reduction ---------------------------------------------
cat("\n4. Performing dimensionality reduction (PCA & UMAP)...\n")
source("R/03_dimensionality.R")

# 5. Clustering Analysis --------------------------------------------------
cat("\n5. Performing hierarchical and K-prototypes clustering...\n")
source("R/04_clustering.R")

# 6. Fuzzy Clustering Analysis --------------------------------------------
cat("\n6. Performing fuzzy c-means clustering...\n")
source("R/05_fuzzy_clustering.R")

# Record end time and calculate duration
end_time <- Sys.time()
duration <- end_time - start_time

# Print completion message
cat("\n========================================================\n")
cat("BOAT2 Cluster and Factor Analysis Complete!\n")
cat("Analysis started at:", format(start_time), "\n")
cat("Analysis completed at:", format(end_time), "\n")
cat("Total duration:", format(duration), "\n")
cat("========================================================\n\n")

cat("All results have been saved to the 'results' directory:\n")
cat("- Tables: results/tables/\n")
cat("- Figures: results/figures/\n\n")

cat("Main output files include:\n")
cat("- Hierarchical clustering results: hierarchical_clusters.csv\n")
cat("- K-prototypes clustering results: kproto_clusters_k2.csv, kproto_clusters_k3.csv\n")
cat("- Fuzzy clustering results: fuzzy_clustering_results.csv\n")
cat("- PCA/UMAP results: dimensionality_reduction_results.csv\n\n")

cat("To view the PDF reports, check:\n")
cat("- Exploratory analysis: results/figures/key_variable_pairs.pdf\n")
cat("- Dimensionality reduction: results/figures/dimensionality_reduction_analysis.pdf\n")
cat("- Clustering analysis: results/figures/clustering_analysis_summary.pdf\n")
cat("- Fuzzy clustering: results/figures/fuzzy_clustering_report.pdf\n") 