# 111_inter_cluster_distance_analysis_k4.R

# Purpose: Perform inter-cluster distance analysis for K=4 clustering results.
# This script calculates Euclidean distances between cluster medians to help assess
# cluster separation and interpretability for K=4.

# --- Ensure Packages are Installed ---
cat("--- Checking and Installing Required Packages ---\\n")
required_packages <- c("tidyverse", "pheatmap", "knitr")
# Check which packages are not already installed
packages_to_install <- required_packages[!sapply(required_packages, function(pkg) requireNamespace(pkg, quietly = TRUE))]

if (length(packages_to_install) > 0) {
  cat("The following required packages are missing or not found:", paste(packages_to_install, collapse=", "), "\\n")
  cat("Attempting to install them now...\\n")
  install.packages(packages_to_install, repos = "http://cran.us.r-project.org")
  cat("Package installation attempt finished.\\nRe-checking packages...\\n")
  sapply(required_packages, function(pkg) {
    if (requireNamespace(pkg, quietly = TRUE)) {
      cat(paste("Package", pkg, "is now available.\\n"))
    } else {
      cat(paste("Failed to install or load package:", pkg, ". Please install it manually and try again.\\n"))
    }
  })
} else {
  cat("All required packages appear to be already installed.\\n")
}
cat("--- Package Check Complete ---\\n\\n")

# Load necessary libraries
cat("Loading tidyverse...\\n")
library(tidyverse)
cat("tidyverse loaded successfully.\\n")

cat("Loading pheatmap...\\n")
library(pheatmap) # For heatmap visualization
cat("pheatmap loaded successfully.\\n")

cat("Loading knitr...\\n")
library(knitr)    # For pretty printing tables
cat("knitr loaded successfully.\\n")

# --- Configuration ---
# Define the relative path to the K=4 CSV file
input_csv_path <- "Results/tables/090_kprototype_post_1_removal_k4/kproto_medians_k4_post_1_removed.csv"
# Define the output path for the heatmap with dendrograms (K=4)
output_heatmap_path <- "Results/plots/111_inter_cluster_distance_heatmap_k4.png"
# Define the output path for the new heatmap without dendrograms (K=4)
output_heatmap_no_dendrogram_path <- "Results/plots/111_inter_cluster_distance_heatmap_no_dendrogram_k4.png"

cat("\\n--- Input/Output Paths ---\\n")
cat("Input CSV path:", input_csv_path, "\\n")
cat("Output heatmap path (with dendrograms):", output_heatmap_path, "\\n")
cat("Output heatmap path (no dendrograms):", output_heatmap_no_dendrogram_path, "\\n")

# --- 0. Pre-run check for input file ---
cat("\\n--- Checking for Input File ---\\n")
if (!file.exists(input_csv_path)) {
  cat("CRITICAL ERROR: Input CSV file not found at the specified path:", input_csv_path, "\\n")
  cat("Please ensure the file exists at that location and the path is correct.\\n")
  stop(paste("Input CSV file not found:", input_csv_path))
} else {
  cat("Input CSV file found successfully at:", input_csv_path, "\\n")
}

# --- 1. Read the Data ---
cat("Reading data from:", input_csv_path, "\\n")
cluster_medians_df <- read.csv(input_csv_path)

cat("\\nOriginal Data Head (K=4):\\n")
print(head(cluster_medians_df))
cat("\\nOriginal Data Structure (K=4):\\n")
str(cluster_medians_df)

# --- 2. Prepare the Data for Distance Calculation ---
cat("\\nPreparing data for distance calculation...\\n")
medians_for_dist <- cluster_medians_df %>% 
  column_to_rownames(var = "Cluster") 

feature_columns <- colnames(medians_for_dist)
medians_for_dist[feature_columns] <- lapply(medians_for_dist[feature_columns], as.numeric)

cat("\\nData prepared for distance calculation (Head):\\n")
print(head(medians_for_dist))
cat("\\nStructure of data for distance calculation:\\n")
str(medians_for_dist)

# --- 3. Calculate Pairwise Euclidean Distances ---
cat("\\nCalculating inter-cluster Euclidean distances...\\n")
inter_cluster_distances <- dist(medians_for_dist, method = "euclidean")
cat("Inter-cluster distances calculated successfully.\\n")

distance_matrix <- as.matrix(inter_cluster_distances)
cat("Distance matrix created successfully.\\n")

cat("\\n--- Inter-Cluster Distance Matrix (Euclidean, K=4) ---\\n")
print(kable(distance_matrix, caption = "Pairwise Euclidean Distances Between Cluster Medians (K=4)", format = "pipe", digits = 2))

# --- 4a. Visualization: Heatmap WITH Dendrograms (K=4) ---
cat("\\nGenerating and saving heatmap WITH dendrograms (K=4)...\\n")

output_dir_with_dendro <- dirname(output_heatmap_path)
if (!dir.exists(output_dir_with_dendro)) {
  cat("Creating directory:", output_dir_with_dendro, "\\n")
  dir.create(output_dir_with_dendro, recursive = TRUE)
  cat("Output directory created.\\n")
} else {
  cat("Output directory (with dendrograms) already exists or path is root.\\n")
}

cat("Attempting to generate pheatmap (with dendrograms)...\\n")
pheatmap(distance_matrix,
         clustering_distance_rows = inter_cluster_distances, 
         clustering_distance_cols = inter_cluster_distances, 
         display_numbers = TRUE,          
         number_format = "%.2f",          
         main = "Heatmap of Euclidean Distances Between Cluster Medians (K=4)",
         fontsize_number = 10,            
         fontsize_row = 10,               
         fontsize_col = 10,               
         filename = output_heatmap_path   
)
cat("pheatmap (with dendrograms) generated and attempted to save.\\n")
cat(paste0("\\nHeatmap (with dendrograms) saved to: ", output_heatmap_path, "\\n"))

# --- 4b. Visualization: Heatmap WITHOUT Dendrograms (K=4) ---
cat("\\nGenerating and saving heatmap WITHOUT dendrograms (K=4)...\\n")

output_dir_no_dendro <- dirname(output_heatmap_no_dendrogram_path)
if (!dir.exists(output_dir_no_dendro)) {
  cat("Creating directory:", output_dir_no_dendro, "\\n")
  dir.create(output_dir_no_dendro, recursive = TRUE)
  cat("Output directory created.\\n")
} else {
  cat("Output directory (no dendrograms) already exists or path is root.\\n")
}

cat("Attempting to generate pheatmap without dendrograms...\\n")
pheatmap(distance_matrix,
         cluster_rows = FALSE,            
         cluster_cols = FALSE,            
         display_numbers = TRUE,          
         number_format = "%.2f",          
         main = "Heatmap of Euclidean Distances (No Dendrograms, K=4)",
         fontsize_number = 10,
         fontsize_row = 10,
         fontsize_col = 10,
         filename = output_heatmap_no_dendrogram_path 
)
cat("pheatmap without dendrograms generated and attempted to save.\\n")
cat(paste0("\\nHeatmap (without dendrograms) saved to: ", output_heatmap_no_dendrogram_path, "\\n"))

# --- 5. Interpretation Guidance (K=4) ---
cat("\\n--- How to Interpret the K=4 Results ---\\n")
cat("The goal is to determine if the K=4 clusters are well-separated and interpretable.\\n\\n")
cat("1. Examine the Distance Matrix and Heatmaps:\\n")
cat("   - Small values indicate two clusters are close (similar).
")
cat("   - Large values indicate clusters are far apart (dissimilar).
")
cat("   - Look for pairs of clusters with very small distances. These might be difficult to distinguish.
")
cat("   - Ideally, for K=4, you want to see reasonably large distances between all pairs of the 4 clusters.\\n\\n")
cat("2. Consider Cluster Interpretability:\\n")
cat("   - Refer back to the original K=4 medians CSV file.
")
cat("   - Analyze the median values for all features for each of the 4 clusters.
")
cat("   - Create a profile for each cluster. What makes it unique compared to the other three?\\n\\n")
cat("3. Assessing Difficulty in Distinguishing Clusters:\\n")
cat("   - If any pairs of clusters have small inter-cluster distances, they are likely hard to distinguish.
")
cat("   - The heatmaps visually highlight these similarities/dissimilarities.\\n\\n")
cat("4. Comparing K=4 to K=7:\\n")
cat("   - Compare the distinctiveness of clusters in this K=4 analysis to what you saw in the K=7 analysis.
")
cat("   - Does K=4 provide a clearer, more interpretable set of distinct groups?
")
cat("   - Often, a smaller K can resolve issues where a larger K produced clusters that were too similar.\\n")
cat("\\n--- Script Execution Complete (111_inter_cluster_distance_analysis_k4.R) ---\\n") 