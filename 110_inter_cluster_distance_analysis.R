# 110_inter_cluster_distance_analysis.R

# Purpose: Perform inter-cluster distance analysis for K=7 clustering results.
# This script calculates Euclidean distances between cluster medians to help assess
# cluster separation and interpretability.

# --- Ensure Packages are Installed ---
cat("--- Checking and Installing Required Packages ---\n")
required_packages <- c("tidyverse", "pheatmap", "knitr")
# Check which packages are not already installed
packages_to_install <- required_packages[!sapply(required_packages, function(pkg) requireNamespace(pkg, quietly = TRUE))]

if (length(packages_to_install) > 0) {
  cat("The following required packages are missing or not found:", paste(packages_to_install, collapse=", "), "\n")
  cat("Attempting to install them now...\n")
  # Note: Depending on your R environment and permissions, you might need to run R/RStudio as an administrator
  # or configure a personal library path if you encounter installation issues.
  install.packages(packages_to_install, repos = "http://cran.us.r-project.org") # Added a default repo
  cat("Package installation attempt finished.\nRe-checking packages...\n")
  # Re-check after attempting installation
  sapply(required_packages, function(pkg) {
    if (requireNamespace(pkg, quietly = TRUE)) {
      cat(paste("Package", pkg, "is now available.\n"))
    } else {
      cat(paste("Failed to install or load package:", pkg, ". Please install it manually and try again.\n"))
      # stop(paste("Failed to install or load package:", pkg)) # Optional: stop execution if critical package fails
    }
  })
} else {
  cat("All required packages appear to be already installed.\n")
}
cat("--- Package Check Complete ---\n\n")


# Load necessary libraries
cat("Loading tidyverse...\n")
library(tidyverse)
cat("tidyverse loaded successfully.\n")

cat("Loading pheatmap...\n")
library(pheatmap) # For heatmap visualization
cat("pheatmap loaded successfully.\n")

cat("Loading knitr...\n")
library(knitr)    # For pretty printing tables
cat("knitr loaded successfully.\n")

# --- Configuration ---
# Define the relative path to the CSV file
input_csv_path <- "Results/tables/081_kprototype_post_1_removal_k7/kproto_medians_k7_post_1_removed.csv"
# Define the output path for the heatmap
output_heatmap_path <- "Results/plots/110_inter_cluster_distance_heatmap.png"
# Define the output path for the new heatmap without dendrograms
output_heatmap_no_dendrogram_path <- "Results/plots/110_inter_cluster_distance_heatmap_no_dendrogram.png"

cat("\n--- Input/Output Paths ---\n")
cat("Input CSV path:", input_csv_path, "\n")
cat("Output heatmap path (with dendrograms):", output_heatmap_path, "\n")
cat("Output heatmap path (no dendrograms):", output_heatmap_no_dendrogram_path, "\n")

# --- 0. Pre-run check for input file ---
cat("\n--- Checking for Input File ---\n")
if (!file.exists(input_csv_path)) {
  cat("CRITICAL ERROR: Input CSV file not found at the specified path:", input_csv_path, "\n")
  cat("Please ensure the file exists at that location and the path is correct.\n")
  stop(paste("Input CSV file not found:", input_csv_path))
} else {
  cat("Input CSV file found successfully at:", input_csv_path, "\n")
}

# --- 1. Read the Data ---
cat("Reading data from:", input_csv_path, "\n")
cluster_medians_df <- read.csv(input_csv_path)

cat("\nOriginal Data Head:\n")
print(head(cluster_medians_df))
cat("\nOriginal Data Structure:\n")
str(cluster_medians_df)

# --- 2. Prepare the Data for Distance Calculation ---
# The 'Cluster' column contains the cluster labels.
# The 'Project_Success' column has NAs and should be excluded from distance calculation.
# All other columns are features representing medians for each cluster.

cat("\nPreparing data for distance calculation...\n")
medians_for_dist <- cluster_medians_df %>%
  select(-Project_Success) %>% # Remove the 'Project_Success' column (contains NAs)
  column_to_rownames(var = "Cluster") # Set 'Cluster' column as row names

# Ensure all selected columns are numeric.
# This step is important if any columns were read as non-numeric types.
# Forcing conversion:
feature_columns <- colnames(medians_for_dist)
medians_for_dist[feature_columns] <- lapply(medians_for_dist[feature_columns], as.numeric)


cat("\nData prepared for distance calculation (Head):\n")
print(head(medians_for_dist))
cat("\nStructure of data for distance calculation:\n")
str(medians_for_dist)

# --- 3. Calculate Pairwise Euclidean Distances ---
# The dist() function calculates distances between rows of a data matrix.
cat("\nCalculating inter-cluster Euclidean distances...\n")
inter_cluster_distances <- dist(medians_for_dist, method = "euclidean")
cat("Inter-cluster distances calculated successfully.\n")

# Convert to matrix form for better readability
distance_matrix <- as.matrix(inter_cluster_distances)
cat("Distance matrix created successfully.\n")

cat("\n--- Inter-Cluster Distance Matrix (Euclidean) ---\n")
# Using kable for a nicely formatted table in the console
print(kable(distance_matrix, caption = "Pairwise Euclidean Distances Between Cluster Medians", format = "pipe", digits = 2))

# --- 4. Visualization: Heatmap ---
cat("\nGenerating and saving heatmap...\n")

# Ensure the output directory for the plot exists
output_dir <- dirname(output_heatmap_path)
cat("Output directory for heatmap:", output_dir, "\n")
if (!dir.exists(output_dir)) {
  cat("Creating directory:", output_dir, "\n")
  dir.create(output_dir, recursive = TRUE)
  cat("Output directory created.\n")
} else {
  cat("Output directory already exists.\n")
}

# Generate and save a heatmap of the distance matrix
cat("Attempting to generate pheatmap...\n")
pheatmap(distance_matrix,
         clustering_distance_rows = inter_cluster_distances, # Use the distances for clustering rows
         clustering_distance_cols = inter_cluster_distances, # Use the distances for clustering columns
         display_numbers = TRUE,          # Show numeric values on heatmap cells
         number_format = "%.2f",          # Format for the numbers
         main = "Heatmap of Euclidean Distances Between Cluster Medians (K=7)",
         fontsize_number = 10,            # Font size for numbers in cells
         fontsize_row = 10,               # Font size for row labels
         fontsize_col = 10,               # Font size for column labels
         filename = output_heatmap_path   # Path to save the heatmap image
)
cat("pheatmap generated and attempted to save.\n")

cat(paste0("\nHeatmap (with dendrograms) saved to: ", output_heatmap_path, "\n"))

# --- 4b. Visualization: Heatmap WITHOUT Dendrograms ---
cat("\nGenerating and saving heatmap WITHOUT dendrograms...\n")

# Generate and save a heatmap of the distance matrix without dendrograms
cat("Attempting to generate pheatmap without dendrograms...\n")
pheatmap(distance_matrix,
         cluster_rows = FALSE,            # Do not cluster rows / remove row dendrogram
         cluster_cols = FALSE,            # Do not cluster columns / remove col dendrogram
         display_numbers = TRUE,          # Show numeric values on heatmap cells
         number_format = "%.2f",          # Format for the numbers
         main = "Heatmap of Euclidean Distances (No Dendrograms, K=7)",
         fontsize_number = 10,
         fontsize_row = 10,
         fontsize_col = 10,
         filename = output_heatmap_no_dendrogram_path # Path to save the new heatmap image
)
cat("pheatmap without dendrograms generated and attempted to save.\n")

cat(paste0("\nHeatmap (without dendrograms) saved to: ", output_heatmap_no_dendrogram_path, "\n"))

# --- 5. Interpretation Guidance ---
cat("\n--- How to Interpret the Results ---\n")
cat("The goal is to determine if clusters are well-separated and interpretable.\n\n")

cat("1. Examine the Distance Matrix and Heatmap:\n")
cat("   - Small values (and lighter colors in the heatmap, depending on the color scheme) indicate that two clusters are close (similar) in terms of their median feature values.\n")
cat("   - Large values (and darker colors) indicate clusters are far apart (dissimilar).\n")
cat("   - Look for pairs of clusters with very small distances. These might be difficult to distinguish from each other and could potentially be merged or indicate an issue with the chosen K.\n")
cat("   - Ideally, you want to see relatively large distances between different clusters, suggesting good separation.\n\n")

cat("2. Consider Cluster Interpretability:\n")
cat("   - To interpret each cluster, refer back to the 'cluster_medians_df' (or the original input CSV).\n")
cat("   - For each cluster, analyze its median values for all features (e.g., ORG_Employees, STY_DataDriven, etc.).\n")
cat("   - Create a profile for each cluster: What makes it unique? Is it high on certain features and low on others?\n")
cat("   - Example: 'Cluster 1 is characterized by high ORG_Employees and low STY_Authoritative_Threats.'\n\n")

cat("3. Assessing Difficulty in Distinguishing Clusters:\n")
cat("   - If many pairs of clusters have small inter-cluster distances, they are likely hard to distinguish.\n")
cat("   - If the feature profiles (median values) of different clusters are very similar, it becomes challenging to assign a unique, meaningful interpretation to each one.\n")
cat("   - The heatmap visually highlights these similarities. Blocks of similar color close to the diagonal (if clusters are ordered by similarity) or off-diagonal pairings with similar colors suggest closeness.\n\n")

cat("4. Next Steps:\n")
cat("   - If clusters are hard to distinguish, you might consider:\n")
cat("     a) Exploring a different number of clusters (K).\n")
cat("     b) Reviewing the features used for clustering; perhaps some are not contributing to meaningful separation.\n")
cat("     c) Using other cluster validation metrics (e.g., silhouette analysis on the original data) to assess cluster quality.\n")
cat("   - Effective interpretation means you can clearly articulate the unique characteristics of each cluster and how they differ from others.\n")

cat("\n--- Script Execution Complete ---\n") 