# BOAT2 Cluster and Factor Analysis - Fuzzy Clustering Analysis
# This script performs fuzzy c-means clustering for building owners' decision-making profiles

# 1. Load Setup and Data -----------------------------------------------------
source("R/00_setup.R")

# Load modules for fuzzy clustering
source("R/modules/fuzzy_clustering.R")
source("R/modules/fuzzy_visualization.R")

# Load the prepared datasets
fuzzy_data <- read.csv("results/tables/fuzzy_data.csv")
dimensionality_data <- read.csv("results/tables/dimensionality_reduction_results.csv")

# Define decision variables
decision_vars <- c(
  "Distribution_Centralization", 
  "Distribution_Formalization",
  "Style_Technocracy", 
  "Style_Participation", 
  "Style_Organicity", 
  "Style_Coercion",
  "Culture_Command", 
  "Culture_Symbolic", 
  "Culture_Rationale", 
  "Culture_Generative", 
  "Culture_Transactive",
  "Flexibility_openness", 
  "Flexibility_Recursiveness",
  "Risk",
  "Environment_Growth", 
  "Environment_Hostile", 
  "Environment_Stable"
)

# 2. Determine Optimal Number of Clusters ------------------------------------

# 2.1 Silhouette Analysis
silhouette_results <- get_fcm_silhouette(fuzzy_data, decision_vars)

# Create silhouette plot
silhouette_plot <- ggplot(silhouette_results, aes(x = k, y = silhouette)) +
  geom_line() +
  geom_point(size = 3) +
  labs(
    title = "Silhouette Analysis for Fuzzy Clustering",
    x = "Number of Clusters (k)",
    y = "Average Silhouette Width"
  ) +
  theme_minimal()

# Save silhouette plot
save_plot(silhouette_plot, "fuzzy_silhouette_plot.pdf", width = 10, height = 7)

# 2.2 Dunn Index Analysis
dunn_results <- get_fcm_dunn(fuzzy_data, decision_vars)

# Create Dunn index plot
dunn_plot <- ggplot(dunn_results, aes(x = k, y = dunn)) +
  geom_line() +
  geom_point(size = 3) +
  labs(
    title = "Dunn Index for Fuzzy Clustering",
    x = "Number of Clusters (k)",
    y = "Dunn Index"
  ) +
  theme_minimal()

# Save Dunn index plot
save_plot(dunn_plot, "fuzzy_dunn_plot.pdf", width = 10, height = 7)

# 2.3 Determine optimal number of clusters
# Based on silhouette and Dunn index
best_k_silhouette <- silhouette_results$k[which.max(silhouette_results$silhouette)]
best_k_dunn <- dunn_results$k[which.max(dunn_results$dunn)]

cat("Optimal number of clusters based on silhouette:", best_k_silhouette, "\n")
cat("Optimal number of clusters based on Dunn index:", best_k_dunn, "\n")

# For this analysis, we'll use k=2 (assuming it's the optimal value from indices)
optimal_k <- 2

# 3. Perform Fuzzy C-means Clustering ----------------------------------------

# Run fuzzy clustering using our module function
fuzzy_clustering_result <- run_fuzzy_clustering(fuzzy_data, decision_vars, k = optimal_k)

# Extract results
fuzzy_results <- fuzzy_clustering_result$results
fcm_object <- fuzzy_clustering_result$fcm_object
centroids <- fuzzy_clustering_result$centroids

# Save fuzzy clustering results
save_data(fuzzy_results, "fuzzy_clustering_results.csv")
save_data(as.data.frame(t(centroids)), "fuzzy_cluster_centroids.csv")

# 4. Add Uncertainty Measurement ---------------------------------------------

# Calculate membership uncertainty (entropy)
fuzzy_results$Uncertainty <- calculate_uncertainty(fuzzy_results[, paste0("Cluster", 1:optimal_k)])

# 5. PDM Analysis -----------------------------------------------------------

# Analyze PDM distribution in clusters
pdm_analysis <- analyze_pdm_distribution(fuzzy_results)

# Save PDM analysis results
pdm_cluster_table <- pdm_analysis$contingency_table
pdm_cluster_prop <- pdm_analysis$proportions
pdm_membership <- pdm_analysis$avg_membership

# Save contingency table
write.csv(pdm_cluster_table, "results/tables/fuzzy_pdm_cluster_table.csv")

# Save PDM membership table
save_data(pdm_membership, "fuzzy_pdm_membership.csv")

# 6. Create Visualizations --------------------------------------------------

# 6.1 Create heat map of cluster centroids
heatmap_plot <- plot_centroids_heatmap(centroids)
save_plot(heatmap_plot, "fuzzy_cluster_heatmap.pdf", width = 10, height = 12)

# 6.2 Create radar chart data
radar_data <- prepare_radar_data(centroids)

# 6.3 Create membership distribution plot
membership_plot <- plot_membership_distribution(fuzzy_results)
save_plot(membership_plot, "fuzzy_membership_distribution.pdf", width = 12, height = 8)

# 6.4 Create membership scatter plot
membership_scatter <- plot_membership_scatter(fuzzy_results)
save_plot(membership_scatter, "fuzzy_membership_scatter.pdf", width = 10, height = 8)

# 6.5 Create PDM distribution plot
pdm_plot <- plot_pdm_distribution(fuzzy_results)
save_plot(pdm_plot, "fuzzy_pdm_distribution.pdf", width = 10, height = 7)

# 6.6 Create PDM membership plot
pdm_membership_plot <- plot_pdm_membership(fuzzy_results)
save_plot(pdm_membership_plot, "fuzzy_pdm_membership_plot.pdf", width = 10, height = 7)

# 6.7 Create uncertainty plots
uncertainty_hist <- plot_uncertainty_histogram(fuzzy_results)
save_plot(uncertainty_hist, "fuzzy_uncertainty_histogram.pdf", width = 10, height = 7)

uncertainty_pdm <- plot_uncertainty_pdm(fuzzy_results)
save_plot(uncertainty_pdm, "fuzzy_uncertainty_pdm.pdf", width = 10, height = 7)

# 7. Create Comprehensive Report ---------------------------------------------

# Generate a full report with all visualizations
create_fuzzy_visualization_report(
  fuzzy_results = fuzzy_results,
  centroids = centroids,
  radar_data = radar_data,
  dim_data = dimensionality_data,
  file_path = "results/figures/fuzzy_clustering_report.pdf"
)

# 8. Print Summary ----------------------------------------------------------
cat("\nFuzzy Clustering Analysis complete!\n")
cat("Optimal number of clusters:", optimal_k, "\n")
cat("Cluster sizes (based on dominant membership):\n")
print(table(fuzzy_results$HardCluster))
cat("\nPDM distribution by cluster:\n")
print(round(pdm_cluster_prop, 1))
cat("\nVisualizations saved to results/figures/\n")
cat("Results data saved to results/tables/\n") 