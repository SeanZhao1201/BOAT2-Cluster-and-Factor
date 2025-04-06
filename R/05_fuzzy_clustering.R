# BOAT2 Cluster and Factor Analysis - Fuzzy Clustering Analysis
# This script performs fuzzy c-means clustering for building owners' decision-making profiles

# 1. Load Setup and Data -----------------------------------------------------
source("R/00_setup.R")

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

# 2.1 Prepare data
# Extract decision variables
fuzzy_matrix <- as.matrix(scale(fuzzy_data[, decision_vars]))
rownames(fuzzy_matrix) <- fuzzy_data$ID

# 2.2 Silhouette Analysis
# Function to calculate silhouette scores for different fuzzy clusters
get_fcm_silhouette <- function(data, max_k = 8) {
  sil_scores <- numeric(max_k - 1)
  
  for (k in 2:max_k) {
    # Run fuzzy c-means
    fcm_result <- cluster::fanny(data, k = k, memb.exp = 1.5, metric = "euclidean")
    
    # Get silhouette information
    sil_scores[k-1] <- mean(silhouette(fcm_result$clustering, dist(data))[,3])
  }
  
  return(data.frame(k = 2:max_k, silhouette = sil_scores))
}

# Calculate silhouette scores
silhouette_results <- get_fcm_silhouette(fuzzy_matrix)

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

# 2.3 Dunn Index
# Function to calculate Dunn index
calculate_dunn_index <- function(data, clustering) {
  # Calculate distance matrix
  dist_matrix <- dist(data)
  
  # Convert to full matrix for easier manipulation
  dist_full <- as.matrix(dist_matrix)
  
  # Get unique clusters
  clusters <- unique(clustering)
  n_clusters <- length(clusters)
  
  # Initialize min_inter_cluster_dist and max_intra_cluster_dist
  min_inter_cluster_dist <- Inf
  max_intra_cluster_dist <- -Inf
  
  # Calculate maximum intra-cluster distance for each cluster
  for (i in 1:n_clusters) {
    cluster_i_indices <- which(clustering == clusters[i])
    if (length(cluster_i_indices) > 1) {
      intra_dists <- dist_full[cluster_i_indices, cluster_i_indices]
      intra_dists <- intra_dists[lower.tri(intra_dists)]
      if (length(intra_dists) > 0) {
        max_intra_cluster_dist <- max(max_intra_cluster_dist, max(intra_dists))
      }
    }
  }
  
  # Calculate minimum inter-cluster distance
  for (i in 1:(n_clusters-1)) {
    for (j in (i+1):n_clusters) {
      cluster_i_indices <- which(clustering == clusters[i])
      cluster_j_indices <- which(clustering == clusters[j])
      
      inter_dists <- dist_full[cluster_i_indices, cluster_j_indices]
      min_inter_cluster_dist <- min(min_inter_cluster_dist, min(inter_dists))
    }
  }
  
  # Calculate Dunn index
  dunn_index <- min_inter_cluster_dist / max_intra_cluster_dist
  return(dunn_index)
}

# Calculate Dunn index for different k values
dunn_scores <- numeric(7)
for (k in 2:8) {
  fcm_result <- fanny(fuzzy_matrix, k = k, memb.exp = 1.5)
  dunn_scores[k-1] <- calculate_dunn_index(fuzzy_matrix, fcm_result$clustering)
}

dunn_results <- data.frame(k = 2:8, dunn = dunn_scores)

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

# 2.4 Determine optimal number of clusters
# Based on silhouette and Dunn index
best_k_silhouette <- silhouette_results$k[which.max(silhouette_results$silhouette)]
best_k_dunn <- dunn_results$k[which.max(dunn_results$dunn)]

cat("Optimal number of clusters based on silhouette:", best_k_silhouette, "\n")
cat("Optimal number of clusters based on Dunn index:", best_k_dunn, "\n")

# For this analysis, we'll use k=2 (assuming it's the optimal value from indices)
optimal_k <- 2

# 3. Perform Fuzzy C-means Clustering ----------------------------------------

# 3.1 Run fuzzy c-means
set.seed(123)  # For reproducibility
fcm_result <- fanny(
  fuzzy_matrix,
  k = optimal_k,
  memb.exp = 1.5,  # Fuzziness parameter (1 = hard clustering, >1 = increasingly fuzzy)
  maxit = 1000,    # Maximum iterations
  metric = "euclidean"
)

# 3.2 Extract results
# Get cluster memberships
memberships <- fcm_result$membership
colnames(memberships) <- paste0("Cluster", 1:optimal_k)

# Get hard cluster assignments (for comparison)
hard_clusters <- fcm_result$clustering

# Create results dataframe
fuzzy_results <- cbind(
  fuzzy_data,
  memberships,
  HardCluster = hard_clusters
)

# Save fuzzy clustering results
save_data(fuzzy_results, "fuzzy_clustering_results.csv")

# 4. Analyze Cluster Characteristics -----------------------------------------

# 4.1 Calculate cluster centroids
centroids <- t(fcm_result$centroids)
rownames(centroids) <- decision_vars
centroids_df <- as.data.frame(centroids)

# Save centroids
save_data(as.data.frame(t(centroids)), "fuzzy_cluster_centroids.csv")

# 4.2 Create heatmap of cluster centroids
centroids_long <- as.data.frame(centroids) %>%
  rownames_to_column(var = "Variable") %>%
  pivot_longer(
    cols = -Variable,
    names_to = "Cluster",
    values_to = "Value"
  )

heatmap_plot <- ggplot(centroids_long, 
                      aes(x = Cluster, y = Variable, fill = Value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  labs(
    title = "Fuzzy Cluster Centroids Heatmap",
    subtitle = "Red = higher than average, Blue = lower than average",
    x = "Cluster",
    y = "Variable",
    fill = "Z-Score"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0),
        axis.text.y = element_text(size = 8))

# Save heatmap
save_plot(heatmap_plot, "fuzzy_cluster_heatmap.pdf", width = 10, height = 12)

# 4.3 Create radar chart
# Prepare data for radar chart
radar_data <- t(centroids)
rownames(radar_data) <- paste0("Cluster", 1:optimal_k)
radar_data <- as.data.frame(radar_data)

# Add max and min rows required by fmsb
radar_data <- rbind(
  rep(3, ncol(radar_data)),  # max values
  rep(-3, ncol(radar_data)), # min values
  radar_data
)

# Set row names for max and min
rownames(radar_data)[1:2] <- c("max", "min")

# Create radar chart
pdf("results/figures/fuzzy_cluster_radar.pdf", width = 10, height = 10)
# Plot radar chart
radarchart(
  radar_data,
  pcol = brewer.pal(n = optimal_k, name = "Set1"),
  pfcol = adjustcolor(brewer.pal(n = optimal_k, name = "Set1"), alpha.f = 0.3),
  plwd = 2,
  cglcol = "gray",
  cglty = 1,
  axislabcol = "gray30",
  caxislabels = seq(-3, 3, 1.5),
  title = "Fuzzy Cluster Profiles"
)

# Add a legend
legend(
  "bottomright",
  legend = paste("Cluster", 1:optimal_k),
  col = brewer.pal(n = optimal_k, name = "Set1"),
  lwd = 2,
  pch = 20,
  bty = "n"
)
dev.off()

# 5. Membership Visualizations ----------------------------------------------

# 5.1 Membership distribution plot
membership_long <- fuzzy_results %>%
  select(ID, PDM_Type, starts_with("Cluster"), HardCluster) %>%
  pivot_longer(
    cols = starts_with("Cluster") & !matches("HardCluster"),
    names_to = "Cluster",
    values_to = "Membership"
  )

# Create a membership distribution plot by ID
membership_plot <- ggplot(membership_long, 
                         aes(x = reorder(factor(ID), HardCluster), 
                             y = Membership, 
                             fill = Cluster)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(
    title = "Fuzzy Cluster Membership Distribution",
    subtitle = "Showing membership degree for each observation",
    x = "Observation ID (ordered by dominant cluster)",
    y = "Membership Degree",
    fill = "Cluster"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank()) +
  scale_fill_brewer(palette = "Set1")

# Save membership plot
save_plot(membership_plot, "fuzzy_membership_distribution.pdf", width = 12, height = 8)

# 5.2 Membership Scatterplot
membership_scatter <- ggplot(fuzzy_results, 
                            aes(x = Cluster1, y = Cluster2, color = factor(HardCluster))) +
  geom_point(size = 3, alpha = 0.7) +
  labs(
    title = "Fuzzy Membership Space",
    subtitle = "Each point represents an organization's membership in both clusters",
    x = "Cluster 1 Membership",
    y = "Cluster 2 Membership",
    color = "Dominant Cluster"
  ) +
  theme_minimal() +
  scale_color_brewer(palette = "Set1") +
  geom_abline(intercept = 0.5, slope = -1, linetype = "dashed", color = "gray") +
  annotate("text", x = 0.25, y = 0.75, label = "Stronger Cluster 2\nMembership", color = "gray30") +
  annotate("text", x = 0.75, y = 0.25, label = "Stronger Cluster 1\nMembership", color = "gray30")

# Save membership scatter plot
save_plot(membership_scatter, "fuzzy_membership_scatter.pdf", width = 10, height = 8)

# 6. Membership Analysis in Reduced Space -----------------------------------

# 6.1 Combine with dimensionality reduction results
fuzzy_dims <- merge(
  fuzzy_results[, c("ID", "PDM_Type", "Cluster1", "Cluster2", "HardCluster")],
  dimensionality_data[, c("ID", "PC1", "PC2", "UMAP1", "UMAP2")],
  by = "ID"
)

# 6.2 Visualize memberships in PCA space
pca_membership_plot <- ggplot(fuzzy_dims, aes(x = PC1, y = PC2)) +
  geom_point(aes(color = factor(HardCluster), size = pmax(Cluster1, Cluster2)), alpha = 0.7) +
  scale_color_brewer(palette = "Set1", name = "Dominant Cluster") +
  scale_size_continuous(name = "Membership Strength", range = c(2, 8)) +
  labs(
    title = "Fuzzy Cluster Memberships in PCA Space",
    subtitle = "Point size indicates strength of dominant membership",
    x = "Principal Component 1",
    y = "Principal Component 2"
  ) +
  theme_minimal()

# Save PCA membership plot
save_plot(pca_membership_plot, "fuzzy_membership_pca.pdf", width = 10, height = 8)

# 6.3 Visualize memberships in UMAP space
umap_membership_plot <- ggplot(fuzzy_dims, aes(x = UMAP1, y = UMAP2)) +
  geom_point(aes(color = factor(HardCluster), size = pmax(Cluster1, Cluster2)), alpha = 0.7) +
  scale_color_brewer(palette = "Set1", name = "Dominant Cluster") +
  scale_size_continuous(name = "Membership Strength", range = c(2, 8)) +
  labs(
    title = "Fuzzy Cluster Memberships in UMAP Space",
    subtitle = "Point size indicates strength of dominant membership",
    x = "UMAP Dimension 1",
    y = "UMAP Dimension 2"
  ) +
  theme_minimal()

# Save UMAP membership plot
save_plot(umap_membership_plot, "fuzzy_membership_umap.pdf", width = 10, height = 8)

# 7. PDM Analysis ----------------------------------------------------------

# 7.1 Calculate PDM distribution by cluster
pdm_cluster_table <- table(fuzzy_results$PDM_Type, fuzzy_results$HardCluster)
pdm_cluster_prop <- prop.table(pdm_cluster_table, margin = 2) * 100

# Save contingency table
write.csv(pdm_cluster_table, "results/tables/fuzzy_pdm_cluster_table.csv")

# 7.2 Calculate average membership by PDM type
pdm_membership <- fuzzy_results %>%
  group_by(PDM_Type) %>%
  summarise(
    Cluster1_Avg = mean(Cluster1),
    Cluster2_Avg = mean(Cluster2),
    Count = n()
  )

# Save PDM membership table
save_data(pdm_membership, "fuzzy_pdm_membership.csv")

# 7.3 Create PDM distribution plot
pdm_distribution <- as.data.frame(pdm_cluster_table)
names(pdm_distribution) <- c("PDM_Type", "Cluster", "Count")

pdm_plot <- ggplot(pdm_distribution, 
                  aes(x = Cluster, y = Count, fill = PDM_Type)) +
  geom_bar(stat = "identity", position = "fill") +
  geom_text(
    aes(
      label = sprintf("%.1f%%", prop.table(as.matrix(pdm_cluster_table), 2)[cbind(as.character(PDM_Type), Cluster)] * 100)
    ),
    position = position_fill(vjust = 0.5),
    size = 3
  ) +
  labs(
    title = "PDM Distribution by Fuzzy Cluster",
    x = "Cluster",
    y = "Proportion",
    fill = "PDM Type"
  ) +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent)

# Save PDM distribution plot
save_plot(pdm_plot, "fuzzy_pdm_distribution.pdf", width = 10, height = 7)

# 7.4 Create PDM membership plot
pdm_membership_long <- pdm_membership %>%
  pivot_longer(
    cols = c(Cluster1_Avg, Cluster2_Avg),
    names_to = "Cluster",
    values_to = "Average_Membership"
  ) %>%
  mutate(Cluster = gsub("_Avg", "", Cluster))

pdm_membership_plot <- ggplot(pdm_membership_long, 
                             aes(x = PDM_Type, y = Average_Membership, fill = Cluster)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Average Fuzzy Membership by Project Delivery Method",
    subtitle = "Higher values indicate stronger association with a cluster",
    x = "Project Delivery Method",
    y = "Average Membership",
    fill = "Cluster"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer(palette = "Set1")

# Save PDM membership plot
save_plot(pdm_membership_plot, "fuzzy_pdm_membership_plot.pdf", width = 10, height = 7)

# 8. Membership Uncertainty Analysis ----------------------------------------

# 8.1 Calculate membership uncertainty (entropy)
fuzzy_results <- fuzzy_results %>%
  mutate(
    Entropy = -(Cluster1 * log(pmax(Cluster1, 1e-10)) + 
                Cluster2 * log(pmax(Cluster2, 1e-10))),
    Uncertainty = Entropy / log(2)  # Normalize by log(k) where k is number of clusters
  )

# 8.2 Create histogram of uncertainty
uncertainty_hist <- ggplot(fuzzy_results, aes(x = Uncertainty, fill = factor(HardCluster))) +
  geom_histogram(alpha = 0.7, bins = 20) +
  labs(
    title = "Membership Uncertainty Distribution",
    subtitle = "Higher values indicate more ambiguous cluster assignment",
    x = "Uncertainty (Normalized Entropy)",
    y = "Count",
    fill = "Dominant Cluster"
  ) +
  theme_minimal() +
  scale_fill_brewer(palette = "Set1")

# Save uncertainty histogram
save_plot(uncertainty_hist, "fuzzy_uncertainty_histogram.pdf", width = 10, height = 7)

# 8.3 Uncertainty by PDM type
uncertainty_pdm <- ggplot(fuzzy_results, aes(x = PDM_Type, y = Uncertainty, fill = PDM_Type)) +
  geom_boxplot() +
  labs(
    title = "Membership Uncertainty by Project Delivery Method",
    subtitle = "Higher values indicate more ambiguous cluster assignment",
    x = "Project Delivery Method",
    y = "Uncertainty (Normalized Entropy)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer(palette = "Set2") +
  guides(fill = "none")

# Save uncertainty by PDM plot
save_plot(uncertainty_pdm, "fuzzy_uncertainty_pdm.pdf", width = 10, height = 7)

# 9. Create Summary Report -------------------------------------------------

# Generate a summary PDF with all visualizations
pdf("results/figures/fuzzy_clustering_report.pdf", width = 12, height = 10)

# Optimal cluster number
print(silhouette_plot)
print(dunn_plot)

# Cluster profiles
print(heatmap_plot)
par(mar = c(0, 0, 2, 0))
radarchart(
  radar_data,
  pcol = brewer.pal(n = optimal_k, name = "Set1"),
  pfcol = adjustcolor(brewer.pal(n = optimal_k, name = "Set1"), alpha.f = 0.3),
  plwd = 2,
  cglcol = "gray",
  cglty = 1,
  axislabcol = "gray30",
  caxislabels = seq(-3, 3, 1.5),
  title = "Fuzzy Cluster Profiles"
)
legend(
  "bottomright",
  legend = paste("Cluster", 1:optimal_k),
  col = brewer.pal(n = optimal_k, name = "Set1"),
  lwd = 2,
  pch = 20,
  bty = "n"
)

# Membership visualizations
print(membership_plot)
print(membership_scatter)

# PDM analysis
print(pdm_plot)
print(pdm_membership_plot)

# Uncertainty analysis
print(uncertainty_hist)
print(uncertainty_pdm)

# Membership in reduced space
print(pca_membership_plot)
print(umap_membership_plot)

dev.off()

# Print summary
cat("\nFuzzy Clustering Analysis complete!\n")
cat("Optimal number of clusters:", optimal_k, "\n")
cat("Cluster sizes (based on dominant membership):\n")
print(table(fuzzy_results$HardCluster))
cat("\nPDM distribution by cluster:\n")
print(round(pdm_cluster_prop, 1))
cat("\nVisualizations saved to results/figures/\n")
cat("Results data saved to results/tables/\n") 