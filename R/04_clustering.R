# BOAT2 Cluster and Factor Analysis - Clustering Analysis
# This script performs hierarchical and K-prototype clustering analyses

# 1. Load Setup and Data -----------------------------------------------------
source("R/00_setup.R")

# Load the prepared datasets
hierarchical_data <- read.csv("results/tables/hierarchical_data.csv")
kproto_data <- read.csv("results/tables/kproto_data.csv")

# Define variable groups
merged_ordinal_vars <- c(
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

numerical_org_vars <- c(
  "Org_Structure_Employees",
  "Org_Structure_Locations",
  "Org_Structure_Depts",
  "Org_Structure_Layers"
)

# 2. Hierarchical Clustering -------------------------------------------------

# 2.1 Prepare data for hierarchical clustering
# Extract relevant columns
hc_data <- hierarchical_data %>%
  select(all_of(merged_ordinal_vars))

# Scale the data
hc_data_scaled <- scale(hc_data)
rownames(hc_data_scaled) <- hierarchical_data$ID

# 2.2 Compute distance matrix
dist_matrix <- dist(hc_data_scaled, method = "euclidean")

# 2.3 Determine optimal number of clusters

# Elbow method
wss <- numeric(10)
for (i in 1:10) {
  wss[i] <- sum(kmeans(hc_data_scaled, centers = i, nstart = 25)$withinss)
}

# Create elbow plot
elbow_plot <- ggplot(data.frame(k = 1:10, wss = wss), aes(x = k, y = wss)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Elbow Method for Optimal Cluster Number",
    x = "Number of Clusters (k)",
    y = "Within-cluster Sum of Squares"
  ) +
  theme_minimal()

# Save elbow plot
save_plot(elbow_plot, "hierarchical_elbow_plot.pdf", width = 10, height = 7)

# Silhouette method
silhouette_results <- data.frame(
  k = 2:9,
  avg_silhouette = numeric(8)
)

for (k in 2:9) {
  # Create clusters
  hc_clusters <- cutree(hclust(dist_matrix, method = "ward.D2"), k = k)
  # Calculate silhouette
  sil <- silhouette(hc_clusters, dist_matrix)
  silhouette_results$avg_silhouette[k-1] <- mean(sil[, 3])
}

# Create silhouette plot
silhouette_plot <- ggplot(silhouette_results, aes(x = k, y = avg_silhouette)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Silhouette Analysis for Optimal Cluster Number",
    x = "Number of Clusters (k)",
    y = "Average Silhouette Width"
  ) +
  theme_minimal()

# Save silhouette plot
save_plot(silhouette_plot, "hierarchical_silhouette_plot.pdf", width = 10, height = 7)

# Gap statistic method (if not too computationally intensive)
if (nrow(hc_data_scaled) < 200) {  # Only run for smaller datasets
  gap_stat <- clusGap(
    hc_data_scaled, 
    FUN = function(x, k) list(cluster = cutree(hclust(dist(x), method = "ward.D2"), k = k)),
    K.max = 9, 
    B = 50  # Reduced for speed, increase for production
  )
  
  # Create gap statistic plot
  gap_plot <- ggplot(
    data.frame(
      k = 1:9,
      gap = gap_stat$Tab[, "gap"],
      se = gap_stat$Tab[, "SE.sim"]
    ),
    aes(x = k, y = gap)
  ) +
    geom_line() +
    geom_point() +
    geom_errorbar(aes(ymin = gap - se, ymax = gap + se), width = 0.1) +
    labs(
      title = "Gap Statistic Analysis for Optimal Cluster Number",
      x = "Number of Clusters (k)",
      y = "Gap Statistic (with SE)"
    ) +
    theme_minimal()
  
  # Save gap plot
  save_plot(gap_plot, "hierarchical_gap_plot.pdf", width = 10, height = 7)
}

# Based on the above analyses, determine optimal number of clusters
# For this script, we'll use k=3 as an example but adjust based on results
optimal_k <- 3

# 2.4 Perform hierarchical clustering
hc_result <- hclust(dist_matrix, method = "ward.D2")
hc_clusters <- cutree(hc_result, k = optimal_k)

# 2.5 Create clustering results dataframe
hc_results_df <- hierarchical_data %>%
  mutate(Cluster = hc_clusters)

# Save hierarchical clustering results
save_data(hc_results_df, "hierarchical_clusters.csv")

# 2.6 Visualize clusters

# Dendrogram with clusters highlighted
pdf("results/figures/hierarchical_dendrogram.pdf", width = 12, height = 8)
par(mar = c(6, 4, 2, 8))  # Adjust margins for better visualization

# Plot the dendrogram
plot(
  as.dendrogram(hc_result),
  main = "Hierarchical Clustering Dendrogram",
  xlab = "",
  ylab = "Height (Dissimilarity)",
  horiz = FALSE,
  cex = 0.6
)

# Add rectangles around clusters
rect.hclust(hc_result, k = optimal_k, border = 2:(optimal_k+1))

# Add cluster labels
cluster_centers <- tapply(1:length(hc_clusters), hc_clusters, mean)
text(
  cluster_centers, 
  par("usr")[4] * 0.95, 
  labels = paste("Cluster", 1:optimal_k), 
  cex = 1.2, 
  font = 2,
  col = 2:(optimal_k+1)
)

dev.off()

# Dendrogram colored by PDM type
pdf("results/figures/hierarchical_dendrogram_pdm.pdf", width = 12, height = 8)
par(mar = c(6, 4, 2, 8))  # Adjust margins for better visualization

# Get PDM types
pdm_types <- hierarchical_data$PDM_Type

# Define color palette for PDM types
pdm_unique <- unique(pdm_types)
pdm_colors <- brewer.pal(length(pdm_unique), "Set1")
names(pdm_colors) <- pdm_unique

# Map PDM types to colors
pdm_color_mapping <- pdm_colors[pdm_types]

# Create dendrogram
dend <- as.dendrogram(hc_result)

# Color labels by PDM type
labels_colors(dend) <- pdm_color_mapping[order.dendrogram(dend)]

# Plot the dendrogram
plot(
  dend,
  main = "Hierarchical Clustering Dendrogram Colored by PDM Type",
  xlab = "",
  ylab = "Height (Dissimilarity)",
  horiz = FALSE,
  cex = 0.6
)

# Add a legend for PDM types
legend(
  "topright",
  legend = names(pdm_colors),
  fill = pdm_colors,
  title = "Project Delivery Method",
  cex = 0.8,
  bg = "white"
)

dev.off()

# 3. K-Prototypes Clustering -------------------------------------------------

# 3.1 Prepare data for K-prototypes clustering
# K-prototypes can handle mixed data types (numerical and categorical)

# First, identify numerical and categorical variables
kproto_numerical_vars <- c(numerical_org_vars)
kproto_categorical_vars <- setdiff(colnames(kproto_data), c("ID", "PDM_Type", kproto_numerical_vars))

# Create a mixed dataset
kproto_mixed_data <- kproto_data %>%
  select(all_of(c(kproto_numerical_vars, kproto_categorical_vars)))

# Convert ordinal variables to ordered factors if needed
kproto_mixed_data <- kproto_mixed_data %>%
  mutate(across(all_of(kproto_categorical_vars), ~ ordered(round(.), levels = 1:5)))

# 3.2 Determine optimal number of clusters
# Calculate Gower distance (suitable for mixed data types)
gower_dist <- daisy(kproto_mixed_data, metric = "gower")

# PAM silhouette analysis
pam_silhouette_results <- data.frame(
  k = 2:9,
  avg_silhouette = numeric(8)
)

for (k in 2:9) {
  pam_fit <- pam(gower_dist, k = k, diss = TRUE)
  pam_silhouette_results$avg_silhouette[k-1] <- pam_fit$silinfo$avg.width
}

# Create PAM silhouette plot
pam_silhouette_plot <- ggplot(pam_silhouette_results, aes(x = k, y = avg_silhouette)) +
  geom_line() +
  geom_point() +
  labs(
    title = "PAM Silhouette Analysis for Optimal Cluster Number",
    x = "Number of Clusters (k)",
    y = "Average Silhouette Width"
  ) +
  theme_minimal()

# Save PAM silhouette plot
save_plot(pam_silhouette_plot, "kproto_pam_silhouette_plot.pdf", width = 10, height = 7)

# 3.3 Perform k-prototypes clustering with optimal k
# Based on the above analysis, determine optimal number of clusters
# For this script, we'll use k=2 and k=3 for comparison
kproto_k_values <- c(2, 3)

# Create a list to store results
kproto_results <- list()

# Run k-prototypes for each k value
for (k in kproto_k_values) {
  # Set seed for reproducibility
  set.seed(123)
  
  # Run k-prototypes
  kproto_result <- clustMixType::kproto(
    kproto_mixed_data, 
    k = k,
    verbose = FALSE
  )
  
  # Create results dataframe
  kproto_results[[paste0("k", k)]] <- kproto_data %>%
    mutate(Cluster = kproto_result$cluster)
  
  # Save k-prototypes clustering results
  save_data(
    kproto_results[[paste0("k", k)]], 
    paste0("kproto_clusters_k", k, ".csv")
  )
  
  # Create visualization of cluster characteristics
  # Calculate cluster centroids
  kproto_centroids <- data.frame(
    Cluster = 1:k,
    kproto_result$centers
  )
  
  # Save centroids
  save_data(
    kproto_centroids, 
    paste0("kproto_centroids_k", k, ".csv")
  )
}

# 3.4 Compare clustering solutions
# Create a combined dataset with both k=2 and k=3 results
kproto_comparison <- kproto_data %>%
  mutate(
    Cluster_k2 = kproto_results[["k2"]]$Cluster,
    Cluster_k3 = kproto_results[["k3"]]$Cluster
  )

# Save comparison results
save_data(kproto_comparison, "kproto_comparison.csv")

# 4. PDM Analysis with Clustering Results ------------------------------------

# 4.1 Create contingency tables of PDM type by cluster
# Hierarchical clustering
hc_pdm_table <- table(hierarchical_data$PDM_Type, hc_clusters)
hc_pdm_prop <- prop.table(hc_pdm_table, margin = 2) * 100

# K-prototypes clustering (k=2)
kproto_k2_pdm_table <- table(kproto_data$PDM_Type, kproto_results[["k2"]]$Cluster)
kproto_k2_pdm_prop <- prop.table(kproto_k2_pdm_table, margin = 2) * 100

# K-prototypes clustering (k=3)
kproto_k3_pdm_table <- table(kproto_data$PDM_Type, kproto_results[["k3"]]$Cluster)
kproto_k3_pdm_prop <- prop.table(kproto_k3_pdm_table, margin = 2) * 100

# Save PDM tables
write.csv(hc_pdm_table, "results/tables/hierarchical_pdm_table.csv")
write.csv(kproto_k2_pdm_table, "results/tables/kproto_k2_pdm_table.csv")
write.csv(kproto_k3_pdm_table, "results/tables/kproto_k3_pdm_table.csv")

# 4.2 Create PDM-Cluster visualization
# Function to create PDM distribution plot
create_pdm_plot <- function(pdm_table, title) {
  # Convert to data frame for ggplot
  pdm_df <- as.data.frame(pdm_table)
  colnames(pdm_df) <- c("PDM_Type", "Cluster", "Count")
  pdm_df$Cluster <- paste("Cluster", pdm_df$Cluster)
  
  # Create plot
  p <- ggplot(pdm_df, aes(x = Cluster, y = Count, fill = PDM_Type)) +
    geom_bar(stat = "identity", position = "fill") +
    geom_text(
      aes(
        label = sprintf("%.1f%%", Count / sum(Count[Cluster == Cluster]) * 100)
      ),
      position = position_fill(vjust = 0.5),
      size = 3
    ) +
    labs(
      title = title,
      x = "Cluster",
      y = "Proportion",
      fill = "PDM Type"
    ) +
    theme_minimal() +
    scale_y_continuous(labels = scales::percent)
  
  return(p)
}

# Create PDM distribution plots
hc_pdm_plot <- create_pdm_plot(
  hc_pdm_table, 
  "PDM Distribution by Hierarchical Cluster"
)

kproto_k2_pdm_plot <- create_pdm_plot(
  kproto_k2_pdm_table, 
  "PDM Distribution by K-Prototypes Cluster (k=2)"
)

kproto_k3_pdm_plot <- create_pdm_plot(
  kproto_k3_pdm_table, 
  "PDM Distribution by K-Prototypes Cluster (k=3)"
)

# Save PDM distribution plots
save_plot(hc_pdm_plot, "hierarchical_pdm_distribution.pdf", width = 10, height = 7)
save_plot(kproto_k2_pdm_plot, "kproto_k2_pdm_distribution.pdf", width = 10, height = 7)
save_plot(kproto_k3_pdm_plot, "kproto_k3_pdm_distribution.pdf", width = 10, height = 7)

# 5. Cluster Characteristics Analysis ----------------------------------------

# 5.1 Hierarchical Cluster Characteristics
# Calculate mean values for each cluster
hc_characteristics <- hc_results_df %>%
  group_by(Cluster) %>%
  summarise(across(all_of(merged_ordinal_vars), mean))

# Save characteristics
save_data(hc_characteristics, "hierarchical_cluster_characteristics.csv")

# Create radar chart data
hc_radar_data <- hc_characteristics %>%
  select(-Cluster) %>%
  as.data.frame()

# Add row names for the radar chart
rownames(hc_radar_data) <- paste("Cluster", hc_characteristics$Cluster)

# Add max and min rows required by fmsb
hc_radar_data <- rbind(
  rep(5, ncol(hc_radar_data)),  # max values
  rep(1, ncol(hc_radar_data)),  # min values
  hc_radar_data
)

# Set row names for max and min
rownames(hc_radar_data)[1:2] <- c("max", "min")

# Create radar chart
pdf("results/figures/hierarchical_cluster_radar.pdf", width = 10, height = 10)
# Plot radar chart
radarchart(
  hc_radar_data,
  pcol = brewer.pal(n = optimal_k, name = "Set1"),
  pfcol = adjustcolor(brewer.pal(n = optimal_k, name = "Set1"), alpha.f = 0.3),
  plwd = 2,
  cglcol = "gray",
  cglty = 1,
  axislabcol = "gray30",
  title = "Hierarchical Cluster Profiles"
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

# 5.2 K-Prototypes (k=2) Cluster Characteristics
# Calculate mean values for each cluster
kproto_k2_characteristics <- kproto_results[["k2"]] %>%
  group_by(Cluster) %>%
  summarise(across(all_of(kproto_categorical_vars), ~ mean(as.numeric(as.character(.)))))

# Save characteristics
save_data(kproto_k2_characteristics, "kproto_k2_cluster_characteristics.csv")

# Create radar chart data
kproto_k2_radar_data <- kproto_k2_characteristics %>%
  select(-Cluster) %>%
  as.data.frame()

# Add row names for the radar chart
rownames(kproto_k2_radar_data) <- paste("Cluster", kproto_k2_characteristics$Cluster)

# Add max and min rows required by fmsb
kproto_k2_radar_data <- rbind(
  rep(5, ncol(kproto_k2_radar_data)),  # max values
  rep(1, ncol(kproto_k2_radar_data)),  # min values
  kproto_k2_radar_data
)

# Set row names for max and min
rownames(kproto_k2_radar_data)[1:2] <- c("max", "min")

# Create radar chart
pdf("results/figures/kproto_k2_cluster_radar.pdf", width = 10, height = 10)
# Plot radar chart
radarchart(
  kproto_k2_radar_data,
  pcol = brewer.pal(n = 2, name = "Set1"),
  pfcol = adjustcolor(brewer.pal(n = 2, name = "Set1"), alpha.f = 0.3),
  plwd = 2,
  cglcol = "gray",
  cglty = 1,
  axislabcol = "gray30",
  title = "K-Prototypes (k=2) Cluster Profiles"
)

# Add a legend
legend(
  "bottomright",
  legend = paste("Cluster", 1:2),
  col = brewer.pal(n = 2, name = "Set1"),
  lwd = 2,
  pch = 20,
  bty = "n"
)
dev.off()

# 5.3 K-Prototypes (k=3) Cluster Characteristics
# Calculate mean values for each cluster
kproto_k3_characteristics <- kproto_results[["k3"]] %>%
  group_by(Cluster) %>%
  summarise(across(all_of(kproto_categorical_vars), ~ mean(as.numeric(as.character(.)))))

# Save characteristics
save_data(kproto_k3_characteristics, "kproto_k3_cluster_characteristics.csv")

# Create radar chart data
kproto_k3_radar_data <- kproto_k3_characteristics %>%
  select(-Cluster) %>%
  as.data.frame()

# Add row names for the radar chart
rownames(kproto_k3_radar_data) <- paste("Cluster", kproto_k3_characteristics$Cluster)

# Add max and min rows required by fmsb
kproto_k3_radar_data <- rbind(
  rep(5, ncol(kproto_k3_radar_data)),  # max values
  rep(1, ncol(kproto_k3_radar_data)),  # min values
  kproto_k3_radar_data
)

# Set row names for max and min
rownames(kproto_k3_radar_data)[1:2] <- c("max", "min")

# Create radar chart
pdf("results/figures/kproto_k3_cluster_radar.pdf", width = 10, height = 10)
# Plot radar chart
radarchart(
  kproto_k3_radar_data,
  pcol = brewer.pal(n = 3, name = "Set1"),
  pfcol = adjustcolor(brewer.pal(n = 3, name = "Set1"), alpha.f = 0.3),
  plwd = 2,
  cglcol = "gray",
  cglty = 1,
  axislabcol = "gray30",
  title = "K-Prototypes (k=3) Cluster Profiles"
)

# Add a legend
legend(
  "bottomright",
  legend = paste("Cluster", 1:3),
  col = brewer.pal(n = 3, name = "Set1"),
  lwd = 2,
  pch = 20,
  bty = "n"
)
dev.off()

# 6. Create Comparative Summary PDF ------------------------------------------

# Generate a summary PDF combining key visualizations
pdf("results/figures/clustering_analysis_summary.pdf", width = 12, height = 10)

# Optimal number of clusters
print(elbow_plot)
print(silhouette_plot)
if (exists("gap_plot")) print(gap_plot)
print(pam_silhouette_plot)

# PDM distribution by cluster
print(hc_pdm_plot)
print(kproto_k2_pdm_plot)
print(kproto_k3_pdm_plot)

# Close PDF
dev.off()

# Print completion message
cat("\nClustering Analysis complete!\n")
cat("Visualizations saved to results/figures/\n")
cat("Results data saved to results/tables/\n") 