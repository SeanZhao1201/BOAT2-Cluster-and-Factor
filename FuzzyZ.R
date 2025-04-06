# Fuzzy C-means Clustering Analysis for Building Owners Assessment Tool (BOAT) Research
# This script implements Fuzzy C-means clustering to identify typical owner's
# decision-making profile characteristics (DMPC) and analyze relationships with
# project delivery methods (PDM)

# 1. Load Required Libraries -------------------------------------------------
library(tidyverse)    # For data manipulation and visualization
library(cluster)      # For clustering algorithms including fanny()
library(factoextra)   # For visualization of clustering results
library(e1071)        # For cmeans() function (alternative to fanny())
library(corrplot)     # For correlation visualization
library(RColorBrewer) # For color palettes
library(gridExtra)    # For arranging multiple plots together
library(fmsb)         # For radar charts

# 2. Data Preparation --------------------------------------------------------

# Read the data
boat_data <- read.csv("BOAT2_Data.csv", stringsAsFactors = TRUE)

# Create a unique ID for each observation
boat_data$ID <- 1:nrow(boat_data)

# Check for and remove outliers (such as the org with 3700 employees)
outliers <- boxplot.stats(boat_data$Org_Structure_Employees)$out
boat_data_filtered <- boat_data %>%
  filter(!Org_Structure_Employees %in% outliers)

# Calculate mean values for multi-item constructs
dmpc_data <- boat_data_filtered %>%
  mutate(
    Distribution_Centralization = (Distribution_Centralization1 + Distribution_Centralization2) / 2,
    Distribution_Formalization = (Distribution_Formalization1 + Distribution_Formalization2) / 2,
    Style_Participation = (Style_Participation1 + Style_Participation2) / 2,
    Style_Organicity = (Style_Organicity1 + Style_Organicity2) / 2,
    Style_Coercion = (Style_Coercion1 + Style_Coercion2) / 2
  )

# Define variables for clustering
numerical_vars <- c(
  "Org_Structure_Employees",
  "Org_Structure_Locations",
  "Org_Structure_Depts",
  "Org_Structure_Layers"
)

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

# Create ID column if it doesn't exist
if(!"ID" %in% colnames(dmpc_data)) {
  dmpc_data$ID <- 1:nrow(dmpc_data)
}

# Select variables for clustering
cluster_data <- dmpc_data %>%
  select(ID, all_of(c(numerical_vars, decision_vars)), PDM_Type)

# Handle missing values
cat("Missing values before imputation:\n")
print(colSums(is.na(cluster_data)))

# Impute missing values with column medians
cluster_data <- cluster_data %>%
  mutate(across(where(is.numeric), ~ifelse(is.na(.), median(., na.rm = TRUE), .)))

# Extract ID and PDM_Type for later use
ids <- cluster_data$ID
pdm_types <- cluster_data$PDM_Type

# Prepare data for clustering (exclude ID and PDM_Type)
cluster_matrix <- cluster_data %>%
  select(-ID, -PDM_Type) %>%
  as.matrix()

# Scale the data for clustering
cluster_scaled <- scale(cluster_matrix)
rownames(cluster_scaled) <- ids

# 3. Determine Optimal Number of Clusters ------------------------------------

# Function to calculate silhouette scores for different fuzzy c-means clusters
get_fcm_silhouette <- function(data, max_k = 8) {
  sil_scores <- numeric(max_k - 1)
  
  for (k in 2:max_k) {
    # Run fuzzy c-means
    fcm_result <- fanny(data, k = k, memb.exp = 1.5, metric = "euclidean")
    
    # Get silhouette information
    sil_scores[k-1] <- mean(silhouette(fcm_result$clustering, dist(data))[,3])
  }
  
  return(data.frame(k = 2:max_k, silhouette = sil_scores))
}

# Calculate silhouette scores
sil_results <- get_fcm_silhouette(cluster_scaled)

# Plot silhouette scores
silhouette_plot <- ggplot(sil_results, aes(x = k, y = silhouette)) +
  geom_line() +
  geom_point(size = 3) +
  labs(title = "Silhouette Scores for Different Numbers of Clusters",
       x = "Number of Clusters (k)",
       y = "Average Silhouette Width") +
  theme_minimal()

# Dunn index calculation function
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
  fcm_result <- fanny(cluster_scaled, k = k, memb.exp = 1.5)
  dunn_scores[k-1] <- calculate_dunn_index(cluster_scaled, fcm_result$clustering)
}

dunn_results <- data.frame(k = 2:8, dunn = dunn_scores)

# Plot Dunn index
dunn_plot <- ggplot(dunn_results, aes(x = k, y = dunn)) +
  geom_line() +
  geom_point(size = 3) +
  labs(title = "Dunn Index for Different Numbers of Clusters",
       x = "Number of Clusters (k)",
       y = "Dunn Index") +
  theme_minimal()

# Arrange both plots side by side
validation_plots <- grid.arrange(silhouette_plot, dunn_plot, ncol = 2)

# Determine optimal number of clusters (based on both silhouette and Dunn index)
best_k <- dunn_results$k[which.max(dunn_results$dunn)]
cat("Optimal number of clusters based on Dunn index:", best_k, "\n")

best_sil_k <- sil_results$k[which.max(sil_results$silhouette)]
cat("Optimal number of clusters based on silhouette:", best_sil_k, "\n")

# We'll proceed with the optimal k value from Dunn index
k <- best_k
cat("Selected number of clusters for analysis:", k, "\n")

# 4. Perform Fuzzy C-means Clustering ----------------------------------------

# Set random seed for reproducibility
set.seed(123)

# Run fuzzy c-means clustering
fcm_result <- fanny(
  cluster_scaled,
  k = k,
  memb.exp = 1.5,  # Fuzziness parameter (1 = hard clustering, >1 = increasingly fuzzy)
  maxit = 1000,    # Maximum iterations
  metric = "euclidean"
)

# Get cluster memberships
memberships <- fcm_result$membership
colnames(memberships) <- paste0("Cluster", 1:k)

# Get hard cluster assignments (for comparison)
hard_clusters <- fcm_result$clustering

# Add results back to original data
cluster_results <- cbind(
  cluster_data,
  memberships,
  HardCluster = hard_clusters
)

# 5. Visualize Clustering Results --------------------------------------------

# 5.1 Membership Plot
# Prepare data for membership visualization
membership_long <- as.data.frame(memberships) %>%
  mutate(ID = ids) %>%
  pivot_longer(
    cols = starts_with("Cluster"),
    names_to = "Cluster",
    values_to = "Membership"
  ) %>%
  mutate(
    PDM_Type = rep(pdm_types, each = k)
  )

# Sort by hard cluster and then by membership within cluster
membership_plot_data <- membership_long %>%
  mutate(
    HardCluster = rep(hard_clusters, each = k)
  ) %>%
  arrange(HardCluster, desc(Membership))

# Create a unique ID for plotting
membership_plot_data$PlotID <- rep(1:length(ids), each = k)

# Plot memberships
membership_plot <- ggplot(membership_plot_data, 
                          aes(x = reorder(PlotID, HardCluster), 
                              y = Membership, 
                              fill = Cluster)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Fuzzy C-means Cluster Memberships",
       x = "Observations (sorted by dominant cluster)",
       y = "Membership Degree") +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  scale_fill_brewer(palette = "Set1")

# 5.2 Cluster Profiles Heatmap
# Calculate cluster centroids
centroids <- t(fcm_result$centroids)
rownames(centroids) <- colnames(cluster_matrix)
centroids_df <- as.data.frame(centroids)

# Convert to long format for plotting
centroids_long <- centroids_df %>%
  rownames_to_column(var = "Variable") %>%
  pivot_longer(
    cols = -Variable,
    names_to = "Cluster",
    values_to = "Value"
  )

# Create heatmap
heatmap_plot <- ggplot(centroids_long, aes(x = Cluster, y = Variable, fill = Value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  labs(title = "Cluster Centroids Heatmap",
       x = "Cluster",
       y = "Variable") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0),
        axis.text.y = element_text(size = 8))

# 5.3 Radar Charts for Cluster Profiles
# Prepare data for radar charts
radar_data <- centroids_df
radar_data <- rbind(rep(3, ncol(radar_data)), rep(-3, ncol(radar_data)), radar_data)
rownames(radar_data)[1:2] <- c("max", "min")

# Function to create radar chart for a specific cluster
create_radar_chart <- function(radar_data, cluster_id) {
  colors <- brewer.pal(8, "Set2")
  
  radarchart(
    radar_data[c(1, 2, cluster_id + 2), ],
    pcol = colors[cluster_id],
    pfcol = adjustcolor(colors[cluster_id], alpha.f = 0.5),
    cglcol = "grey",
    cglty = 1,
    axislabcol = "grey50",
    caxislabels = seq(-3, 3, 1),
    title = paste("Cluster", cluster_id, "Profile")
  )
}

# 5.4 Project Delivery Method Analysis
# Calculate mean membership by PDM type
pdm_membership <- aggregate(memberships, by = list(PDM = pdm_types), mean)

# Convert to long format for plotting
pdm_membership_long <- pdm_membership %>%
  pivot_longer(
    cols = -PDM,
    names_to = "Cluster",
    values_to = "Mean_Membership"
  )

# Create PDM membership plot
pdm_plot <- ggplot(pdm_membership_long, 
                   aes(x = PDM, y = Mean_Membership, fill = Cluster)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Average Cluster Membership by Project Delivery Method",
       x = "Project Delivery Method",
       y = "Mean Membership Degree") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer(palette = "Set1")

# Calculate contingency table of PDM types by dominant cluster
pdm_cluster_table <- table(pdm_types, hard_clusters)
pdm_cluster_percent <- prop.table(pdm_cluster_table, margin = 2) * 100

# 5.5 Decision Making Characteristics Co-occurrence Analysis
# Calculate correlation matrix for decision variables
decision_corr <- cor(cluster_data[, decision_vars], use = "pairwise.complete.obs")

# Create correlation heatmap
corr_plot <- corrplot(
  decision_corr,
  method = "color",
  type = "upper",
  order = "hclust",
  tl.col = "black",
  tl.srt = 45,
  addCoef.col = "black",
  number.cex = 0.7,
  diag = FALSE
)

# 6. Save Results ------------------------------------------------------------

# Save cluster membership results
write.csv(cluster_results, "BOAT2_Fuzzy_Clustering_Results.csv", row.names = FALSE)

# Save cluster centroids
write.csv(centroids, "BOAT2_Fuzzy_Cluster_Centroids.csv")

# Save PDM by cluster table
write.csv(pdm_cluster_table, "BOAT2_PDM_Fuzzy_Cluster_Table.csv")

# Print mean membership by PDM type
print("Mean Membership by Project Delivery Method:")
print(pdm_membership)

# Print silhouette scores
print("Silhouette Scores by k:")
print(sil_results)

# 7. Generate PDF Report -----------------------------------------------------
pdf("BOAT2_Fuzzy_Clustering_Report.pdf", width = 12, height = 10)

# Plot cluster validation metrics
grid.arrange(silhouette_plot, dunn_plot, ncol = 2)

# Plot membership
print(membership_plot)

# Plot heatmap
print(heatmap_plot)

# Plot radar charts
par(mfrow = c(ceiling(k/2), 2))
for (i in 1:k) {
  create_radar_chart(radar_data, i)
}

# Plot PDM analysis
print(pdm_plot)

# Plot correlation matrix
corrplot(
  decision_corr,
  method = "color",
  type = "upper",
  order = "hclust",
  tl.col = "black",
  tl.srt = 45
)

dev.off()

# 8. Print Summary Report ----------------------------------------------------

cat("\n=====================================================\n")
cat("Fuzzy C-means Clustering Analysis for BOAT Research\n")
cat("=====================================================\n\n")

cat("Number of clusters:", k, "\n\n")

cat("Cluster sizes (based on dominant membership):\n")
print(table(hard_clusters))

cat("\nMean silhouette width:", mean(silhouette(hard_clusters, dist(cluster_scaled))[,3]), "\n\n")

cat("Cluster centroids (decision-making variables only):\n")
print(centroids[decision_vars,])

cat("\nPDM distribution across clusters (percentages):\n")
print(round(pdm_cluster_percent, 1))

cat("\nAnalysis Summary:\n")
cat("- The fuzzy c-means clustering identified", k, "distinct decision-making profiles\n")
cat("- These profiles show varying degrees of membership across the dataset\n")
cat("- Certain DMPCs tend to co-occur, as shown in the correlation heatmap\n")
cat("- Project delivery methods show pattern alignment with specific DMPC profiles\n")
cat("\nComplete results are available in the generated CSV files and PDF report\n")
