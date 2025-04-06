# BOAT2 Fuzzy C-means Clustering Visualizations
# This script creates visualizations for the BOAT2 Fuzzy Clustering Results

# 1. Load Required Libraries -------------------------------------------------
library(tidyverse)    # For data manipulation and visualization
library(cluster)      # For clustering evaluation
library(factoextra)   # For visualization of clustering results
library(corrplot)     # For correlation visualization
library(RColorBrewer) # For color palettes
library(gridExtra)    # For arranging multiple plots together
library(fmsb)         # For radar charts
library(ggrepel)      # For text labels that don't overlap
# Check if devtools is installed, if not install it
if (!require("devtools")) {
  install.packages("devtools")
}

# Check if ggradar is installed, if not install it
if (!require("ggradar")) {
  devtools::install_github("ricardo-bion/ggradar")
}

# We'll use fmsb package for radar charts instead of ggradar
# ggradar is not on CRAN and requires GitHub installation

# 2. Load Results -----------------------------------------------------------
# Read the clustering results
cluster_results <- read.csv("BOAT2_Fuzzy_Clustering_Results.csv")

# 3. Membership Visualizations ----------------------------------------------

# 3.1 Membership distribution plot
membership_long <- cluster_results %>%
  select(ID, PDM_Type, Cluster1, Cluster2, HardCluster) %>%
  pivot_longer(
    cols = c(Cluster1, Cluster2),
    names_to = "Cluster",
    values_to = "Membership"
  )

# Create a membership distribution plot by ID
membership_plot <- ggplot(membership_long, 
                          aes(x = reorder(factor(ID), HardCluster), 
                              y = Membership, 
                              fill = Cluster)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(title = "Fuzzy Cluster Membership Distribution",
       subtitle = "Showing membership degree for each observation",
       x = "Observation ID (ordered by dominant cluster)",
       y = "Membership Degree",
       fill = "Cluster") +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank()) +
  scale_fill_brewer(palette = "Set1")

# 3.2 Membership Scatterplot
# Create a scatterplot of membership values
membership_scatter <- ggplot(cluster_results, aes(x = Cluster1, y = Cluster2)) +
  geom_point(aes(color = factor(HardCluster), size = 3)) +
  labs(title = "Fuzzy Membership Space",
       subtitle = "Each point represents an organization's membership in both clusters",
       x = "Cluster 1 Membership",
       y = "Cluster 2 Membership",
       color = "Dominant Cluster") +
  theme_minimal() +
  scale_color_brewer(palette = "Set1") +
  guides(size = "none") +
  geom_abline(intercept = 0.5, slope = 1, linetype = "dashed", color = "gray") +
  annotate("text", x = 0.25, y = 0.75, label = "Stronger Cluster 2\nMembership", color = "gray30") +
  annotate("text", x = 0.75, y = 0.25, label = "Stronger Cluster 1\nMembership", color = "gray30")

# 4. Cluster Characteristics Visualizations ---------------------------------

# 4.1 Calculate cluster centroids by hard cluster
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

# Scale decision variables for visualization
cluster_results_scaled <- cluster_results %>%
  mutate(across(all_of(decision_vars), scale))

# Calculate centroids
centroids <- cluster_results_scaled %>%
  group_by(HardCluster) %>%
  summarise(across(all_of(decision_vars), mean))

# Convert to long format for plotting
centroids_long <- centroids %>%
  pivot_longer(
    cols = -HardCluster,
    names_to = "Variable",
    values_to = "Value"
  )

# Create heatmap of cluster centroids
heatmap_plot <- ggplot(centroids_long, aes(x = Variable, y = factor(HardCluster), fill = Value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  labs(title = "Cluster Centroids Heatmap",
       subtitle = "Red = higher than average, Blue = lower than average",
       x = "",
       y = "Cluster",
       fill = "Z-Score") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))

# 4.2 Radar Chart for Cluster Profiles
# Prepare data for radar chart
radar_data <- centroids %>%
  select(-HardCluster) %>%
  as.data.frame()

# Add row names for the radar chart
rownames(radar_data) <- paste("Cluster", centroids$HardCluster)

# Add max and min rows required by fmsb
radar_data <- rbind(
  rep(3, ncol(radar_data)),  # max values
  rep(-3, ncol(radar_data)), # min values
  radar_data
)

# Set row names for max and min
rownames(radar_data)[1:2] <- c("max", "min")

# Function to create a radar chart
create_radar <- function() {
  # Set up plotting area
  par(mar = c(0, 0, 2, 0))
  
  # Plot radar chart
  radarchart(
    radar_data,
    pcol = brewer.pal(n = 2, name = "Set1"),
    pfcol = adjustcolor(brewer.pal(n = 2, name = "Set1"), alpha.f = 0.3),
    plwd = 2,
    cglcol = "gray",
    cglty = 1,
    axislabcol = "gray30",
    caxislabels = seq(-3, 3, 1.5),
    title = "Cluster Profiles Comparison"
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
}

# 5. Project Delivery Method Analysis ---------------------------------------

# 5.1 PDM by Cluster Visualization
pdm_cluster <- table(cluster_results$PDM_Type, cluster_results$HardCluster)
pdm_cluster_df <- as.data.frame.matrix(pdm_cluster)
pdm_cluster_df$PDM_Type <- rownames(pdm_cluster_df)

# Convert to long format for plotting
pdm_cluster_long <- pdm_cluster_df %>%
  pivot_longer(
    cols = -PDM_Type,
    names_to = "Cluster",
    values_to = "Count"
  )

# Create a grouped bar chart
pdm_bar <- ggplot(pdm_cluster_long, aes(x = PDM_Type, y = Count, fill = Cluster)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(title = "Project Delivery Methods by Cluster",
       subtitle = "Distribution of PDM types across clusters",
       x = "Project Delivery Method",
       y = "Count",
       fill = "Cluster") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer(palette = "Set1")

# 5.2 PDM by Fuzzy Membership
# Calculate average membership by PDM type
pdm_membership <- cluster_results %>%
  group_by(PDM_Type) %>%
  summarise(
    Cluster1_Avg = mean(Cluster1),
    Cluster2_Avg = mean(Cluster2),
    Count = n()
  )

# Convert to long format for plotting
pdm_membership_long <- pdm_membership %>%
  pivot_longer(
    cols = c(Cluster1_Avg, Cluster2_Avg),
    names_to = "Cluster",
    values_to = "Avg_Membership"
  ) %>%
  mutate(Cluster = gsub("_Avg", "", Cluster))

# Create a grouped bar chart of average memberships
pdm_membership_bar <- ggplot(pdm_membership_long, 
                             aes(x = PDM_Type, y = Avg_Membership, fill = Cluster)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(title = "Average Fuzzy Membership by Project Delivery Method",
       subtitle = "Higher values indicate stronger association with a cluster",
       x = "Project Delivery Method",
       y = "Average Membership",
       fill = "Cluster") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer(palette = "Set1")

# 6. Organizational Characteristics by Cluster ------------------------------

# 6.1 Organization Size Analysis
size_box <- ggplot(cluster_results, 
                   aes(x = factor(HardCluster), y = Org_Structure_Employees, fill = factor(HardCluster))) +
  geom_boxplot() +
  labs(title = "Organization Size by Cluster",
       subtitle = "Number of employees across clusters",
       x = "Cluster",
       y = "Number of Employees") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set1") +
  guides(fill = "none")

# 6.2 Organization Structure Variables
structure_vars <- c("Org_Structure_Locations", "Org_Structure_Depts", "Org_Structure_Layers")

# Convert to long format
structure_long <- cluster_results %>%
  select(ID, HardCluster, all_of(structure_vars)) %>%
  pivot_longer(
    cols = all_of(structure_vars),
    names_to = "Variable",
    values_to = "Value"
  ) %>%
  mutate(Variable = gsub("Org_Structure_", "", Variable))  # Clean variable names

# Create boxplots for organizational structure variables
structure_box <- ggplot(structure_long, 
                        aes(x = factor(HardCluster), y = Value, fill = factor(HardCluster))) +
  geom_boxplot() +
  facet_wrap(~ Variable, scales = "free_y") +
  labs(title = "Organizational Structure by Cluster",
       x = "Cluster",
       y = "Value") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set1") +
  guides(fill = "none")

# 7. Decision Making Characteristics Analysis ------------------------------

# 7.1 Decision Variables Correlation Matrix
decision_cor <- cor(cluster_results[, decision_vars], use = "pairwise.complete.obs")

# Create a correlation plot function
create_cor_plot <- function() {
  corrplot(
    decision_cor,
    method = "color",
    type = "upper",
    order = "hclust",
    tl.col = "black",
    tl.srt = 45,
    addCoef.col = "black",
    number.cex = 0.7,
    diag = FALSE,
    title = "Correlation Matrix of Decision Making Variables",
    mar = c(0, 0, 1, 0)
  )
}

# 7.2 Compare key decision variables across clusters
key_vars <- c(
  "Distribution_Centralization", 
  "Distribution_Formalization",
  "Style_Participation", 
  "Style_Organicity",
  "Culture_Symbolic",
  "Flexibility_openness"
)

# Convert to long format
key_vars_long <- cluster_results %>%
  select(ID, HardCluster, all_of(key_vars)) %>%
  pivot_longer(
    cols = all_of(key_vars),
    names_to = "Variable",
    values_to = "Value"
  )

# Create violin plots for key decision variables
violin_plot <- ggplot(key_vars_long, 
                      aes(x = factor(HardCluster), y = Value, fill = factor(HardCluster))) +
  geom_violin(alpha = 0.7) +
  geom_boxplot(width = 0.1, fill = "white", alpha = 0.7) +
  facet_wrap(~ Variable, scales = "free_y") +
  labs(title = "Key Decision Variables by Cluster",
       x = "Cluster",
       y = "Value",
       fill = "Cluster") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set1")

# 8. Membership Uncertainty Analysis --------------------------------------

# 8.1 Calculate membership uncertainty (entropy)
cluster_results <- cluster_results %>%
  mutate(
    Entropy = -(Cluster1 * log(Cluster1) + Cluster2 * log(Cluster2)),
    Uncertainty = Entropy / log(2)  # Normalize by log(k) where k is number of clusters
  )

# Create histogram of uncertainty
uncertainty_hist <- ggplot(cluster_results, aes(x = Uncertainty, fill = factor(HardCluster))) +
  geom_histogram(alpha = 0.7, bins = 20) +
  labs(title = "Membership Uncertainty Distribution",
       subtitle = "Higher values indicate more ambiguous cluster assignment",
       x = "Uncertainty (Normalized Entropy)",
       y = "Count",
       fill = "Dominant Cluster") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set1")

# 8.2 Uncertainty by PDM type
uncertainty_pdm <- ggplot(cluster_results, aes(x = PDM_Type, y = Uncertainty, fill = PDM_Type)) +
  geom_boxplot() +
  labs(title = "Membership Uncertainty by Project Delivery Method",
       subtitle = "Higher values indicate more ambiguous cluster assignment",
       x = "Project Delivery Method",
       y = "Uncertainty (Normalized Entropy)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer(palette = "Set2") +
  guides(fill = "none")

# 9. Save Visualizations to PDF ---------------------------------------------
pdf("BOAT2_Fuzzy_Clustering_Visualizations.pdf", width = 11, height = 8.5)

# 9.1 Membership visualizations
print(membership_plot)
print(membership_scatter)

# 9.2 Cluster profiles
print(heatmap_plot)
create_radar()

# 9.3 PDM analysis
print(pdm_bar)
print(pdm_membership_bar)

# 9.4 Organizational characteristics
print(size_box)
print(structure_box)

# 9.5 Decision variables analysis
create_cor_plot()
print(violin_plot)

# 9.6 Uncertainty analysis
print(uncertainty_hist)
print(uncertainty_pdm)

dev.off()

# 10. Interactive Analysis Dashboard (for RStudio) -------------------------
# These plots can be displayed interactively in RStudio

# Display membership plot
membership_plot

# Display cluster heatmap
heatmap_plot

# Display PDM analysis
pdm_membership_bar

# Display decision variables
violin_plot

# 11. Print Analysis Summary -----------------------------------------------
cat("\n==========================================================\n")
cat("BOAT2 Fuzzy Clustering Visualization Analysis Summary\n")
cat("==========================================================\n\n")

# Cluster sizes
cat("Cluster sizes:\n")
print(table(cluster_results$HardCluster))

# PDM distribution
cat("\nPDM distribution by cluster:\n")
print(pdm_cluster)

# Key differences between clusters
cat("\nKey differences between clusters (standardized centroids):\n")
print(centroids[, c("HardCluster", key_vars)])

# PDM membership summary
cat("\nAverage membership by PDM Type:\n")
print(pdm_membership)

cat("\nVisualization complete. PDF has been saved to 'BOAT2_Fuzzy_Clustering_Visualizations.pdf'\n")
