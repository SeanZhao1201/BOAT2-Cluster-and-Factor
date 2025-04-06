# BOAT2 Cluster and Factor Analysis - Dimensionality Reduction
# This script performs PCA and UMAP dimensionality reduction for visualization and analysis

# 1. Load Setup and Data -----------------------------------------------------
source("R/00_setup.R")

# Load the prepared datasets
fuzzy_data <- read.csv("results/tables/fuzzy_data.csv")

# 2. Principal Component Analysis (PCA) --------------------------------------

# 2.1 Prepare data for PCA
# Extract just the decision variables
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

# Extract and scale data for PCA
pca_data <- scale(fuzzy_data[, decision_vars])
rownames(pca_data) <- fuzzy_data$ID

# 2.2 Run PCA
pca_result <- prcomp(pca_data, center = TRUE, scale. = TRUE)

# 2.3 Extract and save results
# Get variance explained by each component
pca_var <- pca_result$sdev^2
pca_var_percent <- round(100 * pca_var / sum(pca_var), 2)

# Create a dataframe of variance explained
pca_var_df <- data.frame(
  PC = paste0("PC", 1:length(pca_var)),
  Variance = pca_var,
  Percentage = pca_var_percent,
  Cumulative = cumsum(pca_var_percent)
)

# Save variance table
save_data(pca_var_df, "pca_variance.csv")

# Extract component scores
pca_scores <- as.data.frame(pca_result$x)
pca_scores$ID <- fuzzy_data$ID
pca_scores$PDM_Type <- fuzzy_data$PDM_Type

# Save PCA scores
save_data(pca_scores, "pca_scores.csv")

# Extract variable loadings
pca_loadings <- as.data.frame(pca_result$rotation)
pca_loadings$Variable <- rownames(pca_loadings)

# Save loadings
save_data(pca_loadings, "pca_loadings.csv")

# 2.4 PCA Visualizations

# Scree plot
scree_plot <- ggplot(pca_var_df, aes(x = PC, y = Percentage)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_line(aes(y = Cumulative, group = 1), color = "red") +
  geom_point(aes(y = Cumulative), color = "red", size = 3) +
  labs(
    title = "PCA Scree Plot",
    subtitle = "Percentage of variance explained by each principal component",
    x = "Principal Component",
    y = "Percentage of Variance Explained"
  ) +
  scale_y_continuous(
    sec.axis = sec_axis(~., name = "Cumulative Percentage")
  ) +
  geom_hline(yintercept = 70, linetype = "dashed", color = "darkgray") +
  theme_minimal()

# Save scree plot
save_plot(scree_plot, "pca_scree_plot.pdf", width = 10, height = 7)

# Biplot of PC1 and PC2
biplot_pc1_pc2 <- ggplot(pca_scores, aes(x = PC1, y = PC2, color = PDM_Type)) +
  geom_point(size = 3, alpha = 0.7) +
  labs(
    title = "PCA Biplot: PC1 vs PC2",
    subtitle = paste0("PC1 (", pca_var_percent[1], "%) vs PC2 (", pca_var_percent[2], "%)"),
    x = paste0("PC1 (", pca_var_percent[1], "% variance)"),
    y = paste0("PC2 (", pca_var_percent[2], "% variance)"),
    color = "PDM Type"
  ) +
  theme_minimal() +
  # Add variable loadings as arrows
  geom_segment(
    data = pca_loadings,
    aes(x = 0, y = 0, xend = PC1 * 5, yend = PC2 * 5),
    arrow = arrow(length = unit(0.2, "cm")),
    color = "darkgray"
  ) +
  geom_text_repel(
    data = pca_loadings,
    aes(x = PC1 * 6, y = PC2 * 6, label = Variable),
    color = "black",
    size = 3,
    segment.alpha = 0.5
  )

# Save biplot
save_plot(biplot_pc1_pc2, "pca_biplot_pc1_pc2.pdf", width = 12, height = 10)

# Biplot of PC3 and PC4 (if they explain a significant amount of variance)
if (sum(pca_var_percent[3:4]) > 10) {
  biplot_pc3_pc4 <- ggplot(pca_scores, aes(x = PC3, y = PC4, color = PDM_Type)) +
    geom_point(size = 3, alpha = 0.7) +
    labs(
      title = "PCA Biplot: PC3 vs PC4",
      subtitle = paste0("PC3 (", pca_var_percent[3], "%) vs PC4 (", pca_var_percent[4], "%)"),
      x = paste0("PC3 (", pca_var_percent[3], "% variance)"),
      y = paste0("PC4 (", pca_var_percent[4], "% variance)"),
      color = "PDM Type"
    ) +
    theme_minimal() +
    # Add variable loadings as arrows
    geom_segment(
      data = pca_loadings,
      aes(x = 0, y = 0, xend = PC3 * 5, yend = PC4 * 5),
      arrow = arrow(length = unit(0.2, "cm")),
      color = "darkgray"
    ) +
    geom_text_repel(
      data = pca_loadings,
      aes(x = PC3 * 6, y = PC4 * 6, label = Variable),
      color = "black",
      size = 3,
      segment.alpha = 0.5
    )
  
  # Save biplot
  save_plot(biplot_pc3_pc4, "pca_biplot_pc3_pc4.pdf", width = 12, height = 10)
}

# 3D PCA Plot (using plotly)
if (requireNamespace("plotly", quietly = TRUE)) {
  pca_3d <- plotly::plot_ly(
    pca_scores,
    x = ~PC1, y = ~PC2, z = ~PC3,
    color = ~PDM_Type,
    type = "scatter3d",
    mode = "markers",
    marker = list(size = 5)
  ) %>%
    plotly::layout(
      title = "3D PCA Plot: PC1, PC2, and PC3",
      scene = list(
        xaxis = list(title = paste0("PC1 (", pca_var_percent[1], "%)")),
        yaxis = list(title = paste0("PC2 (", pca_var_percent[2], "%)")),
        zaxis = list(title = paste0("PC3 (", pca_var_percent[3], "%)"))
      )
    )
  
  # Save as HTML file
  htmlwidgets::saveWidget(pca_3d, "results/figures/pca_3d_plot.html")
}

# 3. UMAP Dimensionality Reduction -------------------------------------------

# 3.1 Run UMAP
# Set UMAP parameters
umap_n_neighbors <- 15     # Controls local versus global structure
umap_min_dist <- 0.1       # Controls clustering tightness
umap_n_components <- 2     # Number of dimensions to reduce to

# Run UMAP
set.seed(123)  # For reproducibility
umap_result <- umap::umap(
  pca_data,
  n_neighbors = umap_n_neighbors,
  min_dist = umap_min_dist,
  n_components = umap_n_components
)

# Create dataframe with UMAP results
umap_df <- as.data.frame(umap_result$layout)
colnames(umap_df) <- c("UMAP1", "UMAP2")
umap_df$ID <- fuzzy_data$ID
umap_df$PDM_Type <- fuzzy_data$PDM_Type

# Save UMAP results
save_data(umap_df, "umap_results.csv")

# 3.2 UMAP Visualization
umap_plot <- ggplot(umap_df, aes(x = UMAP1, y = UMAP2, color = PDM_Type)) +
  geom_point(size = 3, alpha = 0.7) +
  labs(
    title = "UMAP Dimensionality Reduction",
    subtitle = paste0("n_neighbors = ", umap_n_neighbors, ", min_dist = ", umap_min_dist),
    x = "UMAP Dimension 1",
    y = "UMAP Dimension 2",
    color = "PDM Type"
  ) +
  theme_minimal()

# Save UMAP plot
save_plot(umap_plot, "umap_plot.pdf", width = 10, height = 8)

# 4. Combined PCA and UMAP Analysis ------------------------------------------

# 4.1 Identify important variables for each PC
# Get top contributing variables for each PC
get_top_vars <- function(loadings, pc, n = 5) {
  pc_loadings <- loadings[, pc]
  top_positive <- names(sort(pc_loadings, decreasing = TRUE)[1:n])
  top_negative <- names(sort(pc_loadings)[1:n])
  return(list(positive = top_positive, negative = top_negative))
}

# Get top variables for first 3 PCs
top_vars_pc1 <- get_top_vars(pca_result$rotation, "PC1")
top_vars_pc2 <- get_top_vars(pca_result$rotation, "PC2")
top_vars_pc3 <- get_top_vars(pca_result$rotation, "PC3")

# Create a table of top variables
top_vars_table <- data.frame(
  PC = rep(c("PC1", "PC2", "PC3"), each = 10),
  Direction = rep(c("Positive", "Negative"), each = 5, times = 3),
  Variable = c(
    top_vars_pc1$positive, top_vars_pc1$negative,
    top_vars_pc2$positive, top_vars_pc2$negative,
    top_vars_pc3$positive, top_vars_pc3$negative
  ),
  Loading = c(
    pca_result$rotation[top_vars_pc1$positive, "PC1"],
    pca_result$rotation[top_vars_pc1$negative, "PC1"],
    pca_result$rotation[top_vars_pc2$positive, "PC2"],
    pca_result$rotation[top_vars_pc2$negative, "PC2"],
    pca_result$rotation[top_vars_pc3$positive, "PC3"],
    pca_result$rotation[top_vars_pc3$negative, "PC3"]
  )
)

# Save table of top variables
save_data(top_vars_table, "pca_top_variables.csv")

# 4.2 Combine PCA and UMAP results for full analysis
# Create combined dataset with original variables, PCA scores, and UMAP coordinates
combined_df <- fuzzy_data
combined_df$PC1 <- pca_scores$PC1
combined_df$PC2 <- pca_scores$PC2
combined_df$PC3 <- pca_scores$PC3
combined_df$UMAP1 <- umap_df$UMAP1
combined_df$UMAP2 <- umap_df$UMAP2

# Save combined results
save_data(combined_df, "dimensionality_reduction_results.csv")

# 5. PDM Analysis with Dimensionality Reduction ------------------------------

# 5.1 PDM clustering tendency in reduced dimensions
# Calculate centroids by PDM type in PCA space
pdm_centroids_pca <- combined_df %>%
  group_by(PDM_Type) %>%
  summarise(
    PC1_mean = mean(PC1),
    PC2_mean = mean(PC2),
    PC3_mean = mean(PC3),
    UMAP1_mean = mean(UMAP1),
    UMAP2_mean = mean(UMAP2)
  )

# Save PDM centroids
save_data(pdm_centroids_pca, "pdm_centroids_reduced_dimensions.csv")

# Create PDM centroid plot in PCA space
pdm_pca_plot <- ggplot() +
  geom_point(data = combined_df, aes(x = PC1, y = PC2, color = PDM_Type), alpha = 0.5) +
  geom_point(data = pdm_centroids_pca, aes(x = PC1_mean, y = PC2_mean, color = PDM_Type), size = 5) +
  geom_text_repel(data = pdm_centroids_pca, aes(x = PC1_mean, y = PC2_mean, label = PDM_Type, color = PDM_Type)) +
  labs(
    title = "PDM Types in PCA Space",
    subtitle = "Large points represent centroids of each PDM type",
    x = paste0("PC1 (", pca_var_percent[1], "% variance)"),
    y = paste0("PC2 (", pca_var_percent[2], "% variance)"),
    color = "PDM Type"
  ) +
  theme_minimal()

# Save PDM PCA plot
save_plot(pdm_pca_plot, "pdm_pca_plot.pdf", width = 10, height = 8)

# Create PDM centroid plot in UMAP space
pdm_umap_plot <- ggplot() +
  geom_point(data = combined_df, aes(x = UMAP1, y = UMAP2, color = PDM_Type), alpha = 0.5) +
  geom_point(data = pdm_centroids_pca, aes(x = UMAP1_mean, y = UMAP2_mean, color = PDM_Type), size = 5) +
  geom_text_repel(data = pdm_centroids_pca, aes(x = UMAP1_mean, y = UMAP2_mean, label = PDM_Type, color = PDM_Type)) +
  labs(
    title = "PDM Types in UMAP Space",
    subtitle = "Large points represent centroids of each PDM type",
    x = "UMAP Dimension 1",
    y = "UMAP Dimension 2",
    color = "PDM Type"
  ) +
  theme_minimal()

# Save PDM UMAP plot
save_plot(pdm_umap_plot, "pdm_umap_plot.pdf", width = 10, height = 8)

# 6. Create Summary PDF -----------------------------------------------------

# Generate a summary PDF combining key visualizations
pdf("results/figures/dimensionality_reduction_analysis.pdf", width = 12, height = 10)

# PCA analysis
print(scree_plot)
print(biplot_pc1_pc2)
if (exists("biplot_pc3_pc4")) print(biplot_pc3_pc4)

# UMAP analysis
print(umap_plot)

# PDM analysis
print(pdm_pca_plot)
print(pdm_umap_plot)

dev.off()

# Print completion message
cat("\nDimensionality Reduction Analysis complete!\n")
cat("Visualizations saved to results/figures/\n")
cat("Results data saved to results/tables/\n") 