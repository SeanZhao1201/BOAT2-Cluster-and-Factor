# BOAT2 Cluster and Factor Analysis - Fuzzy Clustering Visualization Module
# This module implements visualization functions for fuzzy c-means clustering results
# Derived from the original FuzzyCVis.R script

# -----------------------------------------------------------------------------
# Function to create membership distribution plot
# -----------------------------------------------------------------------------
plot_membership_distribution <- function(fuzzy_results) {
  # Prepare data for plotting
  membership_long <- fuzzy_results %>%
    select(ID, PDM_Type, starts_with("Cluster"), HardCluster) %>%
    pivot_longer(
      cols = starts_with("Cluster") & !matches("HardCluster"),
      names_to = "Cluster",
      values_to = "Membership"
    )
  
  # Create membership distribution plot
  p <- ggplot(membership_long, 
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
    theme(
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      panel.grid.major.x = element_blank()
    ) +
    scale_fill_brewer(palette = "Set1")
  
  return(p)
}

# -----------------------------------------------------------------------------
# Function to create membership scatterplot
# -----------------------------------------------------------------------------
plot_membership_scatter <- function(fuzzy_results) {
  # Check if we have exactly 2 clusters
  cluster_cols <- grep("^Cluster[0-9]+$", names(fuzzy_results), value = TRUE)
  if (length(cluster_cols) != 2) {
    stop("Membership scatterplot requires exactly 2 clusters")
  }
  
  # Create scatter plot
  p <- ggplot(fuzzy_results, 
              aes_string(x = cluster_cols[1], y = cluster_cols[2], color = "factor(HardCluster)")) +
    geom_point(size = 3, alpha = 0.7) +
    labs(
      title = "Fuzzy Membership Space",
      subtitle = "Each point represents an organization's membership in both clusters",
      x = cluster_cols[1],
      y = cluster_cols[2],
      color = "Dominant Cluster"
    ) +
    theme_minimal() +
    scale_color_brewer(palette = "Set1") +
    geom_abline(intercept = 0.5, slope = -1, linetype = "dashed", color = "gray") +
    annotate("text", x = 0.25, y = 0.75, 
             label = paste("Stronger", cluster_cols[2], "\nMembership"), 
             color = "gray30") +
    annotate("text", x = 0.75, y = 0.25, 
             label = paste("Stronger", cluster_cols[1], "\nMembership"), 
             color = "gray30")
  
  return(p)
}

# -----------------------------------------------------------------------------
# Function to create cluster centroids heatmap
# -----------------------------------------------------------------------------
plot_centroids_heatmap <- function(centroids) {
  # Convert to long format for plotting
  centroids_long <- as.data.frame(centroids) %>%
    rownames_to_column(var = "Variable") %>%
    pivot_longer(
      cols = -Variable,
      names_to = "Cluster",
      values_to = "Value"
    )
  
  # Create heatmap
  p <- ggplot(centroids_long, aes(x = Cluster, y = Variable, fill = Value)) +
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
    theme(
      axis.text.x = element_text(angle = 0),
      axis.text.y = element_text(size = 8)
    )
  
  return(p)
}

# -----------------------------------------------------------------------------
# Function to create radar chart for cluster profiles
# -----------------------------------------------------------------------------
create_radar_chart <- function(radar_data, title = "Fuzzy Cluster Profiles") {
  # Determine number of clusters (excluding max and min rows)
  n_clusters <- nrow(radar_data) - 2
  
  # Create a function to be called in a PDF device
  radar_plot_function <- function() {
    # Set up plotting area
    par(mar = c(1, 1, 3, 1))
    
    # Plot radar chart
    radarchart(
      radar_data,
      pcol = brewer.pal(n = n_clusters, name = "Set1"),
      pfcol = adjustcolor(brewer.pal(n = n_clusters, name = "Set1"), alpha.f = 0.3),
      plwd = 2,
      cglcol = "gray",
      cglty = 1,
      axislabcol = "gray30",
      caxislabels = seq(-3, 3, 1.5),
      title = title
    )
    
    # Add a legend
    legend(
      "bottomright",
      legend = rownames(radar_data)[3:(n_clusters+2)],
      col = brewer.pal(n = n_clusters, name = "Set1"),
      lwd = 2,
      pch = 20,
      bty = "n"
    )
  }
  
  return(radar_plot_function)
}

# -----------------------------------------------------------------------------
# Function to create PDM distribution plot
# -----------------------------------------------------------------------------
plot_pdm_distribution <- function(fuzzy_results) {
  # Calculate PDM by cluster contingency table
  pdm_cluster_table <- table(fuzzy_results$PDM_Type, fuzzy_results$HardCluster)
  
  # Convert to data frame for plotting
  pdm_distribution <- as.data.frame(pdm_cluster_table)
  names(pdm_distribution) <- c("PDM_Type", "Cluster", "Count")
  
  # Create plot
  p <- ggplot(pdm_distribution, 
             aes(x = Cluster, y = Count, fill = PDM_Type)) +
    geom_bar(stat = "identity", position = "fill") +
    geom_text(
      aes(
        label = sprintf("%.1f%%", 
                        prop.table(as.matrix(pdm_cluster_table), 2)[cbind(as.character(PDM_Type), Cluster)] * 100)
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
  
  return(p)
}

# -----------------------------------------------------------------------------
# Function to create PDM membership plot
# -----------------------------------------------------------------------------
plot_pdm_membership <- function(fuzzy_results) {
  # Calculate average membership by PDM type
  pdm_membership <- fuzzy_results %>%
    group_by(PDM_Type) %>%
    summarise(across(starts_with("Cluster") & !matches("HardCluster"), 
                      mean, 
                      .names = "{.col}_Avg")) %>%
    mutate(Count = as.vector(table(fuzzy_results$PDM_Type)))
  
  # Convert to long format for plotting
  pdm_membership_long <- pdm_membership %>%
    pivot_longer(
      cols = ends_with("_Avg"),
      names_to = "Cluster",
      values_to = "Average_Membership"
    ) %>%
    mutate(Cluster = gsub("_Avg", "", Cluster))
  
  # Create plot
  p <- ggplot(pdm_membership_long, 
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
  
  return(p)
}

# -----------------------------------------------------------------------------
# Function to create uncertainty histogram
# -----------------------------------------------------------------------------
plot_uncertainty_histogram <- function(fuzzy_results) {
  # Create histogram
  p <- ggplot(fuzzy_results, aes(x = Uncertainty, fill = factor(HardCluster))) +
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
  
  return(p)
}

# -----------------------------------------------------------------------------
# Function to create uncertainty by PDM type plot
# -----------------------------------------------------------------------------
plot_uncertainty_pdm <- function(fuzzy_results) {
  # Create boxplot
  p <- ggplot(fuzzy_results, aes(x = PDM_Type, y = Uncertainty, fill = PDM_Type)) +
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
  
  return(p)
}

# -----------------------------------------------------------------------------
# Function to create memberships in reduced space plot
# -----------------------------------------------------------------------------
plot_reduced_space_membership <- function(fuzzy_results, dim_data, dim_type = "PCA") {
  # Join fuzzy results with dimensionality reduction results
  if (dim_type == "PCA") {
    x_col <- "PC1"
    y_col <- "PC2"
    title <- "Fuzzy Cluster Memberships in PCA Space"
  } else if (dim_type == "UMAP") {
    x_col <- "UMAP1"
    y_col <- "UMAP2"
    title <- "Fuzzy Cluster Memberships in UMAP Space"
  } else {
    stop("dim_type must be either 'PCA' or 'UMAP'")
  }
  
  # Create plot
  p <- ggplot(fuzzy_results, aes_string(x = x_col, y = y_col)) +
    geom_point(
      aes(
        color = factor(HardCluster), 
        size = apply(fuzzy_results[, grep("^Cluster[0-9]+$", names(fuzzy_results))], 1, max)
      ), 
      alpha = 0.7
    ) +
    scale_color_brewer(palette = "Set1", name = "Dominant Cluster") +
    scale_size_continuous(name = "Membership Strength", range = c(2, 8)) +
    labs(
      title = title,
      subtitle = "Point size indicates strength of dominant membership",
      x = x_col,
      y = y_col
    ) +
    theme_minimal()
  
  return(p)
}

# -----------------------------------------------------------------------------
# Function to create all fuzzy clustering visualizations and save to PDF
# -----------------------------------------------------------------------------
create_fuzzy_visualization_report <- function(fuzzy_results, centroids, radar_data, 
                                             dim_data = NULL, file_path = "fuzzy_clustering_report.pdf") {
  # Start PDF device
  pdf(file_path, width = 12, height = 10)
  
  # Membership visualizations
  print(plot_membership_distribution(fuzzy_results))
  print(plot_membership_scatter(fuzzy_results))
  
  # Cluster profiles
  print(plot_centroids_heatmap(centroids))
  radar_plot <- create_radar_chart(radar_data)
  radar_plot()
  
  # PDM analysis
  print(plot_pdm_distribution(fuzzy_results))
  print(plot_pdm_membership(fuzzy_results))
  
  # Uncertainty analysis
  if ("Uncertainty" %in% names(fuzzy_results)) {
    print(plot_uncertainty_histogram(fuzzy_results))
    print(plot_uncertainty_pdm(fuzzy_results))
  }
  
  # Reduced space visualizations (if dimensionality data provided)
  if (!is.null(dim_data)) {
    fuzzy_dim <- fuzzy_results
    # Add PCA or UMAP columns if they exist in dim_data
    if (all(c("PC1", "PC2") %in% names(dim_data))) {
      fuzzy_dim$PC1 <- dim_data$PC1[match(fuzzy_dim$ID, dim_data$ID)]
      fuzzy_dim$PC2 <- dim_data$PC2[match(fuzzy_dim$ID, dim_data$ID)]
      print(plot_reduced_space_membership(fuzzy_dim, dim_data, "PCA"))
    }
    if (all(c("UMAP1", "UMAP2") %in% names(dim_data))) {
      fuzzy_dim$UMAP1 <- dim_data$UMAP1[match(fuzzy_dim$ID, dim_data$ID)]
      fuzzy_dim$UMAP2 <- dim_data$UMAP2[match(fuzzy_dim$ID, dim_data$ID)]
      print(plot_reduced_space_membership(fuzzy_dim, dim_data, "UMAP"))
    }
  }
  
  # Close PDF device
  dev.off()
  
  cat("Fuzzy clustering visualization report saved to:", file_path, "\n")
} 