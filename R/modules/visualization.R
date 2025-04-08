# BOAT2 Cluster and Factor Analysis - Common Visualization Module
# This module implements visualization functions used across different clustering methods

#' Create a PDM distribution plot for clustering results
#' 
#' @param data Data frame with clustering results and PDM_Type
#' @param title Plot title
#' @return ggplot object with PDM distribution visualization
#' 
create_pdm_distribution_plot <- function(data, title = "PDM Distribution by Cluster") {
  if (!"PDM_Type" %in% colnames(data) || !"Cluster" %in% colnames(data)) {
    stop("Data frame must contain 'PDM_Type' and 'Cluster' columns")
  }
  
  # Create contingency table
  pdm_table <- table(data$PDM_Type, data$Cluster)
  
  # Convert to data frame for ggplot
  pdm_df <- as.data.frame(pdm_table)
  colnames(pdm_df) <- c("PDM_Type", "Cluster", "Count")
  pdm_df$Cluster <- paste("Cluster", pdm_df$Cluster)
  
  # Calculate percentages
  pdm_df <- pdm_df %>%
    group_by(Cluster) %>%
    mutate(Percentage = Count / sum(Count) * 100)
  
  # Create plot
  p <- ggplot(pdm_df, aes(x = Cluster, y = Count, fill = PDM_Type)) +
    geom_bar(stat = "identity", position = "fill") +
    geom_text(
      aes(
        label = sprintf("%.1f%%", Percentage)
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
    scale_y_continuous(labels = scales::percent) +
    scale_fill_brewer(palette = "Set3")
  
  return(p)
}

#' Create cluster characteristics visualization for mixed data types
#' 
#' @param centroids Data frame containing cluster centroids
#' @param variables Variables to include in the visualization
#' @param title Plot title
#' @return ggplot object with cluster characteristics visualization
#' 
visualize_kprototype_clusters <- function(centroids, variables, title = "Cluster Characteristics") {
  # Separate numerical and categorical variables
  numerical_vars <- variables[sapply(centroids[, variables], is.numeric)]
  categorical_vars <- variables[!variables %in% numerical_vars]
  
  # Create separate plots for numerical and categorical variables
  if (length(numerical_vars) > 0) {
    # Reshape numerical data
    numerical_data <- centroids %>%
      select(Cluster, all_of(numerical_vars)) %>%
      pivot_longer(
        cols = all_of(numerical_vars),
        names_to = "Variable",
        values_to = "Value"
      )
    
    # Create numerical variables plot
    numerical_plot <- ggplot(numerical_data, aes(x = Variable, y = factor(Cluster), fill = Value)) +
      geom_tile() +
      geom_text(aes(label = sprintf("%.2f", Value)), size = 3) +
      scale_fill_viridis_c() +
      labs(
        title = paste(title, "- Numerical Variables"),
        x = "Variable",
        y = "Cluster",
        fill = "Value"
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "right"
      )
  } else {
    numerical_plot <- NULL
  }
  
  if (length(categorical_vars) > 0) {
    # Reshape categorical data
    categorical_data <- centroids %>%
      select(Cluster, all_of(categorical_vars)) %>%
      pivot_longer(
        cols = all_of(categorical_vars),
        names_to = "Variable",
        values_to = "Value"
      )
    
    # Create categorical variables plot
    categorical_plot <- ggplot(categorical_data, aes(x = Variable, y = factor(Cluster), fill = Value)) +
      geom_tile() +
      geom_text(aes(label = as.character(Value)), size = 3) +
      scale_fill_viridis_d() +
      labs(
        title = paste(title, "- Categorical Variables"),
        x = "Variable",
        y = "Cluster",
        fill = "Value"
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "right"
      )
  } else {
    categorical_plot <- NULL
  }
  
  # Combine plots if both exist
  if (!is.null(numerical_plot) && !is.null(categorical_plot)) {
    return(grid.arrange(numerical_plot, categorical_plot, ncol = 2))
  } else if (!is.null(numerical_plot)) {
    return(numerical_plot)
  } else if (!is.null(categorical_plot)) {
    return(categorical_plot)
  } else {
    stop("No variables to plot")
  }
}

#' Create boxplots showing variable distribution by cluster
#' 
#' @param data Data frame containing the clustering results
#' @param variables Variables to create boxplots for
#' @param cluster_col Name of the column containing cluster assignments
#' @param ncol Number of columns in the grid arrangement
#' @param title Main title for the plot
#' @return ggplot object with boxplots arranged in a grid
#' 
create_cluster_boxplots <- function(data, variables, cluster_col = "Cluster", 
                                  ncol = 3, title = "Variable Distribution by Cluster") {
  # Create an empty list to hold individual plots
  plot_list <- list()
  
  # Create a boxplot for each variable
  for (i in seq_along(variables)) {
    var <- variables[i]
    
    # Create boxplot
    p <- ggplot(data, aes_string(x = paste0("factor(", cluster_col, ")"), y = var, fill = paste0("factor(", cluster_col, ")"))) +
      geom_boxplot(alpha = 0.7) +
      stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "black") +
      labs(
        title = var,
        x = "Cluster",
        y = "Value"
      ) +
      theme_minimal() +
      theme(legend.position = "none")
    
    plot_list[[i]] <- p
  }
  
  # Arrange plots in a grid
  grid_plot <- gridExtra::arrangeGrob(
    grobs = plot_list,
    ncol = ncol,
    top = grid::textGrob(title, gp = grid::gpar(fontsize = 14, font = 2))
  )
  
  return(grid_plot)
}

#' Create radar chart for cluster profiles
#' 
#' @param centroids Centroid data frame
#' @param variables Variables to include in the radar chart
#' @param cluster_col Name of the column containing cluster assignments
#' @param title Plot title
#' @param max_val Maximum value for radar chart scaling (optional)
#' @param min_val Minimum value for radar chart scaling (optional)
#' @return Radar chart visualization
#' 
create_radar_chart <- function(centroids, variables, cluster_col = "Cluster", 
                             title = "Cluster Profiles", max_val = NULL, min_val = NULL) {
  # Get number of clusters
  k <- length(unique(centroids[[cluster_col]]))
  
  # Determine max and min values if not provided
  if (is.null(max_val)) {
    max_val <- max(centroids[, variables], na.rm = TRUE)
  }
  if (is.null(min_val)) {
    min_val <- min(centroids[, variables], na.rm = TRUE)
  }
  
  # Prepare data for radar chart
  radar_data <- centroids %>%
    select(!!sym(cluster_col), all_of(variables)) %>%
    tidyr::pivot_wider(
      names_from = !!sym(cluster_col),
      values_from = all_of(variables)
    )
  
  # Convert to format required by fmsb
  radar_matrix <- as.matrix(radar_data[, -1])  # Remove variable name column
  rownames(radar_matrix) <- variables
  radar_df <- as.data.frame(t(radar_matrix))  # Transpose
  
  # Add max and min rows required by fmsb
  radar_df <- rbind(
    rep(max_val, ncol(radar_df)),  # max values
    rep(min_val, ncol(radar_df)),  # min values
    radar_df
  )
  
  # Set row names for max and min
  rownames(radar_df)[1:2] <- c("max", "min")
  
  # Get colors for clusters
  colors <- rainbow(k)
  
  # Create radar chart
  fmsb::radarchart(
    radar_df,
    pcol = colors,
    pfcol = scales::alpha(colors, 0.3),
    plwd = 2,
    cglcol = "grey",
    cglty = 1,
    axislabcol = "grey",
    caxislabels = seq(min_val, max_val, length.out = 5),
    cglwd = 0.8,
    title = title,
    vlcex = 0.8
  )
  
  # Add a legend
  legend(
    "topright",
    legend = paste("Cluster", 1:k),
    fill = scales::alpha(colors, 0.3),
    col = colors,
    bty = "n"
  )
  
  # Return invisibly
  return(invisible(radar_df))
}

#' Create a PCA biplot with cluster coloring
#' 
#' @param data Data frame containing the variables for PCA
#' @param variables Variables to include in PCA
#' @param cluster_col Name of the column containing cluster assignments
#' @param title Plot title
#' @param label_points Whether to label points with IDs
#' @return ggplot object with PCA biplot
#' 
create_pca_biplot <- function(data, variables, cluster_col = "Cluster", 
                            title = "PCA Biplot with Cluster Assignments", label_points = FALSE) {
  # Perform PCA
  pca_result <- prcomp(data[, variables], scale. = TRUE)
  
  # Create data frame for plotting
  pca_data <- as.data.frame(pca_result$x[, 1:2])
  pca_data$Cluster <- factor(data[[cluster_col]])
  
  if ("ID" %in% colnames(data)) {
    pca_data$ID <- data$ID
  }
  
  # Calculate variance explained
  var_explained <- pca_result$sdev^2 / sum(pca_result$sdev^2) * 100
  
  # Create biplot
  p <- ggplot(pca_data, aes(x = PC1, y = PC2, color = Cluster)) +
    geom_point(size = 3, alpha = 0.7) +
    labs(
      title = title,
      x = sprintf("PC1 (%.1f%%)", var_explained[1]),
      y = sprintf("PC2 (%.1f%%)", var_explained[2])
    ) +
    theme_minimal() +
    stat_ellipse(level = 0.95, linetype = 2)
  
  # Add variable arrows
  loadings <- as.data.frame(pca_result$rotation[, 1:2])
  loadings$Variable <- rownames(loadings)
  
  # Scale loadings for better visualization
  scaling_factor <- max(sqrt(pca_data$PC1^2 + pca_data$PC2^2)) / 
                   max(sqrt(loadings$PC1^2 + loadings$PC2^2)) * 0.8
  
  p <- p +
    geom_segment(
      data = loadings,
      aes(x = 0, y = 0, xend = PC1 * scaling_factor, yend = PC2 * scaling_factor),
      arrow = arrow(length = unit(0.2, "cm")),
      color = "darkred"
    ) +
    geom_text_repel(
      data = loadings,
      aes(x = PC1 * scaling_factor, y = PC2 * scaling_factor, label = Variable),
      color = "darkred",
      size = 3
    )
  
  # Add point labels if requested
  if (label_points && "ID" %in% colnames(pca_data)) {
    p <- p + 
      geom_text_repel(
        aes(label = ID),
        size = 2.5,
        max.overlaps = 10
      )
  }
  
  return(p)
}

#' Create UMAP projection with cluster coloring
#' 
#' @param data Data frame containing the variables for UMAP
#' @param variables Variables to include in UMAP
#' @param cluster_col Name of the column containing cluster assignments
#' @param title Plot title
#' @param n_neighbors Number of neighbors for UMAP
#' @param min_dist Minimum distance for UMAP
#' @return ggplot object with UMAP projection
#' 
create_umap_projection <- function(data, variables, cluster_col = "Cluster", 
                                 title = "UMAP Projection with Cluster Assignments", 
                                 n_neighbors = 15, min_dist = 0.1) {
  # Prepare data for UMAP
  umap_data <- data[, variables]
  
  # Scale data
  umap_data_scaled <- scale(umap_data)
  
  # Run UMAP
  set.seed(123)  # For reproducibility
  umap_result <- umap::umap(
    umap_data_scaled,
    n_neighbors = n_neighbors,
    min_dist = min_dist,
    n_components = 2
  )
  
  # Create data frame for plotting
  umap_df <- as.data.frame(umap_result$layout)
  colnames(umap_df) <- c("UMAP1", "UMAP2")
  umap_df$Cluster <- factor(data[[cluster_col]])
  
  if ("ID" %in% colnames(data)) {
    umap_df$ID <- data$ID
  }
  
  # Create UMAP plot
  p <- ggplot(umap_df, aes(x = UMAP1, y = UMAP2, color = Cluster)) +
    geom_point(size = 3, alpha = 0.7) +
    labs(
      title = title,
      x = "UMAP1",
      y = "UMAP2"
    ) +
    theme_minimal() +
    stat_ellipse(level = 0.95, linetype = 2)
  
  return(p)
}

#' Create a comprehensive cluster report
#' 
#' @param cluster_results Clustering results list
#' @param variables Variables used for clustering
#' @param title Main title for the report
#' @param file Output PDF file path
#' @param width PDF width in inches
#' @param height PDF height in inches
#' @return Invisibly returns NULL
#' 
create_cluster_report <- function(cluster_results, variables, title, 
                                file = NULL, width = 11, height = 8.5) {
  # Extract components
  data <- cluster_results$results
  centroids <- cluster_results$centroids
  k <- length(unique(data$Cluster))
  
  # Create PDF if file is provided
  if (!is.null(file)) {
    pdf(file, width = width, height = height)
  }
  
  # Set up multi-page plot layout
  par(mfrow = c(1, 1))
  
  # Title page
  plot.new()
  text(0.5, 0.6, title, cex = 2, font = 2)
  text(0.5, 0.5, paste("Number of clusters:", k), cex = 1.5)
  text(0.5, 0.4, paste("Generated on:", Sys.Date()), cex = 1.2)
  
  # PDM distribution if available
  if ("PDM_Type" %in% colnames(data)) {
    pdm_plot <- create_pdm_distribution_plot(
      data,
      title = paste("PDM Distribution by Cluster (k =", k, ")")
    )
    print(pdm_plot)
  }
  
  # Cluster characteristics heatmap
  heatmap <- create_cluster_heatmap(
    centroids,
    variables,
    title = paste("Cluster Characteristics (k =", k, ")")
  )
  print(heatmap)
  
  # Boxplots (multiple pages)
  var_groups <- split(variables, ceiling(seq_along(variables) / 9))
  for (var_group in var_groups) {
    boxplots <- create_cluster_boxplots(
      data,
      var_group,
      ncol = 3,
      title = paste("Variable Distribution by Cluster (k =", k, ")")
    )
    grid::grid.newpage()
    grid::grid.draw(boxplots)
  }
  
  # Radar chart
  plot.new()
  create_radar_chart(
    centroids,
    variables,
    title = paste("Cluster Profiles (k =", k, ")")
  )
  
  # PCA biplot
  pca_plot <- create_pca_biplot(
    data,
    variables,
    title = paste("PCA Biplot with Cluster Assignments (k =", k, ")")
  )
  print(pca_plot)
  
  # UMAP projection if not too many points
  if (nrow(data) < 500) {
    umap_plot <- create_umap_projection(
      data,
      variables,
      title = paste("UMAP Projection with Cluster Assignments (k =", k, ")")
    )
    print(umap_plot)
  }
  
  # Close PDF if opened
  if (!is.null(file)) {
    dev.off()
  }
  
  return(invisible(NULL))
} 