# BOAT2 Cluster and Factor Analysis - Hierarchical Clustering Module
# This module implements hierarchical clustering methods

#' Perform hierarchical clustering 
#' 
#' @param data Data frame containing the variables to cluster
#' @param variables Names of variables to use for clustering
#' @param distance_method Distance method to use (e.g., "euclidean")
#' @param linkage_method Linkage method to use (e.g., "ward.D2")
#' @param scale_data Whether to scale data before clustering
#' @param k Number of clusters to extract
#' @return List containing clustering results
#' 
run_hierarchical_clustering <- function(data, variables, distance_method = "euclidean", 
                                       linkage_method = "ward.D2", scale_data = TRUE, k = 3) {
  # Extract and prepare the data matrix
  cluster_data <- data %>%
    select(all_of(variables))
  
  # Scale the data if requested
  if (scale_data) {
    cluster_matrix <- scale(cluster_data)
  } else {
    cluster_matrix <- as.matrix(cluster_data)
  }
  
  # Set row names for the matrix
  if ("ID" %in% colnames(data)) {
    rownames(cluster_matrix) <- data$ID
  }
  
  # Compute distance matrix
  dist_matrix <- dist(cluster_matrix, method = distance_method)
  
  # Perform hierarchical clustering
  hc_result <- hclust(dist_matrix, method = linkage_method)
  
  # Cut the tree to get the requested number of clusters
  clusters <- cutree(hc_result, k = k)
  
  # Create results dataframe
  results_df <- data %>%
    mutate(Cluster = clusters)
  
  # Calculate cluster centroids
  centroids_list <- list()
  for (i in 1:k) {
    cluster_i <- results_df[results_df$Cluster == i, ]
    cluster_centroid <- colMeans(cluster_i[, variables, drop = FALSE], na.rm = TRUE)
    centroids_list[[i]] <- cluster_centroid
  }
  
  centroids_df <- do.call(rbind, centroids_list)
  centroids_df <- as.data.frame(centroids_df)
  centroids_df$Cluster <- 1:k
  
  # Return list of results
  return(list(
    results = results_df,
    centroids = centroids_df,
    hc_object = hc_result,
    distance = dist_matrix,
    k = k
  ))
}

#' Determine optimal number of clusters for hierarchical clustering
#' 
#' @param data Data frame containing the variables to analyze
#' @param variables Names of variables to use
#' @param max_k Maximum number of clusters to consider
#' @param scale_data Whether to scale data before clustering
#' @param linkage_method Linkage method to use
#' @return List of results with different optimal k determination methods
#' 
determine_optimal_clusters <- function(data, variables, max_k = 9, 
                                      scale_data = TRUE, linkage_method = "ward.D2") {
  # Prepare data
  cluster_data <- data %>%
    select(all_of(variables))
  
  # Scale the data if requested
  if (scale_data) {
    cluster_matrix <- scale(cluster_data)
  } else {
    cluster_matrix <- as.matrix(cluster_data)
  }
  
  # Compute distance matrix
  dist_matrix <- dist(cluster_matrix, method = "euclidean")
  
  # Initialize results containers
  results <- list()
  
  # 1. Elbow method (using kmeans as approximation)
  wss <- numeric(max_k)
  for (i in 1:max_k) {
    kmeans_result <- kmeans(cluster_matrix, centers = i, nstart = 25)
    wss[i] <- sum(kmeans_result$withinss)
  }
  results$elbow <- data.frame(k = 1:max_k, wss = wss)
  
  # 2. Silhouette method
  silhouette_results <- data.frame(
    k = 2:max_k,
    avg_silhouette = numeric(max_k - 1)
  )
  
  hc_result <- hclust(dist_matrix, method = linkage_method)
  for (k in 2:max_k) {
    clusters <- cutree(hc_result, k = k)
    sil <- silhouette(clusters, dist_matrix)
    silhouette_results$avg_silhouette[k-1] <- mean(sil[, 3])
  }
  results$silhouette <- silhouette_results
  
  # 3. Gap statistic method (if not too computationally intensive)
  if (nrow(cluster_matrix) < 200) {  # Only run for smaller datasets
    gap_stat <- clusGap(
      cluster_matrix, 
      FUN = function(x, k) list(cluster = cutree(hclust(dist(x), method = linkage_method), k = k)),
      K.max = max_k, 
      B = 50  # Reduced for speed, increase for production
    )
    
    results$gap <- data.frame(
      k = 1:max_k,
      gap = gap_stat$Tab[, "gap"],
      se = gap_stat$Tab[, "SE.sim"]
    )
  }
  
  return(results)
}

#' Create a dendrogram visualization from hierarchical clustering
#' 
#' @param hc_result Hierarchical clustering result object
#' @param data Original data frame
#' @param k Number of clusters to highlight
#' @param color_by Variable to color the dendrogram labels by (optional)
#' @param main Title for the plot
#' @param label_size Size of labels
#' @return Dendrogram plot object (invisible)
#' 
create_dendrogram <- function(hc_result, data, k = 3, color_by = NULL, 
                             main = "Hierarchical Clustering Dendrogram", label_size = 0.6) {
  # Create dendrogram object
  dend <- as.dendrogram(hc_result)
  
  # Set up plotting parameters
  par(mar = c(6, 4, 2, 8))  # Adjust margins for better visualization
  
  # Color labels by a variable if specified
  if (!is.null(color_by) && color_by %in% colnames(data)) {
    # Get color values
    color_values <- data[[color_by]]
    
    # Determine appropriate color scheme
    if (is.numeric(color_values)) {
      # For numeric values, use a gradient
      color_palette <- colorRampPalette(c("blue", "red"))(length(unique(color_values)))
      color_mapping <- color_palette[rank(color_values)]
    } else {
      # For categorical values, use distinct colors
      unique_values <- unique(color_values)
      if (length(unique_values) <= 9) {
        color_palette <- brewer.pal(max(3, length(unique_values)), "Set1")
      } else {
        color_palette <- rainbow(length(unique_values))
      }
      names(color_palette) <- unique_values
      color_mapping <- color_palette[color_values]
    }
    
    # Apply colors to dendrogram labels
    labels_colors(dend) <- color_mapping[order.dendrogram(dend)]
    
    # Add a legend for categorical values
    if (!is.numeric(color_values)) {
      legend_needed <- TRUE
    }
  }
  
  # Plot the dendrogram
  plot(
    dend,
    main = main,
    xlab = "",
    ylab = "Height (Dissimilarity)",
    horiz = FALSE,
    cex = label_size
  )
  
  # Add rectangles around clusters
  rect.hclust(hc_result, k = k, border = 2:(k+1))
  
  # Add cluster labels
  clusters <- cutree(hc_result, k = k)
  cluster_centers <- tapply(1:length(clusters), clusters, mean)
  text(
    cluster_centers, 
    par("usr")[4] * 0.95, 
    labels = paste("Cluster", 1:k), 
    cex = 1.2, 
    font = 2,
    col = 2:(k+1)
  )
  
  # Add legend if needed
  if (exists("legend_needed") && !is.null(color_by)) {
    legend(
      "topright",
      legend = names(color_palette),
      fill = color_palette,
      title = color_by,
      cex = 0.8,
      bg = "white"
    )
  }
  
  return(invisible(dend))
}

#' Visualize the characteristics of hierarchical clusters
#' 
#' @param centroids Data frame containing cluster centroids
#' @param variables Variables to include in the visualization
#' @param title Plot title
#' @return ggplot object with cluster characteristics visualization
#' 
visualize_hierarchical_clusters <- function(centroids, variables, title = "Cluster Characteristics") {
  # Reshape data for plotting
  plot_data <- centroids %>%
    select(Cluster, all_of(variables)) %>%
    tidyr::pivot_longer(
      cols = all_of(variables),
      names_to = "Variable",
      values_to = "Value"
    )
  
  # Create a heatmap
  p <- ggplot(plot_data, aes(x = Variable, y = factor(Cluster), fill = Value)) +
    geom_tile() +
    geom_text(aes(label = sprintf("%.2f", Value)), size = 3) +
    scale_fill_viridis_c() +
    labs(
      title = title,
      x = "Variable",
      y = "Cluster",
      fill = "Value"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "right"
    )
  
  return(p)
}

#' Create radar chart for cluster profiles
#' 
#' @param centroids Centroid data frame
#' @param variables Variables to include in the radar chart
#' @param k Number of clusters
#' @param title Plot title
#' @return Radar chart visualization (saved to PDF)
#' 
create_cluster_radar <- function(centroids, variables, k, title = "Cluster Profiles") {
  # Prepare data for radar chart
  radar_data <- centroids %>%
    select(Cluster, all_of(variables)) %>%
    tidyr::pivot_wider(
      names_from = Cluster,
      values_from = all_of(variables)
    )
  
  # Add max and min values required by fmsb package
  radar_data <- as.data.frame(rbind(
    rep(max(centroids[, variables]), k),  # max
    rep(min(centroids[, variables]), k),  # min
    radar_data
  ))
  
  # Get colors for clusters
  colors <- rainbow(k)
  
  # Create radar chart
  radarchart(
    radar_data,
    pcol = colors,
    pfcol = scales::alpha(colors, 0.3),
    plwd = 2,
    cglcol = "grey",
    cglty = 1,
    axislabcol = "grey",
    caxislabels = seq(min(centroids[, variables]), max(centroids[, variables]), length.out = 5),
    cglwd = 0.8,
    title = title
  )
  
  # Add a legend
  legend(
    "topright",
    legend = paste("Cluster", 1:k),
    fill = scales::alpha(colors, 0.3),
    col = colors,
    bty = "n"
  )
}

#' Run complete hierarchical clustering analysis workflow
#' 
#' @param data Input data frame
#' @param variables Names of variables to use for clustering
#' @param k Number of clusters
#' @param color_by Variable to color dendrogram by
#' @param save_results Whether to save results to files
#' @param prefix Prefix for saved files
#' @return List containing all analysis results
#' 
run_hierarchical_analysis <- function(data, variables, k = 3, color_by = "PDM_Type",
                                     save_results = TRUE, prefix = "hierarchical") {
  # Run clustering
  hc_results <- run_hierarchical_clustering(
    data = data,
    variables = variables,
    k = k
  )
  
  # Create PDM distribution plot if PDM_Type exists
  if ("PDM_Type" %in% colnames(data)) {
    pdm_plot <- create_pdm_distribution_plot(
      hc_results$results,
      title = paste0("PDM Distribution by Hierarchical Cluster (k=", k, ")")
    )
  } else {
    pdm_plot <- NULL
  }
  
  # Create cluster characteristics visualization
  cluster_viz <- visualize_hierarchical_clusters(
    hc_results$centroids,
    variables,
    title = paste0("Hierarchical Cluster Characteristics (k=", k, ")")
  )
  
  # Save results if requested
  if (save_results) {
    # Create file names
    results_file <- paste0(prefix, "_clusters_k", k, ".csv")
    centroids_file <- paste0(prefix, "_centroids_k", k, ".csv")
    dendrogram_file <- paste0(prefix, "_dendrogram_k", k, ".pdf")
    
    if (!is.null(pdm_plot)) {
      pdm_plot_file <- paste0(prefix, "_k", k, "_pdm_distribution.pdf")
    }
    
    cluster_viz_file <- paste0(prefix, "_k", k, "_characteristics.pdf")
    
    # Save data files
    write.csv(hc_results$results, file.path("results/tables", results_file), row.names = FALSE)
    write.csv(hc_results$centroids, file.path("results/tables", centroids_file), row.names = FALSE)
    
    # Save dendrogram
    pdf(file.path("results/figures", dendrogram_file), width = 12, height = 8)
    create_dendrogram(hc_results$hc_object, data, k = k, color_by = color_by)
    dev.off()
    
    # Save PDM plot if available
    if (!is.null(pdm_plot)) {
      ggsave(file.path("results/figures", pdm_plot_file), pdm_plot, width = 10, height = 7)
    }
    
    # Save cluster visualization
    ggsave(file.path("results/figures", cluster_viz_file), cluster_viz, width = 12, height = 8)
  }
  
  # Return all results
  return(list(
    clustering = hc_results,
    pdm_plot = pdm_plot,
    cluster_viz = cluster_viz
  ))
} 