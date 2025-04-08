# BOAT2 Cluster and Factor Analysis - K-Prototype Clustering Module
# This module implements k-prototype clustering methods for mixed data types

#' Perform K-Prototypes clustering on mixed data types
#' 
#' @param data Data frame containing the variables to cluster
#' @param numerical_vars Names of numerical variables
#' @param categorical_vars Names of categorical variables (will be converted to ordered factors)
#' @param k Number of clusters
#' @param seed Random seed for reproducibility
#' @return List containing clustering results
#' 
run_kprototype_clustering <- function(data, numerical_vars, categorical_vars, k = 3, seed = 123) {
  # Set seed for reproducibility
  set.seed(seed)
  
  # Create a mixed dataset
  mixed_data <- data %>%
    select(all_of(c(numerical_vars, categorical_vars)))
  
  # Convert ordinal variables to ordered factors
  mixed_data <- mixed_data %>%
    mutate(across(all_of(categorical_vars), ~ ordered(round(.), levels = 1:5)))
  
  # Run k-prototypes
  kproto_result <- clustMixType::kproto(
    mixed_data, 
    k = k,
    verbose = FALSE
  )
  
  # Create results dataframe
  results_df <- data %>%
    mutate(Cluster = kproto_result$cluster)
  
  # Create centroids dataframe
  centroids_df <- data.frame(
    Cluster = 1:k,
    kproto_result$centers
  )
  
  # Return list of results
  return(list(
    results = results_df,
    centroids = centroids_df,
    kproto_object = kproto_result,
    k = k
  ))
}

#' Determine optimal number of clusters for K-Prototypes using silhouette method
#' 
#' @param data Data frame containing the variables to analyze
#' @param numerical_vars Names of numerical variables
#' @param categorical_vars Names of categorical variables
#' @param max_k Maximum number of clusters to consider
#' @return Data frame with silhouette scores for each k value
#' 
get_kprototype_silhouette <- function(data, numerical_vars, categorical_vars, max_k = 9) {
  # Calculate Gower distance (suitable for mixed data types)
  mixed_data <- data %>%
    select(all_of(c(numerical_vars, categorical_vars))) %>%
    mutate(across(all_of(categorical_vars), ~ ordered(round(.), levels = 1:5)))
  
  gower_dist <- daisy(mixed_data, metric = "gower")
  
  # PAM silhouette analysis
  silhouette_results <- data.frame(
    k = 2:max_k,
    avg_silhouette = numeric(max_k - 1)
  )
  
  for (k in 2:max_k) {
    pam_fit <- pam(gower_dist, k = k, diss = TRUE)
    silhouette_results$avg_silhouette[k-1] <- pam_fit$silinfo$avg.width
  }
  
  return(silhouette_results)
}

#' Create a PDM distribution plot for clustering results
#' 
#' @param data Data frame with clustering results and PDM_Type
#' @param k Number of clusters
#' @param title Plot title
#' @return ggplot object with PDM distribution visualization
#' 
create_pdm_distribution_plot <- function(data, title = "PDM Distribution by Cluster") {
  # Create contingency table
  pdm_table <- table(data$PDM_Type, data$Cluster)
  
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

#' Create visualization of cluster characteristics
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

#' Run complete K-Prototype analysis workflow for a specific k value
#' 
#' @param data Input data frame
#' @param numerical_vars Names of numerical variables
#' @param categorical_vars Names of categorical variables
#' @param k Number of clusters
#' @param save_results Whether to save results to files
#' @param prefix Prefix for saved files
#' @return List containing all analysis results
#' 
run_kprototype_analysis <- function(data, numerical_vars, categorical_vars, k, 
                                    save_results = TRUE, prefix = "kproto") {
  # Run clustering
  kproto_results <- run_kprototype_clustering(
    data = data,
    numerical_vars = numerical_vars,
    categorical_vars = categorical_vars,
    k = k
  )
  
  # Create PDM distribution plot
  pdm_plot <- create_pdm_distribution_plot(
    kproto_results$results,
    title = paste0("PDM Distribution by K-Prototypes Cluster (k=", k, ")")
  )
  
  # Create cluster characteristics visualization
  all_vars <- c(numerical_vars, categorical_vars)
  cluster_viz <- visualize_kprototype_clusters(
    kproto_results$centroids,
    all_vars,
    title = paste0("K-Prototypes Cluster Characteristics (k=", k, ")")
  )
  
  # Save results if requested
  if (save_results) {
    # Create file names
    results_file <- paste0(prefix, "_clusters_k", k, ".csv")
    centroids_file <- paste0(prefix, "_centroids_k", k, ".csv")
    pdm_plot_file <- paste0(prefix, "_k", k, "_pdm_distribution.pdf")
    cluster_viz_file <- paste0(prefix, "_k", k, "_characteristics.pdf")
    
    # Save data files
    write.csv(kproto_results$results, file.path("results/tables", results_file), row.names = FALSE)
    write.csv(kproto_results$centroids, file.path("results/tables", centroids_file), row.names = FALSE)
    
    # Save plots
    ggsave(file.path("results/figures", pdm_plot_file), pdm_plot, width = 10, height = 7)
    ggsave(file.path("results/figures", cluster_viz_file), cluster_viz, width = 12, height = 8)
  }
  
  # Return all results
  return(list(
    clustering = kproto_results,
    pdm_plot = pdm_plot,
    cluster_viz = cluster_viz
  ))
} 