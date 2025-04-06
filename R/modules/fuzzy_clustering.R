# BOAT2 Cluster and Factor Analysis - Fuzzy Clustering Module
# This module implements fuzzy c-means clustering functionality
# Derived from the original FuzzyZ.R script

# -----------------------------------------------------------------------------
# Function to perform fuzzy c-means clustering on decision-making profiles
# -----------------------------------------------------------------------------
run_fuzzy_clustering <- function(data, decision_vars, k = 2, memb.exp = 1.5) {
  # Prepare data for clustering
  # Scale the data
  fuzzy_matrix <- as.matrix(scale(data[, decision_vars]))
  rownames(fuzzy_matrix) <- data$ID
  
  # Set random seed for reproducibility
  set.seed(123)
  
  # Run fuzzy c-means clustering
  fcm_result <- cluster::fanny(
    fuzzy_matrix,
    k = k,
    memb.exp = memb.exp,  # Fuzziness parameter (1 = hard clustering, >1 = increasingly fuzzy)
    maxit = 1000,         # Maximum iterations
    metric = "euclidean"
  )
  
  # Extract results
  # Get cluster memberships
  memberships <- fcm_result$membership
  colnames(memberships) <- paste0("Cluster", 1:k)
  
  # Get hard cluster assignments
  hard_clusters <- fcm_result$clustering
  
  # Create results dataframe
  results <- cbind(
    data,
    memberships,
    HardCluster = hard_clusters
  )
  
  # Calculate cluster centroids
  centroids <- t(fcm_result$centroids)
  rownames(centroids) <- decision_vars
  
  # Return results as a list
  return(list(
    results = results,
    fcm_object = fcm_result,
    centroids = centroids,
    k = k
  ))
}

# -----------------------------------------------------------------------------
# Function to determine optimal number of clusters using silhouette method
# -----------------------------------------------------------------------------
get_fcm_silhouette <- function(data, decision_vars, max_k = 8) {
  # Prepare data
  fuzzy_matrix <- as.matrix(scale(data[, decision_vars]))
  
  sil_scores <- numeric(max_k - 1)
  
  for (k in 2:max_k) {
    # Run fuzzy c-means
    fcm_result <- cluster::fanny(fuzzy_matrix, k = k, memb.exp = 1.5, metric = "euclidean")
    
    # Get silhouette information
    sil_scores[k-1] <- mean(silhouette(fcm_result$clustering, dist(fuzzy_matrix))[,3])
  }
  
  return(data.frame(k = 2:max_k, silhouette = sil_scores))
}

# -----------------------------------------------------------------------------
# Function to calculate Dunn index for clustering validation
# -----------------------------------------------------------------------------
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

# -----------------------------------------------------------------------------
# Function to get Dunn index for different k values
# -----------------------------------------------------------------------------
get_fcm_dunn <- function(data, decision_vars, max_k = 8) {
  # Prepare data
  fuzzy_matrix <- as.matrix(scale(data[, decision_vars]))
  
  dunn_scores <- numeric(max_k - 1)
  
  for (k in 2:max_k) {
    # Run fuzzy c-means
    fcm_result <- cluster::fanny(fuzzy_matrix, k = k, memb.exp = 1.5, metric = "euclidean")
    
    # Calculate Dunn index
    dunn_scores[k-1] <- calculate_dunn_index(fuzzy_matrix, fcm_result$clustering)
  }
  
  return(data.frame(k = 2:max_k, dunn = dunn_scores))
}

# -----------------------------------------------------------------------------
# Function to calculate membership uncertainty (entropy)
# -----------------------------------------------------------------------------
calculate_uncertainty <- function(memberships) {
  # Get number of clusters
  k <- ncol(memberships)
  
  # Calculate entropy for each observation
  entropy <- apply(memberships, 1, function(x) {
    # Avoid log(0) issues
    x <- pmax(x, 1e-10)
    -sum(x * log(x))
  })
  
  # Normalize by log(k) to get values between 0 and 1
  uncertainty <- entropy / log(k)
  
  return(uncertainty)
}

# -----------------------------------------------------------------------------
# Function to analyze PDM distribution in fuzzy clusters
# -----------------------------------------------------------------------------
analyze_pdm_distribution <- function(fuzzy_results) {
  # Calculate PDM by cluster contingency table
  pdm_cluster_table <- table(fuzzy_results$PDM_Type, fuzzy_results$HardCluster)
  pdm_cluster_prop <- prop.table(pdm_cluster_table, margin = 2) * 100
  
  # Calculate average membership by PDM type
  pdm_membership <- fuzzy_results %>%
    group_by(PDM_Type) %>%
    summarise(across(starts_with("Cluster"), mean, .names = "{.col}_Avg")) %>%
    mutate(Count = as.vector(table(fuzzy_results$PDM_Type)))
  
  # Return results
  return(list(
    contingency_table = pdm_cluster_table,
    proportions = pdm_cluster_prop,
    avg_membership = pdm_membership
  ))
}

# -----------------------------------------------------------------------------
# Function to prepare radar chart data for cluster visualization
# -----------------------------------------------------------------------------
prepare_radar_data <- function(centroids, max_val = 3, min_val = -3) {
  # Transpose centroids to get the right format
  radar_data <- t(centroids)
  radar_data <- as.data.frame(radar_data)
  
  # Add max and min rows required by fmsb
  radar_data <- rbind(
    rep(max_val, ncol(radar_data)),  # max values
    rep(min_val, ncol(radar_data)),  # min values
    radar_data
  )
  
  # Set row names for max and min
  rownames(radar_data)[1:2] <- c("max", "min")
  
  return(radar_data)
} 