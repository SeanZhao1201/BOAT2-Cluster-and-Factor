# BOAT2 Cluster and Factor Analysis - Cluster Validation Module
# This module implements functions for validating and comparing clustering results

#' Calculate silhouette scores for clustering results
#' 
#' @param data Data frame or matrix containing the input data
#' @param clusters Vector of cluster assignments
#' @param distance Distance matrix or method to use
#' @return Silhouette object with scores
#' 
calculate_silhouette <- function(data, clusters, distance = NULL) {
  # Create distance matrix if not provided
  if (is.null(distance)) {
    if (is.matrix(data) || is.data.frame(data)) {
      distance <- dist(data)
    } else {
      stop("If distance is not provided, data must be a matrix or data frame")
    }
  } else if (is.character(distance)) {
    # If distance is a character string, create distance matrix with that method
    distance <- dist(data, method = distance)
  }
  
  # Calculate silhouette
  sil <- cluster::silhouette(clusters, distance)
  
  return(sil)
}

#' Calculate Dunn index for clustering validation
#' 
#' @param data Data matrix
#' @param clusters Vector of cluster assignments
#' @param distance_matrix Optional pre-computed distance matrix
#' @return Dunn index value
#' 
calculate_dunn_index <- function(data, clusters, distance_matrix = NULL) {
  # Calculate distance matrix if not provided
  if (is.null(distance_matrix)) {
    dist_matrix <- dist(data)
    dist_full <- as.matrix(dist_matrix)
  } else {
    dist_full <- as.matrix(distance_matrix)
  }
  
  # Get unique clusters
  unique_clusters <- unique(clusters)
  n_clusters <- length(unique_clusters)
  
  # Initialize min_inter_cluster_dist and max_intra_cluster_dist
  min_inter_cluster_dist <- Inf
  max_intra_cluster_dist <- -Inf
  
  # Calculate maximum intra-cluster distance for each cluster
  for (i in 1:n_clusters) {
    cluster_i_indices <- which(clusters == unique_clusters[i])
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
      cluster_i_indices <- which(clusters == unique_clusters[i])
      cluster_j_indices <- which(clusters == unique_clusters[j])
      
      inter_dists <- dist_full[cluster_i_indices, cluster_j_indices]
      min_inter_cluster_dist <- min(min_inter_cluster_dist, min(inter_dists))
    }
  }
  
  # Calculate Dunn index
  dunn_index <- min_inter_cluster_dist / max_intra_cluster_dist
  return(dunn_index)
}

#' Calculate Davies-Bouldin index for clustering validation
#' 
#' @param data Data matrix
#' @param clusters Vector of cluster assignments
#' @return Davies-Bouldin index value
#' 
calculate_davies_bouldin <- function(data, clusters) {
  # Get unique clusters
  unique_clusters <- unique(clusters)
  n_clusters <- length(unique_clusters)
  
  # Calculate cluster centroids
  centroids <- matrix(0, nrow = n_clusters, ncol = ncol(data))
  for (i in 1:n_clusters) {
    cluster_i_indices <- which(clusters == unique_clusters[i])
    centroids[i, ] <- colMeans(data[cluster_i_indices, , drop = FALSE])
  }
  
  # Calculate average distance of points to their centroids
  cluster_dispersions <- numeric(n_clusters)
  for (i in 1:n_clusters) {
    cluster_i_indices <- which(clusters == unique_clusters[i])
    if (length(cluster_i_indices) > 0) {
      distances <- sqrt(rowSums((data[cluster_i_indices, , drop = FALSE] - 
                               matrix(centroids[i, ], nrow = length(cluster_i_indices), 
                                     ncol = ncol(data), byrow = TRUE))^2))
      cluster_dispersions[i] <- mean(distances)
    }
  }
  
  # Calculate Davies-Bouldin index
  db_sum <- 0
  for (i in 1:n_clusters) {
    max_ratio <- 0
    for (j in 1:n_clusters) {
      if (i != j) {
        # Calculate distance between centroids
        centroid_dist <- sqrt(sum((centroids[i, ] - centroids[j, ])^2))
        # Calculate ratio
        ratio <- (cluster_dispersions[i] + cluster_dispersions[j]) / centroid_dist
        max_ratio <- max(max_ratio, ratio)
      }
    }
    db_sum <- db_sum + max_ratio
  }
  
  davies_bouldin <- db_sum / n_clusters
  return(davies_bouldin)
}

#' Calculate Calinski-Harabasz index (variance ratio criterion)
#' 
#' @param data Data matrix
#' @param clusters Vector of cluster assignments
#' @return Calinski-Harabasz index value
#' 
calculate_calinski_harabasz <- function(data, clusters) {
  n <- nrow(data)
  unique_clusters <- unique(clusters)
  k <- length(unique_clusters)
  
  # Overall mean
  overall_mean <- colMeans(data)
  
  # Between-cluster sum of squares
  bss <- matrix(0, nrow = 1, ncol = ncol(data))
  # Within-cluster sum of squares
  wss <- matrix(0, nrow = 1, ncol = ncol(data))
  
  # Calculate cluster means and sums of squares
  for (i in 1:k) {
    cluster_i_indices <- which(clusters == unique_clusters[i])
    ni <- length(cluster_i_indices)
    
    if (ni > 0) {
      # Cluster mean
      cluster_mean <- colMeans(data[cluster_i_indices, , drop = FALSE])
      
      # Between-cluster sum of squares contribution
      bss <- bss + ni * (t(cluster_mean - overall_mean) %*% (cluster_mean - overall_mean))
      
      # Within-cluster sum of squares contribution
      for (j in cluster_i_indices) {
        wss <- wss + t(data[j, ] - cluster_mean) %*% (data[j, ] - cluster_mean)
      }
    }
  }
  
  # Calculate Calinski-Harabasz index
  ch_index <- (bss / (k - 1)) / (wss / (n - k))
  return(as.numeric(ch_index))
}

#' Calculate stability of clustering results using subsampling
#' 
#' @param data Data matrix
#' @param cluster_func Function to perform clustering
#' @param n_subsample Number of subsamples to generate
#' @param subsample_fraction Fraction of data to include in each subsample
#' @param ... Additional parameters to pass to cluster_func
#' @return List containing stability metrics
#' 
calculate_cluster_stability <- function(data, cluster_func, n_subsample = 10, 
                                       subsample_fraction = 0.75, ...) {
  n <- nrow(data)
  subsample_size <- floor(n * subsample_fraction)
  
  # Run full data clustering
  full_clusters <- cluster_func(data, ...)
  
  # Initialize matrices for results
  ari_values <- numeric(n_subsample)
  jaccard_values <- numeric(n_subsample)
  
  # Run subsampling
  for (i in 1:n_subsample) {
    # Create subsample
    subsample_indices <- sample(1:n, subsample_size)
    subsample_data <- data[subsample_indices, ]
    
    # Cluster subsample
    subsample_clusters <- cluster_func(subsample_data, ...)
    
    # Create comparison clusters (only for common indices)
    full_subset <- full_clusters[subsample_indices]
    
    # Calculate ARI
    ari_values[i] <- mclust::adjustedRandIndex(full_subset, subsample_clusters)
    
    # Calculate Jaccard similarity
    jaccard_values[i] <- calculate_jaccard_similarity(full_subset, subsample_clusters)
  }
  
  # Return stability metrics
  return(list(
    mean_ari = mean(ari_values),
    sd_ari = sd(ari_values),
    mean_jaccard = mean(jaccard_values),
    sd_jaccard = sd(jaccard_values),
    ari_values = ari_values,
    jaccard_values = jaccard_values
  ))
}

#' Calculate Jaccard similarity between two cluster assignments
#' 
#' @param clusters1 First set of cluster assignments
#' @param clusters2 Second set of cluster assignments
#' @return Jaccard similarity coefficient
#' 
calculate_jaccard_similarity <- function(clusters1, clusters2) {
  n <- length(clusters1)
  
  # Create same-cluster matrices
  same_cluster1 <- matrix(0, n, n)
  same_cluster2 <- matrix(0, n, n)
  
  for (i in 1:(n-1)) {
    for (j in (i+1):n) {
      same_cluster1[i, j] <- same_cluster1[j, i] <- (clusters1[i] == clusters1[j])
      same_cluster2[i, j] <- same_cluster2[j, i] <- (clusters2[i] == clusters2[j])
    }
  }
  
  # Calculate Jaccard similarity
  a <- sum(same_cluster1 & same_cluster2)  # Same cluster in both
  b <- sum(same_cluster1 & !same_cluster2)  # Same in 1, different in 2
  c <- sum(!same_cluster1 & same_cluster2)  # Different in 1, same in 2
  
  jaccard <- a / (a + b + c)
  return(jaccard)
}

#' Evaluate cluster solutions for different k values using multiple metrics
#' 
#' @param data Data matrix
#' @param cluster_func Function to perform clustering with signature function(data, k)
#' @param k_range Range of k values to evaluate
#' @param metrics Vector of metrics to calculate ("silhouette", "dunn", "davies_bouldin", "calinski_harabasz")
#' @param distance_method Distance method to use (passed to dist function)
#' @return Data frame with evaluation metrics for each k
#' 
evaluate_cluster_range <- function(data, cluster_func, k_range = 2:10, 
                                  metrics = c("silhouette", "dunn", "davies_bouldin", "calinski_harabasz"),
                                  distance_method = "euclidean") {
  # Calculate distance matrix
  dist_matrix <- dist(data, method = distance_method)
  
  # Initialize results data frame
  results <- data.frame(k = k_range)
  
  # Add columns for each metric
  for (metric in metrics) {
    results[[metric]] <- NA
  }
  
  # Evaluate each k
  for (i in seq_along(k_range)) {
    k <- k_range[i]
    cat("Evaluating k =", k, "\n")
    
    # Get cluster assignments
    clusters <- cluster_func(data, k)
    
    # Calculate metrics
    if ("silhouette" %in% metrics) {
      sil <- calculate_silhouette(data, clusters, dist_matrix)
      results$silhouette[i] <- mean(sil[, 3])
    }
    
    if ("dunn" %in% metrics) {
      results$dunn[i] <- calculate_dunn_index(data, clusters, dist_matrix)
    }
    
    if ("davies_bouldin" %in% metrics) {
      results$davies_bouldin[i] <- calculate_davies_bouldin(data, clusters)
    }
    
    if ("calinski_harabasz" %in% metrics) {
      results$calinski_harabasz[i] <- calculate_calinski_harabasz(data, clusters)
    }
  }
  
  return(results)
}

#' Compare different clustering methods on the same dataset
#' 
#' @param data Data matrix
#' @param method_list List of functions to perform clustering
#' @param method_names Names of clustering methods
#' @param k Number of clusters to extract
#' @param metrics Vector of metrics to calculate
#' @return Data frame with comparison results
#' 
compare_clustering_methods <- function(data, method_list, method_names, k = 3,
                                      metrics = c("silhouette", "dunn", "davies_bouldin", "calinski_harabasz")) {
  # Initialize results
  n_methods <- length(method_list)
  results <- data.frame(method = method_names)
  
  # Calculate distance matrix once
  dist_matrix <- dist(data)
  
  # Add columns for each metric
  for (metric in metrics) {
    results[[metric]] <- NA
  }
  
  # Evaluate each method
  for (i in 1:n_methods) {
    cat("Evaluating method:", method_names[i], "\n")
    
    # Get cluster assignments
    clusters <- method_list[[i]](data, k)
    
    # Calculate metrics
    if ("silhouette" %in% metrics) {
      sil <- calculate_silhouette(data, clusters, dist_matrix)
      results$silhouette[i] <- mean(sil[, 3])
    }
    
    if ("dunn" %in% metrics) {
      results$dunn[i] <- calculate_dunn_index(data, clusters, dist_matrix)
    }
    
    if ("davies_bouldin" %in% metrics) {
      results$davies_bouldin[i] <- calculate_davies_bouldin(data, clusters)
    }
    
    if ("calinski_harabasz" %in% metrics) {
      results$calinski_harabasz[i] <- calculate_calinski_harabasz(data, clusters)
    }
  }
  
  return(results)
} 