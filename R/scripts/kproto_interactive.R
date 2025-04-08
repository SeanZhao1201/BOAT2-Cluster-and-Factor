# BOAT2 Cluster and Factor Analysis - Interactive K-Prototype Clustering
# This script allows interactive K-Prototype clustering with user input for K value

# Load required packages
source("R/00_setup.R")

# Load the prepared dataset
kproto_data <- read.csv("results/tables/kproto_data.csv")

# Define variable groups
kproto_numerical_vars <- c(
  "Org_Structure_Employees",
  "Org_Structure_Locations",
  "Org_Structure_Depts",
  "Org_Structure_Layers"
)

kproto_categorical_vars <- setdiff(
  colnames(kproto_data), 
  c("ID", "PDM_Type", kproto_numerical_vars)
)

# Create mixed dataset
kproto_mixed_data <- kproto_data %>%
  select(all_of(c(kproto_numerical_vars, kproto_categorical_vars)))

# Convert ordinal variables to ordered factors
kproto_mixed_data <- kproto_mixed_data %>%
  mutate(across(all_of(kproto_categorical_vars), ~ ordered(round(.), levels = 1:5)))

# Function to run K-Prototype clustering with user input
run_interactive_kproto <- function() {
  # Get user input for K value
  cat("Please enter the number of clusters (K) you want to use (2-10): ")
  k_value <- as.integer(readline())
  
  # Validate input
  while (!(k_value %in% 2:10)) {
    cat("Invalid input. Please enter a number between 2 and 10: ")
    k_value <- as.integer(readline())
  }
  
  # Set seed for reproducibility
  set.seed(123)
  
  # Run K-Prototype clustering
  cat("\nRunning K-Prototype clustering with K =", k_value, "...\n")
  kproto_result <- clustMixType::kproto(
    kproto_mixed_data, 
    k = k_value,
    verbose = TRUE
  )
  
  # Create results dataframe
  kproto_results <- kproto_data %>%
    mutate(Cluster = kproto_result$cluster)
  
  # Save results
  results_file <- paste0("results/tables/kproto_clusters_k", k_value, ".csv")
  write.csv(kproto_results, results_file, row.names = FALSE)
  cat("\nResults saved to:", results_file, "\n")
  
  # Create centroids dataframe
  kproto_centroids <- data.frame(
    Cluster = 1:k_value,
    kproto_result$centers
  )
  
  # Save centroids
  centroids_file <- paste0("results/tables/kproto_centroids_k", k_value, ".csv")
  write.csv(kproto_centroids, centroids_file, row.names = FALSE)
  cat("Centroids saved to:", centroids_file, "\n")
  
  # Create PDM distribution plot
  pdm_plot <- create_pdm_distribution_plot(
    kproto_results,
    title = paste0("PDM Distribution by K-Prototypes Cluster (k=", k_value, ")")
  )
  
  # Save PDM plot
  pdm_plot_file <- paste0("results/figures/kproto_k", k_value, "_pdm_distribution.pdf")
  ggsave(pdm_plot_file, pdm_plot, width = 10, height = 7)
  cat("PDM distribution plot saved to:", pdm_plot_file, "\n")
  
  # Create cluster characteristics visualization
  all_vars <- c(kproto_numerical_vars, kproto_categorical_vars)
  cluster_viz <- visualize_kprototype_clusters(
    kproto_centroids,
    all_vars,
    title = paste0("K-Prototypes Cluster Characteristics (k=", k_value, ")")
  )
  
  # Save cluster visualization
  cluster_viz_file <- paste0("results/figures/kproto_k", k_value, "_characteristics.pdf")
  ggsave(cluster_viz_file, cluster_viz, width = 12, height = 8)
  cat("Cluster characteristics plot saved to:", cluster_viz_file, "\n")
  
  # Print summary statistics
  cat("\nSummary Statistics:\n")
  cat("Number of observations in each cluster:\n")
  print(table(kproto_results$Cluster))
  
  cat("\nPDM Type distribution by cluster:\n")
  pdm_table <- table(kproto_results$PDM_Type, kproto_results$Cluster)
  print(pdm_table)
  
  cat("\nPDM Type percentage by cluster:\n")
  pdm_percent <- prop.table(pdm_table, margin = 2) * 100
  print(round(pdm_percent, 1))
  
  return(list(
    results = kproto_results,
    centroids = kproto_centroids,
    kproto_object = kproto_result
  ))
}

# Run the interactive analysis
results <- run_interactive_kproto() 