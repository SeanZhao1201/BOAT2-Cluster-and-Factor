# BOAT2 Cluster and Factor Analysis - Run K-Prototype Clustering
# This script runs K-Prototype clustering for K=2 and K=3

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

# Function to run K-Prototype clustering
run_kproto_analysis <- function(k_value) {
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
  
  return(list(
    results = kproto_results,
    centroids = kproto_centroids,
    kproto_object = kproto_result
  ))
}

# Run analysis for K=2 and K=3
k2_results <- run_kproto_analysis(2)
k3_results <- run_kproto_analysis(3)

# Save the results objects for later use
saveRDS(k2_results, "results/rds/kproto_k2_results.rds")
saveRDS(k3_results, "results/rds/kproto_k3_results.rds") 