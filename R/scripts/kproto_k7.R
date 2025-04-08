# BOAT2 Cluster and Factor Analysis - K-Prototype Clustering with K=7
# This script performs K-Prototype clustering with 7 clusters

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

# Set seed for reproducibility
set.seed(123)

# Run K-Prototype clustering with K=7
cat("Running K-Prototype clustering with K = 7...\n")
kproto_result <- clustMixType::kproto(
  kproto_mixed_data, 
  k = 7,
  verbose = TRUE
)

# Create results dataframe
kproto_results <- kproto_data %>%
  mutate(Cluster = kproto_result$cluster)

# Save results
results_file <- "results/tables/kproto_clusters_k7.csv"
write.csv(kproto_results, results_file, row.names = FALSE)
cat("\nResults saved to:", results_file, "\n")

# Create centroids dataframe
kproto_centroids <- data.frame(
  Cluster = 1:7,
  kproto_result$centers
)

# Save centroids
centroids_file <- "results/tables/kproto_centroids_k7.csv"
write.csv(kproto_centroids, centroids_file, row.names = FALSE)
cat("Centroids saved to:", centroids_file, "\n")

# Create PDM distribution plot
pdm_plot <- create_pdm_distribution_plot(
  kproto_results,
  title = "PDM Distribution by K-Prototypes Cluster (k=7)"
)

# Save PDM plot
pdm_plot_file <- "results/figures/kproto_k7_pdm_distribution.pdf"
ggsave(pdm_plot_file, pdm_plot, width = 10, height = 7)
cat("PDM distribution plot saved to:", pdm_plot_file, "\n")

# Create cluster characteristics visualization
all_vars <- c(kproto_numerical_vars, kproto_categorical_vars)
cluster_viz <- visualize_kprototype_clusters(
  kproto_centroids,
  all_vars,
  title = "K-Prototypes Cluster Characteristics (k=7)"
)

# Save cluster visualization
cluster_viz_file <- "results/figures/kproto_k7_characteristics.pdf"
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

# Save the results object for later use
saveRDS(list(
  results = kproto_results,
  centroids = kproto_centroids,
  kproto_object = kproto_result
), "results/rds/kproto_k7_results.rds") 