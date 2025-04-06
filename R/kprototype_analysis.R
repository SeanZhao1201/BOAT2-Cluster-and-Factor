# BOAT2 Cluster and Factor Analysis - K-Prototype Analysis
# This script runs K-Prototype clustering analysis with different k values

# 1. Setup and Data Loading -----------------------------------------------------
source("R/00_setup.R")

# Load modules
source("R/modules/clustering/kprototype.R")
source("R/modules/visualization.R")
source("R/modules/validation.R")

# Load prepared data
if (file.exists("results/tables/kproto_data.csv")) {
  kproto_data <- read.csv("results/tables/kproto_data.csv")
  cat("Loaded existing K-Prototype data from results/tables/kproto_data.csv\n")
} else {
  # If prepared data doesn't exist, load and prepare it from raw data
  if (file.exists("data/BOAT2_Data.csv")) {
    raw_data <- read.csv("data/BOAT2_Data.csv")
    
    # Add unique ID
    raw_data$ID <- 1:nrow(raw_data)
    
    # Define variable groups
    numerical_org_vars <- c(
      "Org_Structure_Employees",
      "Org_Structure_Locations",
      "Org_Structure_Depts",
      "Org_Structure_Layers"
    )
    
    # Define all ordinal variables
    ordinal_vars <- c(
      "Distribution_Centralization1", "Distribution_Centralization2",
      "Distribution_Formalization1", "Distribution_Formalization2",
      "Style_Technocracy", 
      "Style_Participation1", "Style_Participation2",
      "Style_Organicity1", "Style_Organicity2",
      "Style_Coercion1", "Style_Coercion2",
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
    
    # Prepare data for K-prototype clustering (removing outliers)
    kproto_data <- raw_data %>%
      filter(Org_Structure_Employees != 3700) %>%
      select(ID, PDM_Type, all_of(c(numerical_org_vars, ordinal_vars)))
    
    # Handle missing values
    kproto_data <- kproto_data %>%
      mutate(across(where(is.numeric), ~ifelse(is.na(.), median(., na.rm = TRUE), .)))
    
    # Save prepared data
    write.csv(kproto_data, "results/tables/kproto_data.csv", row.names = FALSE)
    cat("Created and saved K-Prototype data to results/tables/kproto_data.csv\n")
  } else {
    stop("Raw data file not found at data/BOAT2_Data.csv")
  }
}

# 2. Define Analysis Parameters -------------------------------------------------
# Define variable groups
numerical_vars <- c(
  "Org_Structure_Employees",
  "Org_Structure_Locations",
  "Org_Structure_Depts",
  "Org_Structure_Layers"
)

categorical_vars <- setdiff(
  colnames(kproto_data), 
  c("ID", "PDM_Type", numerical_vars)
)

# 3. Determine Optimal Number of Clusters ---------------------------------------
cat("\nDetermining optimal number of clusters...\n")

# Use silhouette method to evaluate different k values
silhouette_results <- get_kprototype_silhouette(
  kproto_data, 
  numerical_vars, 
  categorical_vars,
  max_k = 9
)

# Create silhouette plot
silhouette_plot <- ggplot(silhouette_results, aes(x = k, y = avg_silhouette)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Silhouette Analysis for Optimal K-Prototype Cluster Number",
    x = "Number of Clusters (k)",
    y = "Average Silhouette Width"
  ) +
  theme_minimal()

# Save silhouette plot
ggsave("results/figures/kproto_silhouette_plot.pdf", silhouette_plot, width = 10, height = 7)

# 4. Run K-Prototype Clustering for Selected k Values --------------------------
# We'll run k=2, k=3, and k=7 as specified

# Function to create a wrapper for k-prototype clustering
kproto_wrapper <- function(data, k) {
  # Prepare data for clustering
  mixed_data <- data %>%
    select(all_of(c(numerical_vars, categorical_vars))) %>%
    mutate(across(all_of(categorical_vars), ~ ordered(round(.), levels = 1:5)))
  
  # Run k-prototypes clustering
  kproto_result <- clustMixType::kproto(mixed_data, k = k, verbose = FALSE)
  
  # Return just the cluster assignments
  return(kproto_result$cluster)
}

# Function to run evaluation for a specific k value
evaluate_k <- function(k) {
  cat(paste0("\nRunning K-Prototype clustering with k=", k, "...\n"))
  
  # Run full analysis for this k value
  results <- run_kprototype_analysis(
    kproto_data,
    numerical_vars,
    categorical_vars,
    k = k,
    save_results = TRUE,
    prefix = "kproto"
  )
  
  # Calculate validation metrics on the scaled data
  scaled_data <- scale(kproto_data[, c(numerical_vars, categorical_vars)])
  clusters <- results$clustering$results$Cluster
  
  # Calculate metrics
  sil <- calculate_silhouette(scaled_data, clusters)
  mean_sil <- mean(sil[, 3])
  
  dunn <- calculate_dunn_index(scaled_data, clusters)
  db <- calculate_davies_bouldin(scaled_data, clusters)
  ch <- calculate_calinski_harabasz(scaled_data, clusters)
  
  # Print metrics
  cat(paste0("  Mean Silhouette Width: ", round(mean_sil, 4), "\n"))
  cat(paste0("  Dunn Index: ", round(dunn, 4), "\n"))
  cat(paste0("  Davies-Bouldin Index: ", round(db, 4), "\n"))
  cat(paste0("  Calinski-Harabasz Index: ", round(ch, 4), "\n"))
  
  # Save metrics to file
  metrics_df <- data.frame(
    k = k,
    silhouette = mean_sil,
    dunn = dunn,
    davies_bouldin = db,
    calinski_harabasz = ch
  )
  write.csv(metrics_df, paste0("results/tables/kproto_k", k, "_metrics.csv"), row.names = FALSE)
  
  # Create a comprehensive report
  cat(paste0("Creating comprehensive report for k=", k, "...\n"))
  create_cluster_report(
    results$clustering,
    c(numerical_vars, categorical_vars),
    paste0("K-Prototype Clustering Analysis (k=", k, ")"),
    file = paste0("results/figures/kproto_k", k, "_report.pdf"),
    width = 11,
    height = 8.5
  )
  
  return(results)
}

# Evaluate k=2
results_k2 <- evaluate_k(2)

# Evaluate k=3
results_k3 <- evaluate_k(3)

# Evaluate k=7
results_k7 <- evaluate_k(7)

# 5. Compare Results Across Different k Values ---------------------------------
cat("\nComparing results across different k values...\n")

# Compare clustering quality metrics
metrics_k2 <- read.csv("results/tables/kproto_k2_metrics.csv")
metrics_k3 <- read.csv("results/tables/kproto_k3_metrics.csv")
metrics_k7 <- read.csv("results/tables/kproto_k7_metrics.csv")

comparison_metrics <- rbind(metrics_k2, metrics_k3, metrics_k7)
write.csv(comparison_metrics, "results/tables/kproto_comparison_metrics.csv", row.names = FALSE)

# Create comparison plot for metrics
metrics_long <- comparison_metrics %>%
  pivot_longer(cols = -k, names_to = "Metric", values_to = "Value")

# Scale metrics to 0-1 for easier comparison
metrics_long <- metrics_long %>%
  group_by(Metric) %>%
  mutate(Scaled_Value = if (Metric %in% c("silhouette", "dunn", "calinski_harabasz")) {
    # For these metrics, higher is better
    (Value - min(Value)) / (max(Value) - min(Value))
  } else {
    # For davies_bouldin, lower is better
    1 - (Value - min(Value)) / (max(Value) - min(Value))
  })

# Create comparison plot
comparison_plot <- ggplot(metrics_long, aes(x = factor(k), y = Scaled_Value, color = Metric, group = Metric)) +
  geom_line() +
  geom_point(size = 3) +
  labs(
    title = "Comparison of Clustering Metrics Across k Values",
    subtitle = "All metrics scaled to 0-1 (higher is better)",
    x = "Number of Clusters (k)",
    y = "Scaled Metric Value"
  ) +
  theme_minimal()

ggsave("results/figures/kproto_metrics_comparison.pdf", comparison_plot, width = 10, height = 7)

# 6. Check PDM Distribution Across Cluster Solutions ---------------------------
cat("\nAnalyzing PDM distribution across cluster solutions...\n")

# Load cluster assignments
clusters_k2 <- read.csv("results/tables/kproto_clusters_k2.csv")
clusters_k3 <- read.csv("results/tables/kproto_clusters_k3.csv")
clusters_k7 <- read.csv("results/tables/kproto_clusters_k7.csv")

# Create PDM distribution tables
pdm_table_k2 <- table(clusters_k2$PDM_Type, clusters_k2$Cluster)
pdm_table_k3 <- table(clusters_k3$PDM_Type, clusters_k3$Cluster)
pdm_table_k7 <- table(clusters_k7$PDM_Type, clusters_k7$Cluster)

# Save PDM tables
write.csv(pdm_table_k2, "results/tables/kproto_k2_pdm_table.csv")
write.csv(pdm_table_k3, "results/tables/kproto_k3_pdm_table.csv")
write.csv(pdm_table_k7, "results/tables/kproto_k7_pdm_table.csv")

# 7. Create a Summary Report ---------------------------------------------------
cat("\nCreating summary report...\n")

# Create PDF report comparing the different k values
pdf("results/figures/kproto_comparison_report.pdf", width = 12, height = 9)

# Title page
plot.new()
text(0.5, 0.7, "K-Prototype Clustering Analysis", cex = 2, font = 2)
text(0.5, 0.6, "Comparison of k=2, k=3, and k=7 Solutions", cex = 1.5)
text(0.5, 0.5, paste("Generated on:", Sys.Date()), cex = 1.2)

# Silhouette analysis
print(silhouette_plot)

# Metrics comparison
print(comparison_plot)

# Compare PDM distributions
for (k_val in c(2, 3, 7)) {
  clusters_df <- get(paste0("clusters_k", k_val))
  pdm_plot <- create_pdm_distribution_plot(
    clusters_df,
    title = paste0("PDM Distribution by K-Prototype Cluster (k=", k_val, ")")
  )
  print(pdm_plot)
}

# Close the PDF
dev.off()

# Print completion message
cat("\nK-Prototype analysis complete!\n")
cat("Results have been saved to the results directory.\n")
cat("Main output files:\n")
cat("- Clustering results: kproto_clusters_k2.csv, kproto_clusters_k3.csv, kproto_clusters_k7.csv\n")
cat("- Metrics: kproto_comparison_metrics.csv\n")
cat("- Reports: kproto_k2_report.pdf, kproto_k3_report.pdf, kproto_k7_report.pdf\n")
cat("- Summary: kproto_comparison_report.pdf\n") 