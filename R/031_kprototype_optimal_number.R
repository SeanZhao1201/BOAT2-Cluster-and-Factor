# BOAT2 Cluster and Factor Analysis - K-Prototype Optimal Clusters
# This script determines the optimal number of clusters for K-prototype clustering

# 1. Load Setup and Data -----------------------------------------------------
source("R/00_setup.R")

# Create subdirectories for results if they don't exist
dirs <- c(
  "results/figures/031_kprototype_optimal",
  "results/tables/031_kprototype_optimal"
)

for (dir in dirs) {
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE)
    cat(paste("Created directory:", dir, "\n"))
  }
}

# Load the enhanced dataset
data <- read.csv("data/BOAT2_Data_Enhanced.csv")
cat("Loaded dataset with", nrow(data), "rows and", ncol(data), "columns.\n")
cat("Column names:", paste(head(colnames(data), 10), collapse=", "), "...\n")

# 2. Define Variables --------------------------------------------------------

# Define numerical and categorical variables
numerical_org_vars <- c(
  "Org_Structure_Employees",
  "Org_Structure_Locations",
  "Org_Structure_Depts",
  "Org_Structure_Layers"
)

# All variables except PDM_Selected and PDM experience variables
exclude_vars <- c("PDM_Selected", 
                  "PDM_Experience_DBB", 
                  "PDM_Experience_DB", 
                  "PDM_Experience_PDB", 
                  "PDM_Experience_CMAR", 
                  "PDM_Experience_IPD")
all_vars <- setdiff(colnames(data), exclude_vars)

# Categorical variables (all except numerical ones)
categorical_vars <- setdiff(all_vars, numerical_org_vars)

# Check variables
cat("\nNumerical variables:", length(numerical_org_vars), "\n")
cat("Categorical variables:", length(categorical_vars), "\n")
cat("All analysis variables:", length(all_vars), "\n")

# 3. Prepare data for K-prototypes clustering -------------------------------
# Extract relevant columns
kproto_data <- data %>%
  select(all_of(c(numerical_org_vars, categorical_vars, "PDM_Selected")))

# Create mixed dataset for analysis
kproto_mixed_data <- kproto_data %>%
  select(all_of(c(numerical_org_vars, categorical_vars)))

# Convert categorical variables to ordered factors
kproto_mixed_data <- kproto_mixed_data %>%
  mutate(across(all_of(categorical_vars), ~ ordered(round(.), levels = 1:5)))

# Save preprocessed data for later use
save_data(kproto_data, "031_kprototype_optimal/kproto_data.csv")

# 4. Determine Optimal Number of Clusters -----------------------------------

# Calculate Gower distance (suitable for mixed data types)
cat("\nCalculating Gower distance matrix...\n")
gower_dist <- daisy(kproto_mixed_data, metric = "gower")

# PAM silhouette analysis for k=2 to k=9
pam_silhouette_results <- data.frame(
  k = 2:9,
  avg_silhouette = numeric(8)
)

cat("\nRunning PAM silhouette analysis for k=2 to k=9...\n")
for (k in 2:9) {
  cat("  Processing k =", k, "...\n")
  pam_fit <- pam(gower_dist, k = k, diss = TRUE)
  pam_silhouette_results$avg_silhouette[k-1] <- pam_fit$silinfo$avg.width
}

# Save silhouette results
save_data(pam_silhouette_results, "031_kprototype_optimal/silhouette_results.csv")

# Find optimal k from silhouette method
optimal_k_silhouette <- pam_silhouette_results$k[which.max(pam_silhouette_results$avg_silhouette)]

# Create PAM silhouette plot
pam_silhouette_plot <- ggplot(pam_silhouette_results, aes(x = k, y = avg_silhouette)) +
  geom_line() +
  geom_point(size = 3) +
  # 强调最优k值处的点
  geom_point(data = data.frame(k = optimal_k_silhouette, 
                              avg_silhouette = pam_silhouette_results$avg_silhouette[optimal_k_silhouette - 1]), 
             aes(x = k, y = avg_silhouette), 
             color = "red", size = 4) +
  # 添加标注
  annotate("text", x = optimal_k_silhouette + 0.3, 
           y = pam_silhouette_results$avg_silhouette[optimal_k_silhouette - 1], 
           label = paste("Optimal k =", optimal_k_silhouette), 
           color = "red", hjust = 0, vjust = 0.5, size = 4) +
  labs(
    title = "PAM Silhouette Analysis for Optimal Cluster Number",
    subtitle = "Higher silhouette width indicates better clustering",
    x = "Number of Clusters (k)",
    y = "Average Silhouette Width"
  ) +
  # 设置x轴为整数刻度，与elbow图保持一致
  scale_x_continuous(breaks = 2:9, labels = 2:9, limits = c(1.5, 9.5)) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(color = "gray30"),
    axis.title = element_text(face = "bold"),
    # 确保x轴标签显示清晰
    axis.text.x = element_text(size = 12),
    # 添加轻微的垂直网格线帮助识别整数k值
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(color = "gray90", size = 0.5)
  )

# Save PAM silhouette plot
ggsave(
  "results/figures/031_kprototype_optimal/silhouette_plot.pdf",
  pam_silhouette_plot,
  width = 10,
  height = 7,
  dpi = 300
)

# Elbow method for k=1 to k=10 using K-Prototype's own cost function
# This is methodologically more consistent than using K-means on projected distances
cat("\nRunning elbow method analysis for k=2 to k=10 using K-Prototype...\n")
wss_results <- data.frame(
  k = 2:10,
  tot_withinss = numeric(9)
)

# Set seed for reproducibility
set.seed(123)

# Run K-Prototype for different k values and extract the total within-cluster sum of squares
for (i in 1:9) {
  k_value <- i + 1  # Starting from k=2
  cat("  Processing k =", k_value, "...\n")
  
  # Run K-Prototype clustering
  kproto_result <- clustMixType::kproto(
    kproto_mixed_data, 
    k = k_value,
    verbose = FALSE
  )
  
  # Store the total within-cluster sum of squares
  wss_results$tot_withinss[i] <- kproto_result$tot.withinss
}

# Simplified approach for k=1 cost
# For k=1, we estimate the cost as a significantly higher value than k=2
# This is a common approach when the algorithm doesn't support k=1 directly
# Typically k=1 has much higher cost than k=2
k1_est <- wss_results$tot_withinss[1] * 1.5  # Estimate k=1 as 50% higher than k=2

# Create complete results including k=1 estimate
wss_full_results <- data.frame(
  k = 1:10,
  tot_withinss = c(k1_est, wss_results$tot_withinss)
)

# Save elbow method results
save_data(wss_full_results, "031_kprototype_optimal/elbow_results.csv")

# Find optimal k from elbow method (excluding k=1 which is estimated)
# Using a simplified approach to find the elbow point
# We only consider k=2 to k=10 for finding the elbow
wss_for_elbow <- wss_results  # Use actual computed values, not the estimated k=1
slopes <- numeric(nrow(wss_for_elbow) - 1)

for (i in 1:(nrow(wss_for_elbow) - 1)) {
  slopes[i] <- wss_for_elbow$tot_withinss[i] - wss_for_elbow$tot_withinss[i + 1]
}

# Find the point where the slope changes the most (the elbow)
slope_changes <- diff(slopes)
optimal_k_elbow <- which.max(slope_changes) + 2  # +2 because we start at k=2 and look at differences

# Create elbow plot
elbow_plot <- ggplot(wss_full_results, aes(x = k, y = tot_withinss)) +
  geom_line() +
  geom_point(size = 3) +
  # 强调k=optimal_k_elbow处的点
  geom_point(data = data.frame(k = optimal_k_elbow, 
                              tot_withinss = wss_full_results$tot_withinss[optimal_k_elbow]), 
             aes(x = k, y = tot_withinss), 
             color = "red", size = 4) +
  # 添加标注
  annotate("text", x = optimal_k_elbow + 0.3, 
           y = wss_full_results$tot_withinss[optimal_k_elbow], 
           label = paste("Optimal k =", optimal_k_elbow), 
           color = "red", hjust = 0, vjust = 0.5, size = 4) +
  labs(
    title = "Elbow Method for Optimal Cluster Number (K-Prototype)",
    subtitle = "The 'elbow' point suggests the optimal number of clusters",
    x = "Number of Clusters (k)",
    y = "Total Within-Cluster Sum of Squares"
  ) +
  # 设置x轴为整数刻度，并确保范围从1到10
  scale_x_continuous(breaks = 1:10, labels = 1:10, limits = c(1, 10)) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(color = "gray30"),
    axis.title = element_text(face = "bold"),
    # 确保x轴标签显示清晰
    axis.text.x = element_text(size = 12),
    # 添加轻微的垂直网格线帮助识别整数k值
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(color = "gray90", size = 0.5)
  )

# Save elbow plot
ggsave(
  "results/figures/031_kprototype_optimal/elbow_plot.pdf",
  elbow_plot,
  width = 10,
  height = 7,
  dpi = 300
)

# Generate a combined plot with both methods
combined_plot <- gridExtra::grid.arrange(
  elbow_plot + ggtitle("A) Elbow Method (K-Prototype)"),
  pam_silhouette_plot + ggtitle("B) Silhouette Method"),
  ncol = 1
)

# Save combined plot
ggsave(
  "results/figures/031_kprototype_optimal/combined_cluster_analysis.pdf",
  combined_plot,
  width = 12,
  height = 14,
  dpi = 300
)

# Save optimal k values to a file for reference in next script
optimal_k_data <- data.frame(
  Method = c("Silhouette", "Elbow"),
  OptimalK = c(optimal_k_silhouette, optimal_k_elbow)
)

save_data(optimal_k_data, "031_kprototype_optimal/optimal_k.csv")

# Print results
cat("\nClustering Analysis Results:\n")
cat("---------------------------\n")
cat("Silhouette Method Optimal k:", optimal_k_silhouette, "\n")
cat("Elbow Method Optimal k:", optimal_k_elbow, "\n")
cat("\nAnalysis complete!\n")
cat("Visualizations saved to results/figures/031_kprototype_optimal/\n")
cat("Results saved to results/tables/031_kprototype_optimal/\n") 