# R/076_optimal_k_after_4cases_removed.R
# This script removes four specific cases (1 outlier + 3 from a specific cluster)
# and then performs an Elbow Plot analysis to suggest an optimal k for K-prototypes.

# 1. Load Setup and Data -----------------------------------------------------
if (file.exists("R/000_setup.R")) {
  source("R/000_setup.R")
} else {
  if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
  if (!requireNamespace("clustMixType", quietly = TRUE)) install.packages("clustMixType")
  if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
  library(dplyr)
  library(clustMixType)
  library(ggplot2)
  cat("Warning: R/000_setup.R not found. Loaded essential packages directly.\n")
}

# Ensure ggplot2 is loaded if setup didn't cover it
if (!("ggplot2" %in% .packages())) library(ggplot2)

# Create subdirectory for results
plot_dir <- "results/figures/076_optimal_k_post_removal"
if (!dir.exists(plot_dir)) {
  dir.create(plot_dir, recursive = TRUE)
  cat(paste("Created directory:", plot_dir, "\n"))
}
tables_dir <- "results/tables/076_optimal_k_post_removal"
if (!dir.exists(tables_dir)) {
  dir.create(tables_dir, recursive = TRUE)
  cat(paste("Created directory:", tables_dir, "\n"))
}

# Load the original dataset
if (file.exists("data/BOAT2_Data_Enhanced.csv")) {
  data_original <- read.csv("data/BOAT2_Data_Enhanced.csv")
  cat("Loaded original dataset with", nrow(data_original), "rows and", ncol(data_original), "columns.\n")
} else {
  stop("Error: data/BOAT2_Data_Enhanced.csv not found.")
}

# Add an original row ID for tracking
data_original <- data_original %>%
  mutate(Original_Row_ID = 1:n())

# 2. Identify the Four Cases for Removal -----------------------------------
ids_to_remove <- c()

# Case 1: Extreme Outlier
outlier_employee_count <- 3700
case_outlier_df <- data_original %>%
  filter(ORG_Employees == outlier_employee_count) %>%
  slice(1) # Ensure only one row if duplicates exist

if (nrow(case_outlier_df) > 0) {
  ids_to_remove <- c(ids_to_remove, case_outlier_df$Original_Row_ID)
  cat("Identified outlier case ID for removal:", case_outlier_df$Original_Row_ID, "(ORG_Employees:", case_outlier_df$ORG_Employees, ")\n")
} else {
  cat("Warning: Outlier case (ORG_Employees == ", outlier_employee_count, ") not found.\n")
}

# Identify Cases 2, 3, 4: From Cluster 3 (post-outlier removal, k=5, seed=123)
cat("Identifying the 3 cluster cases for removal...\n")
data_for_temp_clustering <- data_original
if (nrow(case_outlier_df) > 0) { # Remove outlier first if found
    data_for_temp_clustering <- data_original %>%
        filter(Original_Row_ID != case_outlier_df$Original_Row_ID)
}

# Define variables for this temporary clustering (consistent with previous scripts)
numerical_org_vars_temp <- c("ORG_Employees", "ORG_Locations", "ORG_Departments", "ORG_Layers")
exclude_vars_temp <- c("PDM_Selected", "PDM_Experience_DBB", "PDM_Experience_DB", 
                         "PDM_Experience_PDB", "PDM_Experience_CMAR", "PDM_Experience_IPD", "X")
exclude_vars_temp <- exclude_vars_temp[exclude_vars_temp %in% colnames(data_for_temp_clustering)]
all_categorical_vars_temp <- setdiff(colnames(data_for_temp_clustering), c(numerical_org_vars_temp, exclude_vars_temp, "Original_Row_ID"))
vars_for_kproto_input_temp <- c(numerical_org_vars_temp, all_categorical_vars_temp)
vars_for_kproto_input_temp <- vars_for_kproto_input_temp[vars_for_kproto_input_temp %in% colnames(data_for_temp_clustering)]

kproto_mixed_input_temp <- data_for_temp_clustering %>%
  select(all_of(vars_for_kproto_input_temp)) %>%
  mutate(across(all_of(all_categorical_vars_temp[all_categorical_vars_temp %in% vars_for_kproto_input_temp]), ~ ordered(round(.), levels = 1:5)))

kproto_analysis_temp <- data_for_temp_clustering %>%
  select(Original_Row_ID, all_of(vars_for_kproto_input_temp))

k_fixed_temp <- 5
set.seed(123) # Seed used in script 071, 072, 073, 074 to identify this cluster
kproto_result_temp <- clustMixType::kproto(kproto_mixed_input_temp, k = k_fixed_temp, verbose = FALSE)
kproto_analysis_temp$Cluster <- factor(kproto_result_temp$cluster)

cluster_counts_temp <- table(kproto_result_temp$cluster)
cat("Temporary clustering (k=5, seed=123, after 1 outlier removal) cluster sizes:", paste(cluster_counts_temp, collapse=", "), "\n")
cluster_label_for_size_3_temp <- names(which(cluster_counts_temp == 3))[1]

if (!is.na(cluster_label_for_size_3_temp)) {
  target_cluster_id_temp <- as.numeric(cluster_label_for_size_3_temp)
  cases_cluster3_temp_df <- kproto_analysis_temp %>%
    filter(Cluster == target_cluster_id_temp)
  ids_to_remove <- unique(c(ids_to_remove, cases_cluster3_temp_df$Original_Row_ID))
  cat("Identified", nrow(cases_cluster3_temp_df), "case IDs from temporary Cluster", target_cluster_id_temp, "for removal:", paste(cases_cluster3_temp_df$Original_Row_ID, collapse=", "), "\n")
} else {
  cat("Warning: Could not identify the cluster with 3 members in the temporary clustering step.\n")
}

# 3. Remove Identified Cases and Prepare Final Dataset ---------------------
cat("Total unique Original_Row_IDs identified for removal:", paste(ids_to_remove, collapse=", "), "\n")
data_cleaned <- data_original %>%
  filter(!Original_Row_ID %in% ids_to_remove)
cat("Removed", nrow(data_original) - nrow(data_cleaned), "cases. New dataset size:", nrow(data_cleaned), "rows.\n")

if (nrow(data_cleaned) < 10) { # Arbitrary small number check
  stop("Error: Too few data points remaining after removal to perform meaningful cluster analysis.")
}

# 4. Prepare data_cleaned for K-prototypes clustering (Optimal K analysis) --
# Define numerical and categorical variables for the main optimal K analysis
numerical_vars_final <- c("ORG_Employees", "ORG_Locations", "ORG_Departments", "ORG_Layers")
exclude_vars_final <- c("PDM_Selected", "PDM_Experience_DBB", "PDM_Experience_DB", 
                          "PDM_Experience_PDB", "PDM_Experience_CMAR", "PDM_Experience_IPD", "X", "Original_Row_ID")
exclude_vars_final <- exclude_vars_final[exclude_vars_final %in% colnames(data_cleaned)]

all_vars_final <- setdiff(colnames(data_cleaned), exclude_vars_final)
categorical_vars_final <- setdiff(all_vars_final, numerical_vars_final)

# Ensure variables exist in data_cleaned
numerical_vars_final <- numerical_vars_final[numerical_vars_final %in% colnames(data_cleaned)]
categorical_vars_final <- categorical_vars_final[categorical_vars_final %in% colnames(data_cleaned)]

cat("Final numerical variables for clustering:", paste(numerical_vars_final, collapse=", "), "\n")
cat("Final categorical variables for clustering (", length(categorical_vars_final), ")\n")

kproto_input_final <- data_cleaned %>%
  select(all_of(c(numerical_vars_final, categorical_vars_final)))

if (length(categorical_vars_final) > 0) {
  kproto_input_final <- kproto_input_final %>%
    mutate(across(all_of(categorical_vars_final), ~ ordered(round(.), levels = 1:5)))
}

if (ncol(kproto_input_final) == 0) {
  stop("Error: No columns selected for the final k-prototypes input.")
}
cat("Final k-prototypes input data prepared with", nrow(kproto_input_final), "rows and", ncol(kproto_input_final), "columns.\n")

# 5. Determine Optimal Number of Clusters (Elbow Method) -------------------
cat("\nDetermining optimal number of clusters using K-prototype algorithm (Elbow Method)...\n")

max_k <- min(10, floor(nrow(kproto_input_final) / 2) -1) # Ensure k is reasonable
if (max_k < 2) max_k <- 2
k_values_elbow <- 2:max_k
wss_results_elbow <- data.frame(
  k = k_values_elbow,
  tot_withinss = numeric(length(k_values_elbow))
)

# Set seed for reproducibility of the elbow method loop
main_elbow_seed <- 456 

for (i in 1:length(k_values_elbow)) {
  k_val <- k_values_elbow[i]
  cat("  Processing k =", k_val, "...")
  set.seed(main_elbow_seed) # Consistent seed for each k-proto run in this loop
  
  # Check for enough unique rows for k-prototypes
  if (nrow(unique(kproto_input_final)) < k_val) {
    cat("Skipping k=", k_val, ", not enough unique rows (", nrow(unique(kproto_input_final)), ") for clustering.\n")
    wss_results_elbow$tot_withinss[i] <- NA # Mark as NA if skipped
    next
  }

  kproto_run <- clustMixType::kproto(
    kproto_input_final, 
    k = k_val,
    verbose = FALSE
  )
  wss_results_elbow$tot_withinss[i] <- kproto_run$tot.withinss
  cat(" WSS:", kproto_run$tot.withinss, "\n")
}

# Remove rows with NA WSS (if any k was skipped)
wss_results_elbow <- wss_results_elbow[!is.na(wss_results_elbow$tot_withinss), ]

if(nrow(wss_results_elbow) < 2) {
    stop("Error: Not enough WSS results to generate an elbow plot (less than 2 k values processed).")
}

# Save WSS results
write.csv(wss_results_elbow, file.path(tables_dir, "elbow_method_wss_results_post_4_removed.csv"), row.names = FALSE)
cat("WSS results saved.\n")

# Find optimal k from elbow method using the improved elbow detection function
find_elbow <- function(x, y) {
  if (length(x) < 3) return(length(x)) # Not enough points for a clear elbow, return max k
  first_point <- c(x[1], y[1])
  last_point <- c(x[length(x)], y[length(y)])
  line_vec <- last_point - first_point
  distances <- numeric(length(x))
  for (i in 1:length(x)) {
    point <- c(x[i], y[i])
    point_vec <- point - first_point
    line_len_sq <- sum(line_vec^2)
    if (line_len_sq == 0) { # All points are the same or first=last
        distances[i] <- sum((point - first_point)^2) # Distance to first point
        next
    }
    projection <- sum(point_vec * line_vec) / line_len_sq
    projection <- max(0, min(1, projection))
    closest <- first_point + projection * line_vec
    distances[i] <- sqrt(sum((point - closest)^2))
  }
  return(which.max(distances))
}

optimal_k_elbow_val <- NA # Initialize
if (nrow(wss_results_elbow) > 0) {
    elbow_idx <- find_elbow(wss_results_elbow$k, wss_results_elbow$tot_withinss)
    optimal_k_elbow_val <- wss_results_elbow$k[elbow_idx]
    cat("\nOptimal k from angle-based elbow method (calculated but not plotted explicitly):", optimal_k_elbow_val, "\n")
} else {
    cat("\nCould not determine optimal k from elbow method as no WSS results were generated.\n")
}


# 6. Generate and Save the Elbow Plot --------------------------------------
elbow_plot_final <- ggplot(wss_results_elbow, aes(x = k, y = tot_withinss)) +
  geom_line(color="blue") +
  geom_point(size = 3, color="red") +
  labs(
    title = "Elbow Method for Optimal k (After Removing 4 Specific Cases)",
    subtitle = paste("Dataset size after removal:", nrow(data_cleaned), "rows."), # Corrected subtitle
    x = "Number of Clusters (k)",
    y = "Total Within-Cluster Sum of Squares (WSS)"
  ) +
  scale_x_continuous(breaks = wss_results_elbow$k) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(color = "gray30")
  )

# The following block is now intentionally removed to avoid plotting the suggested elbow line and text.

plot_file_path <- file.path(plot_dir, "elbow_plot_post_4_removed.pdf")
ggsave(plot_file_path, plot = elbow_plot_final, width = 10, height = 7, dpi = 300)
cat("Elbow plot saved to:", plot_file_path, "\n")

cat("\nScript 076 execution complete.\n") 