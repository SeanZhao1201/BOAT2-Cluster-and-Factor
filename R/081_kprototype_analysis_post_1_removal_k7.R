# R/081_kprototype_analysis_post_4_removal.R
# This script removes one specific outlier case (ORG_Employees == 3700)
# from BOAT2_Data_Success.csv, then performs K-prototypes clustering for a fixed k=7.
# The Project_Success variable is included in the clustering.

# 1. Load Setup and Data -----------------------------------------------------
cat("============== SCRIPT R/081 (K=7 Post 1 Removal) STARTING ==============\n")

# Ensure essential packages are loaded (in case 000_setup.R is not run or complete)
if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
if (!requireNamespace("clustMixType", quietly = TRUE)) install.packages("clustMixType")
if (!requireNamespace("readr", quietly = TRUE)) install.packages("readr") # For read_csv

library(dplyr)
library(clustMixType)
library(readr)

# Attempt to source setup file, but proceed if not found
if (file.exists("R/000_setup.R")) {
  source("R/000_setup.R")
  cat("R/000_setup.R sourced successfully.\n")
} else {
  cat("Warning: R/000_setup.R not found. Essential packages loaded directly.\n")
}

# Create subdirectories for results
K_FIXED_FOR_SCRIPT <- 7
fig_dir_081 <- paste0("results/figures/081_kprototype_post_1_removal_k", K_FIXED_FOR_SCRIPT)
tbl_dir_081 <- paste0("results/tables/081_kprototype_post_1_removal_k", K_FIXED_FOR_SCRIPT)

for(dir_p in c(fig_dir_081, tbl_dir_081)){
  if (!dir.exists(dir_p)) {
    dir.create(dir_p, recursive = TRUE)
    cat(paste("Created directory:", dir_p, "\n"))
  }
}

# Load the dataset (BOAT2_Data_Success.csv)
data_file_path <- "data/BOAT2_Data_Success.csv"
if (file.exists(data_file_path)) {
  data_original_success <- read_csv(data_file_path, show_col_types = FALSE)
  cat("Loaded dataset", data_file_path, "with", nrow(data_original_success), "rows and", ncol(data_original_success), "columns.\n")
} else {
  stop(paste("Error: Dataset not found at", data_file_path))
}

# Add an original row ID for tracking
data_original_success <- data_original_success %>%
  mutate(Original_Row_ID = 1:n())

# Check for 'X' column (often an unnamed index from previous saves) and remove if present
if ("X" %in% colnames(data_original_success)) {
  cat("Removing 'X' column (likely an old index).\n")
  data_original_success <- data_original_success %>% select(-X)
}
if ("X1" %in% colnames(data_original_success)) { # Also check for X1, common from read.csv
  cat("Removing 'X1' column (likely an old index).\n")
  data_original_success <- data_original_success %>% select(-X1)
}

# 2. Identify and Remove the Single Outlier Case ---------------------------
cat("\n--- Identifying and removing 1 outlier case ---\n")
ids_to_remove_081 <- c()
outlier_employee_val_081 <- 3700

case_outlier_081 <- data_original_success %>%
  filter(ORG_Employees == outlier_employee_val_081) %>%
  slice(1) 

if (nrow(case_outlier_081) > 0) {
  ids_to_remove_081 <- c(ids_to_remove_081, case_outlier_081$Original_Row_ID)
  cat("Identified outlier case ID for removal:", case_outlier_081$Original_Row_ID, "(ORG_Employees:", case_outlier_081$ORG_Employees, ")\n")
  data_cleaned_081 <- data_original_success %>% filter(!Original_Row_ID %in% ids_to_remove_081)
  cat("Removed 1 case. New dataset size:", nrow(data_cleaned_081), "rows.\n")
} else {
  data_cleaned_081 <- data_original_success
  cat("Warning: Outlier case (ORG_Employees == ", outlier_employee_val_081, ") not found. Proceeding with original data for k=", K_FIXED_FOR_SCRIPT, " clustering.\n")
}

if (nrow(data_cleaned_081) < K_FIXED_FOR_SCRIPT) { 
  stop(paste("Error: Too few data points (", nrow(data_cleaned_081), ") remaining after removal for k=", K_FIXED_FOR_SCRIPT, " clustering."))
}

# 3. Prepare data_cleaned for K-prototypes clustering (k=7) --
cat("\n--- Preparing data for final k=", K_FIXED_FOR_SCRIPT, " clustering (post 1 removal) ---\n")
numerical_vars_final_081 <- c("ORG_Employees", "ORG_Locations", "ORG_Departments", "ORG_Layers")
exclude_vars_final_081 <- c("PDM_Selected", "PDM_Experience_DBB", "PDM_Experience_DB", 
                              "PDM_Experience_PDB", "PDM_Experience_CMAR", "PDM_Experience_IPD", 
                              "Original_Row_ID")
exclude_vars_final_081 <- exclude_vars_final_081[exclude_vars_final_081 %in% colnames(data_cleaned_081)]

all_vars_for_final_clustering_081 <- setdiff(colnames(data_cleaned_081), exclude_vars_final_081)
categorical_vars_final_081 <- setdiff(all_vars_for_final_clustering_081, numerical_vars_final_081)

numerical_vars_final_exist_081 <- numerical_vars_final_081[numerical_vars_final_081 %in% colnames(data_cleaned_081)]
categorical_vars_final_exist_081 <- categorical_vars_final_081[categorical_vars_final_081 %in% colnames(data_cleaned_081)]

cat("Final k=", K_FIXED_FOR_SCRIPT, " clustering - Numerical vars:", paste(numerical_vars_final_exist_081, collapse=", "), "\n")
cat("Final k=", K_FIXED_FOR_SCRIPT, " clustering - Categorical vars (", length(categorical_vars_final_exist_081), "):", paste(categorical_vars_final_exist_081, collapse=", "), "\n")
cat("Final k=", K_FIXED_FOR_SCRIPT, " clustering - Project_Success is in categorical vars:", ("Project_Success" %in% categorical_vars_final_exist_081), "\n")

vars_for_kproto_input_final_081 <- c(numerical_vars_final_exist_081, categorical_vars_final_exist_081)
vars_for_kproto_input_final_081 <- unique(vars_for_kproto_input_final_081)

if (length(vars_for_kproto_input_final_081) == 0) {
  stop(paste("Error: No columns selected for the final k-prototypes input (k=", K_FIXED_FOR_SCRIPT, ")."))
}

kproto_input_final_df_081 <- data_cleaned_081 %>%
  select(all_of(vars_for_kproto_input_final_081))

# Convert categorical variables to ordered factors.
# Assuming Likert 1-5 for Project_Success. If different, adjust levels.
if (length(categorical_vars_final_exist_081) > 0) {
  kproto_input_final_df_081 <- kproto_input_final_df_081 %>%
    mutate(across(all_of(categorical_vars_final_exist_081), function(col) {
      if(is.numeric(col) && !all(is.na(col))) {
        max_val <- max(col, na.rm = TRUE)
        defined_levels <- 1:max(5, ceiling(max_val))
        # Potential adjustment for specific Project_Success scale if known, e.g., 1:7
        # if (deparse(substitute(col)) == "Project_Success") defined_levels <- 1:7 
        return(ordered(round(col), levels = defined_levels))
      } else {
        return(factor(col))
      }
    }))
}
cat("Final k-prototypes input data for k=", K_FIXED_FOR_SCRIPT, " prepared with", nrow(kproto_input_final_df_081), "rows and", ncol(kproto_input_final_df_081), "columns.\n")

# 4. Perform K-Prototypes Clustering (k=K_FIXED_FOR_SCRIPT) ----------------------------------
cat("\n--- Performing K-Prototype clustering for k=", K_FIXED_FOR_SCRIPT, " (post 1 removal) ---\n")
final_clustering_seed_081 <- 7890 # Distinct seed for this analysis run

if (nrow(unique(kproto_input_final_df_081)) < K_FIXED_FOR_SCRIPT) {
  stop(paste("Error: Number of unique rows (", nrow(unique(kproto_input_final_df_081)), ") is less than k=", K_FIXED_FOR_SCRIPT, ". Cannot proceed."))
}
if (nrow(kproto_input_final_df_081) < K_FIXED_FOR_SCRIPT) {
  stop(paste("Error: Number of rows (", nrow(kproto_input_final_df_081), ") is less than k=", K_FIXED_FOR_SCRIPT, ". Cannot proceed."))
}

set.seed(final_clustering_seed_081)
kproto_result_final_k_081 <- clustMixType::kproto(
  kproto_input_final_df_081, 
  k = K_FIXED_FOR_SCRIPT,
  verbose = TRUE,
  nstart = 25 
)

cluster_col_name <- paste0("Cluster_k", K_FIXED_FOR_SCRIPT)
final_cluster_results_081 <- data_cleaned_081 %>%
  mutate(!!cluster_col_name := factor(kproto_result_final_k_081$cluster))

results_csv_path_081 <- file.path(tbl_dir_081, paste0("kproto_clusters_k", K_FIXED_FOR_SCRIPT, "_post_1_removed.csv"))
write.csv(final_cluster_results_081, results_csv_path_081, row.names = FALSE)
cat("Final clustering results (k=", K_FIXED_FOR_SCRIPT, ") saved to:", results_csv_path_081, "\n")

cat("Calculating and saving centroids for k=", K_FIXED_FOR_SCRIPT, "...\n")
centroids_k_081 <- data.frame(Cluster = 1:K_FIXED_FOR_SCRIPT)
for (var in numerical_vars_final_exist_081) {
  centroids_k_081[, var] <- kproto_result_final_k_081$centers[, var]
}
for (var in categorical_vars_final_exist_081) {
  centroids_k_081[, var] <- kproto_result_final_k_081$centers[, var]
}
centroids_csv_path_081 <- file.path(tbl_dir_081, paste0("kproto_centroids_k", K_FIXED_FOR_SCRIPT, "_post_1_removed.csv"))
write.csv(centroids_k_081, centroids_csv_path_081, row.names = FALSE)
cat("Cluster centroids (k=", K_FIXED_FOR_SCRIPT, ") saved to:", centroids_csv_path_081, "\n")

cat("Calculating and saving medians for k=", K_FIXED_FOR_SCRIPT, "...\n")
medians_k_081 <- data.frame(Cluster = 1:K_FIXED_FOR_SCRIPT)
vars_for_medians_081 <- colnames(kproto_input_final_df_081)

for (var in vars_for_medians_081) {
  medians_for_var_081 <- sapply(1:K_FIXED_FOR_SCRIPT, function(cl_num) {
    current_cluster_data_for_var <- final_cluster_results_081[[var]][final_cluster_results_081[[cluster_col_name]] == cl_num]
    if (var %in% numerical_vars_final_exist_081 || is.numeric(current_cluster_data_for_var)) {
        return(median(as.numeric(current_cluster_data_for_var), na.rm = TRUE))
    } else { 
        # For factors that were originally numeric Likert scales and converted by kproto input prep
        # We need their numeric representation for median
        # This assumes levels correspond to 1,2,3...N, which our `ordered(round(col), levels = ...)` aims for.
        # The values in kproto_input_final_df_081 for these columns are factors.
        # We need to access their numeric equivalent from the original scale.
        original_numeric_values_for_var_in_cluster <- as.numeric(levels(kproto_input_final_df_081[[var]]))[as.numeric(kproto_input_final_df_081[[var]][final_cluster_results_081[[cluster_col_name]] == cl_num])]
        return(median(original_numeric_values_for_var_in_cluster, na.rm = TRUE))
    }
  })
  medians_k_081[, var] <- medians_for_var_081
}

medians_csv_path_081 <- file.path(tbl_dir_081, paste0("kproto_medians_k", K_FIXED_FOR_SCRIPT, "_post_1_removed.csv"))
write.csv(medians_k_081, medians_csv_path_081, row.names = FALSE)
cat("Cluster medians (k=", K_FIXED_FOR_SCRIPT, ") saved to:", medians_csv_path_081, "\n")

cat("\n============== SCRIPT R/081 (K=7 Post 1 Removal) FINISHED ==============\n") 