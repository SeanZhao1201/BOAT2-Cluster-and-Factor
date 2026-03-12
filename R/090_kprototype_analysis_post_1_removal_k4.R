# R/090_kprototype_analysis_post_1_removal_k4.R
# This script removes one specific outlier case (ORG_Employees == 3700)
# from BOAT2_Data_Success.csv, then performs K-prototypes clustering for a fixed k=4.
# The Project_Success variable is included in the clustering.

# 1. Load Setup and Data -----------------------------------------------------
cat("============== SCRIPT R/090 (K=4 Post 1 Removal) STARTING ==============\n")

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
K_FIXED_FOR_SCRIPT <- 4
fig_dir_090 <- paste0("results/figures/090_kprototype_post_1_removal_k", K_FIXED_FOR_SCRIPT)
tbl_dir_090 <- paste0("results/tables/090_kprototype_post_1_removal_k", K_FIXED_FOR_SCRIPT)

for(dir_p in c(fig_dir_090, tbl_dir_090)){
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
ids_to_remove_090 <- c()
outlier_employee_val_090 <- 3700

case_outlier_090 <- data_original_success %>%
  filter(ORG_Employees == outlier_employee_val_090) %>%
  slice(1) 

if (nrow(case_outlier_090) > 0) {
  ids_to_remove_090 <- c(ids_to_remove_090, case_outlier_090$Original_Row_ID)
  cat("Identified outlier case ID for removal:", case_outlier_090$Original_Row_ID, "(ORG_Employees:", case_outlier_090$ORG_Employees, ")\n")
  data_cleaned_090 <- data_original_success %>% filter(!Original_Row_ID %in% ids_to_remove_090)
  cat("Removed 1 case. New dataset size:", nrow(data_cleaned_090), "rows.\n")
} else {
  data_cleaned_090 <- data_original_success
  cat("Warning: Outlier case (ORG_Employees == ", outlier_employee_val_090, ") not found. Proceeding with original data for k=", K_FIXED_FOR_SCRIPT, " clustering.\n")
}

if (nrow(data_cleaned_090) < K_FIXED_FOR_SCRIPT) { 
  stop(paste("Error: Too few data points (", nrow(data_cleaned_090), ") remaining after removal for k=", K_FIXED_FOR_SCRIPT, " clustering."))
}

# 3. Prepare data_cleaned for K-prototypes clustering (k=4) --
cat("\n--- Preparing data for final k=", K_FIXED_FOR_SCRIPT, " clustering (post 1 removal) ---\n")
numerical_vars_final_090 <- c("ORG_Employees", "ORG_Locations", "ORG_Departments", "ORG_Layers")
exclude_vars_final_090 <- c("Owner_Type", "PDM_Selected", "PDM_Experience_DBB", "PDM_Experience_DB",
                              "PDM_Experience_PDB", "PDM_Experience_CMAR", "PDM_Experience_IPD",
                              "Original_Row_ID")
exclude_vars_final_090 <- exclude_vars_final_090[exclude_vars_final_090 %in% colnames(data_cleaned_090)]

all_vars_for_final_clustering_090 <- setdiff(colnames(data_cleaned_090), exclude_vars_final_090)
categorical_vars_final_090 <- setdiff(all_vars_for_final_clustering_090, numerical_vars_final_090)

numerical_vars_final_exist_090 <- numerical_vars_final_090[numerical_vars_final_090 %in% colnames(data_cleaned_090)]
categorical_vars_final_exist_090 <- categorical_vars_final_090[categorical_vars_final_090 %in% colnames(data_cleaned_090)]

cat("Final k=", K_FIXED_FOR_SCRIPT, " clustering - Numerical vars:", paste(numerical_vars_final_exist_090, collapse=", "), "\n")
cat("Final k=", K_FIXED_FOR_SCRIPT, " clustering - Categorical vars (", length(categorical_vars_final_exist_090), "):", paste(categorical_vars_final_exist_090, collapse=", "), "\n")
cat("Final k=", K_FIXED_FOR_SCRIPT, " clustering - Project_Success is in categorical vars:", ("Project_Success" %in% categorical_vars_final_exist_090), "\n")

vars_for_kproto_input_final_090 <- c(numerical_vars_final_exist_090, categorical_vars_final_exist_090)
vars_for_kproto_input_final_090 <- unique(vars_for_kproto_input_final_090)

if (length(vars_for_kproto_input_final_090) == 0) {
  stop(paste("Error: No columns selected for the final k-prototypes input (k=", K_FIXED_FOR_SCRIPT, ")."))
}

kproto_input_final_df_090 <- data_cleaned_090 %>%
  select(all_of(vars_for_kproto_input_final_090))

# Convert categorical variables to ordered factors.
# Assuming Likert 1-5 for Project_Success. If different, adjust levels.
if (length(categorical_vars_final_exist_090) > 0) {
  kproto_input_final_df_090 <- kproto_input_final_df_090 %>%
    mutate(across(all_of(categorical_vars_final_exist_090), function(col) {
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
cat("Final k-prototypes input data for k=", K_FIXED_FOR_SCRIPT, " prepared with", nrow(kproto_input_final_df_090), "rows and", ncol(kproto_input_final_df_090), "columns.\n")

# 4. Perform K-Prototypes Clustering (k=K_FIXED_FOR_SCRIPT) ----------------------------------
cat("\n--- Performing K-Prototype clustering for k=", K_FIXED_FOR_SCRIPT, " (post 1 removal) ---\n")
final_clustering_seed_090 <- 7890 # Distinct seed for this analysis run

if (nrow(unique(kproto_input_final_df_090)) < K_FIXED_FOR_SCRIPT) {
  stop(paste("Error: Number of unique rows (", nrow(unique(kproto_input_final_df_090)), ") is less than k=", K_FIXED_FOR_SCRIPT, ". Cannot proceed."))
}
if (nrow(kproto_input_final_df_090) < K_FIXED_FOR_SCRIPT) {
  stop(paste("Error: Number of rows (", nrow(kproto_input_final_df_090), ") is less than k=", K_FIXED_FOR_SCRIPT, ". Cannot proceed."))
}

set.seed(final_clustering_seed_090)
kproto_result_final_k_090 <- clustMixType::kproto(
  kproto_input_final_df_090, 
  k = K_FIXED_FOR_SCRIPT,
  verbose = TRUE,
  nstart = 25 
)

cluster_col_name <- paste0("Cluster_k", K_FIXED_FOR_SCRIPT)
final_cluster_results_090 <- data_cleaned_090 %>%
  mutate(!!cluster_col_name := factor(kproto_result_final_k_090$cluster))

# 交换Cluster 2和Cluster 4的标签
final_cluster_results_090[[cluster_col_name]] <- factor(
  ifelse(final_cluster_results_090[[cluster_col_name]] == 2, 4, 
         ifelse(final_cluster_results_090[[cluster_col_name]] == 4, 2, 
                final_cluster_results_090[[cluster_col_name]]))
)

results_csv_path_090 <- file.path(tbl_dir_090, paste0("kproto_clusters_k", K_FIXED_FOR_SCRIPT, "_post_1_removed.csv"))
write.csv(final_cluster_results_090, results_csv_path_090, row.names = FALSE)
cat("Final clustering results (k=", K_FIXED_FOR_SCRIPT, ") saved to:", results_csv_path_090, "\n")

cat("Calculating and saving centroids for k=", K_FIXED_FOR_SCRIPT, "...\n")
centroids_k_090 <- data.frame(Cluster = 1:K_FIXED_FOR_SCRIPT)
# 创建映射以交换Cluster 2和Cluster 4
cluster_mapping <- c(1, 4, 3, 2) # 1->1, 2->4, 3->3, 4->2

for (var in numerical_vars_final_exist_090) {
  centroids_k_090[, var] <- kproto_result_final_k_090$centers[cluster_mapping, var]
}
for (var in categorical_vars_final_exist_090) {
  centroids_k_090[, var] <- kproto_result_final_k_090$centers[cluster_mapping, var]
}
centroids_csv_path_090 <- file.path(tbl_dir_090, paste0("kproto_centroids_k", K_FIXED_FOR_SCRIPT, "_post_1_removed.csv"))
write.csv(centroids_k_090, centroids_csv_path_090, row.names = FALSE)
cat("Cluster centroids (k=", K_FIXED_FOR_SCRIPT, ") saved to:", centroids_csv_path_090, "\n")

cat("Calculating and saving medians for k=", K_FIXED_FOR_SCRIPT, "...\n")
medians_k_090 <- data.frame(Cluster = 1:K_FIXED_FOR_SCRIPT)
vars_for_medians_090 <- colnames(kproto_input_final_df_090)

# 排除Project_Success变量，因为它是文字类型的分类变量
if ("Project_Success" %in% vars_for_medians_090) {
  cat("  Excluding Project_Success from median calculations as it is categorical...\n")
  vars_for_medians_090 <- vars_for_medians_090[vars_for_medians_090 != "Project_Success"]
}

# 使用交换后的cluster标签计算中位数
for (var in vars_for_medians_090) {
  medians_for_var_090 <- sapply(1:K_FIXED_FOR_SCRIPT, function(cl_idx) {
    # 查找实际的cluster号
    actual_cluster_num <- if(cl_idx == 2) 4 else if(cl_idx == 4) 2 else cl_idx
    
    current_cluster_data_for_var <- final_cluster_results_090[[var]][final_cluster_results_090[[cluster_col_name]] == cl_idx]
    if (var %in% numerical_vars_final_exist_090 || is.numeric(current_cluster_data_for_var)) {
        return(median(as.numeric(current_cluster_data_for_var), na.rm = TRUE))
    } else { 
        original_numeric_values_for_var_in_cluster <- as.numeric(levels(kproto_input_final_df_090[[var]]))[as.numeric(kproto_input_final_df_090[[var]][final_cluster_results_090[[cluster_col_name]] == cl_idx])]
        return(median(original_numeric_values_for_var_in_cluster, na.rm = TRUE))
    }
  })
  medians_k_090[, var] <- medians_for_var_090
}

medians_csv_path_090 <- file.path(tbl_dir_090, paste0("kproto_medians_k", K_FIXED_FOR_SCRIPT, "_post_1_removed.csv"))
write.csv(medians_k_090, medians_csv_path_090, row.names = FALSE)
cat("Cluster medians (k=", K_FIXED_FOR_SCRIPT, ") saved to:", medians_csv_path_090, "\n")

cat("\n============== SCRIPT R/090 (K=4 Post 1 Removal) FINISHED ==============\n") 