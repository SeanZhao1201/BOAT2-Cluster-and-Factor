# 072 Display Cases in Cluster 3 (from k=5, Outlier Removed)
# This script re-runs the analysis from 071 and prints the data for cases in Cluster 3.

# 1. Load Setup and Data -----------------------------------------------------
if (file.exists("R/000_setup.R")) {
  source("R/000_setup.R")
} else {
  if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
  if (!requireNamespace("clustMixType", quietly = TRUE)) install.packages("clustMixType")
  library(dplyr)
  library(clustMixType)
  cat("Warning: R/000_setup.R not found. Loaded essential packages directly.\n")
}

if (file.exists("data/BOAT2_Data_Enhanced.csv")) {
  data_original <- read.csv("data/BOAT2_Data_Enhanced.csv")
  cat("Loaded original dataset with", nrow(data_original), "rows and", ncol(data_original), "columns.\n")
} else {
  stop("Error: data/BOAT2_Data_Enhanced.csv not found. Please ensure the data file exists.")
}

# 2. Identify and Remove Outlier -------------------------------------------
outlier_value_org_employees <- 3700
cat(paste0("\nAttempting to remove outlier(s) where ORG_Employees == ", outlier_value_org_employees, "...\n"))
if (!"ORG_Employees" %in% colnames(data_original)) {
  stop("Error: ORG_Employees column not found.")
}
data <- data_original %>%
  filter(ORG_Employees != outlier_value_org_employees)
rows_removed <- nrow(data_original) - nrow(data)
cat(paste(rows_removed, "row(s) removed. Dataset now has", nrow(data), "rows.\n"))

# 3. Define Variables --------------------------------------------------------
numerical_org_vars <- c(
  "ORG_Employees",
  "ORG_Locations",
  "ORG_Departments",
  "ORG_Layers"
)
missing_org_vars <- numerical_org_vars[!numerical_org_vars %in% colnames(data)]
if (length(missing_org_vars) > 0) {
  stop(paste("Error: ORG variables not found after outlier removal:", paste(missing_org_vars, collapse=", ")))
}

exclude_vars <- c("Owner_Type",
                  "PDM_Selected",
                  "PDM_Experience_DBB",
                  "PDM_Experience_DB",
                  "PDM_Experience_PDB",
                  "PDM_Experience_CMAR",
                  "PDM_Experience_IPD")
exclude_vars <- exclude_vars[exclude_vars %in% colnames(data)]

all_vars <- setdiff(colnames(data), exclude_vars)
categorical_vars <- setdiff(all_vars, numerical_org_vars)
categorical_vars <- categorical_vars[categorical_vars %in% colnames(data)]
categorical_vars <- setdiff(categorical_vars, numerical_org_vars)

cat("Numerical ORG variables defined.
")
if (length(categorical_vars) > 0) {
  cat("Categorical variables for clustering defined: ", length(categorical_vars), " variables.\n")
} else {
  cat("Warning: No categorical variables defined for clustering.\n")
}

# 4. Prepare data for K-prototypes clustering -------------------------------
selected_vars_for_clustering <- c(numerical_org_vars, categorical_vars)
selected_vars_for_clustering <- selected_vars_for_clustering[selected_vars_for_clustering %in% colnames(data)]

if (length(selected_vars_for_clustering) == 0) {
    stop("Error: No variables selected for clustering from the modified dataset.")
}

# kproto_analysis_data will store the data used for clustering, to which we'll add cluster labels
# It contains the original values (or rounded for categoricals before ordering)
kproto_analysis_data <- data %>%
  select(all_of(selected_vars_for_clustering))

# kproto_mixed_input_data is for the kproto function, with ordered factors
if (length(categorical_vars) > 0) {
  kproto_mixed_input_data <- kproto_analysis_data %>%
    mutate(across(all_of(categorical_vars), ~ ordered(round(.), levels = 1:5)))
} else {
  kproto_mixed_input_data <- kproto_analysis_data[, numerical_org_vars, drop = FALSE]
}

cat("Prepared kproto_mixed_input_data for clustering with", 
    nrow(kproto_mixed_input_data), "rows and", 
    ncol(kproto_mixed_input_data), "columns.\n")

# 5. Perform K-Prototype Clustering (k=5, outlier removed) ------------------
k_fixed <- 5
set.seed(123) # For reproducibility

cat(paste0("\nPerforming K-Prototype clustering with k=", k_fixed, " on outlier-removed data...\n"))

numerical_cols_for_kproto <- intersect(numerical_org_vars, colnames(kproto_mixed_input_data))
if (length(numerical_cols_for_kproto) > 0) {
  has_issues <- sapply(kproto_mixed_input_data[, numerical_cols_for_kproto, drop=FALSE], function(x) any(!is.finite(x) | is.na(x)))
  if (any(has_issues)) {
    cat("Warning: Non-finite or NA values found. Imputing...\n")
    for(col_name in names(has_issues[has_issues])) {
        if(is.numeric(kproto_mixed_input_data[[col_name]])) {
            mean_val <- mean(kproto_mixed_input_data[[col_name]], na.rm = TRUE)
            kproto_mixed_input_data[[col_name]][!is.finite(kproto_mixed_input_data[[col_name]]) | is.na(kproto_mixed_input_data[[col_name]])] <- mean_val
        }
    }
  }
}

if (ncol(kproto_mixed_input_data) == 0) stop("Error: kproto_mixed_input_data has 0 columns.")
if (nrow(kproto_mixed_input_data) < k_fixed) stop("Error: Not enough rows for k.")

if (length(categorical_vars) > 0 && length(numerical_cols_for_kproto) > 0) {
    kproto_result <- clustMixType::kproto(
      kproto_mixed_input_data, k = k_fixed, verbose = FALSE
    )
} else if (length(numerical_cols_for_kproto) > 0) {
    kproto_result <- clustMixType::kproto(
      kproto_mixed_input_data[, numerical_cols_for_kproto, drop=FALSE], k = k_fixed, verbose = FALSE
    )
} else if (length(categorical_vars) > 0) {
    kproto_result <- clustMixType::kproto(
      kproto_mixed_input_data[, categorical_vars, drop=FALSE], k = k_fixed, verbose = FALSE
    )
} else {
    stop("Error: No variables for clustering.")
}

cat("K-Prototype clustering (outlier removed) complete.\n")
cat("Cluster sizes:", paste(table(kproto_result$cluster), collapse=", "), "\n")

# 6. Attach Cluster Assignments to Analysis Data ----------------------------
# Use kproto_analysis_data which has the original (or rounded) values of clustering variables
kproto_analysis_data_with_clusters <- kproto_analysis_data %>%
  mutate(Cluster = factor(kproto_result$cluster))

cat("Cluster assignments added to analysis data.\n")

# 7. Isolate and Print Cases in Cluster 3 -----------------------------------
# The cluster numbers from kproto_result$cluster might be 1, 2, 3, 4, 5.
# We need to identify which one corresponds to the cluster of size 3 from the previous output.
# From the previous run of R/071: New cluster sizes: 52, 14, 3, 25, 14
# This means the cluster labeled '3' by kproto should be the one with 3 members.

cluster_to_display <- 3 # Assuming the third cluster in the table is the one of interest.

cases_in_cluster3 <- kproto_analysis_data_with_clusters %>%
  filter(Cluster == cluster_to_display)

cat(paste0("\n--- Cases in Cluster ", cluster_to_display, " (k=5, Outlier Removed) ---\n"))
if (nrow(cases_in_cluster3) > 0) {
  # To ensure all columns are printed if numerous
  options(width = 200) # Increase print width
  print(as.data.frame(cases_in_cluster3)) # Print as data.frame for better full output
} else {
  cat(paste0("No cases found in Cluster ", cluster_to_display, ". This might indicate an issue or different cluster labeling.\n"))
  cat("Actual cluster sizes from this run:", paste(table(kproto_result$cluster), collapse=", "), "\n")
}
cat("\n--- End of Cluster 3 Cases Display ---\n")

# Additionally, let's print the summary stats for Cluster 3 for ORG variables for quick verification
cat(paste0("\n--- Summary for ORG Variables in Cluster ", cluster_to_display, " ---\n"))
for (org_var in numerical_org_vars) {
  if (org_var %in% colnames(cases_in_cluster3)) {
    cat(paste0("\nSummary for ", org_var, " in Cluster ", cluster_to_display, ":\n"))
    summary_val <- summary(cases_in_cluster3[[org_var]])
    cat(paste("  Min:", summary_val[1], " Mean:", summary_val[4], " Max:", summary_val[6], "\n"))
  }
}

cat("\nScript 072 execution complete.\n") 