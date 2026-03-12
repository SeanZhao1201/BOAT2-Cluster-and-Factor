# 071 K-Prototype ORG Variable Analysis for k=5 (Outlier Removed)
# This script performs K-prototype clustering with k=5 after removing
# a specific outlier, and then analyzes the distribution of ORG variables
# within each new cluster.

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
# Based on the previous analysis (070_script), the outlier had ORG_Employees = 3700.
# We will remove rows matching this criterion.
outlier_value_org_employees <- 3700

cat(paste0("\nAttempting to remove outlier(s) where ORG_Employees == ", outlier_value_org_employees, "...\n"))

# Check if ORG_Employees column exists
if (!"ORG_Employees" %in% colnames(data_original)) {
  stop("Error: ORG_Employees column not found in the dataset. Cannot remove outlier based on this criterion.")
}

# Create the new dataset 'data' by filtering out the outlier
data <- data_original %>%
  filter(ORG_Employees != outlier_value_org_employees)

rows_removed <- nrow(data_original) - nrow(data)
cat(paste(rows_removed, "row(s) removed based on ORG_Employees == ", outlier_value_org_employees, ".\n"))
cat(paste("Dataset now has", nrow(data), "rows.\n"))

if (rows_removed == 0) {
  cat("Warning: No outlier found with ORG_Employees == ", outlier_value_org_employees, ". Clustering will proceed with the original dataset size.\n")
} else if (rows_removed > 1) {
  cat("Warning: More than one row was removed. Please verify if this was intended.\n")
}

# 3. Define Variables (consistent with 070) --------------------------------
numerical_org_vars <- c(
  "ORG_Employees",
  "ORG_Locations",
  "ORG_Departments",
  "ORG_Layers"
)

missing_org_vars <- numerical_org_vars[!numerical_org_vars %in% colnames(data)]
if (length(missing_org_vars) > 0) {
  stop(paste("Error: The following ORG variables are not found in the dataset after outlier removal:", paste(missing_org_vars, collapse=", ")))
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

cat("Numerical ORG variables defined:", paste(numerical_org_vars, collapse=", "), "\n")
if (length(categorical_vars) > 0) {
  cat("Categorical variables for clustering defined:", length(categorical_vars), "variables.\n")
} else {
  cat("Warning: No categorical variables defined for clustering.\n")
}

# 4. Prepare data for K-prototypes clustering (using outlier-removed 'data') ----
selected_vars_for_clustering <- c(numerical_org_vars, categorical_vars)
selected_vars_for_clustering <- selected_vars_for_clustering[selected_vars_for_clustering %in% colnames(data)]

if (length(selected_vars_for_clustering) == 0) {
    stop("Error: No variables selected for clustering from the modified dataset.")
}

kproto_analysis_data <- data %>%
  select(all_of(selected_vars_for_clustering))

if (length(categorical_vars) > 0) {
  kproto_mixed_input_data <- kproto_analysis_data %>%
    mutate(across(all_of(categorical_vars), ~ ordered(round(.), levels = 1:5)))
} else {
  kproto_mixed_input_data <- kproto_analysis_data[, numerical_org_vars, drop = FALSE]
}

cat("Prepared kproto_mixed_input_data for clustering with", 
    nrow(kproto_mixed_input_data), "rows and", 
    ncol(kproto_mixed_input_data), "columns.\n")
cat("Columns in kproto_mixed_input_data:", paste(colnames(kproto_mixed_input_data), collapse=", "), "\n")

cat("Prepared kproto_analysis_data for post-cluster analysis with",
    nrow(kproto_analysis_data), "rows and",
    ncol(kproto_analysis_data), "columns.\n")

# 5. Perform K-Prototype Clustering with k=5 (on outlier-removed data) -----
k_fixed <- 5
set.seed(123) # For reproducibility

cat(paste0("\nPerforming K-Prototype clustering with k=", k_fixed, " on outlier-removed data...\n"))

numerical_cols_for_kproto <- intersect(numerical_org_vars, colnames(kproto_mixed_input_data))
if (length(numerical_cols_for_kproto) > 0) {
  has_issues <- sapply(kproto_mixed_input_data[, numerical_cols_for_kproto, drop=FALSE], function(x) any(!is.finite(x) | is.na(x)))
  if (any(has_issues)) {
    warning_message <- paste("Warning: Non-finite or NA values found in numerical columns post outlier removal:", 
                           paste(names(has_issues[has_issues]), collapse=", "), 
                           ". Imputing...")
    cat(warning_message, "\n")
    for(col_name in names(has_issues[has_issues])) {
        if(is.numeric(kproto_mixed_input_data[[col_name]])) {
            mean_val <- mean(kproto_mixed_input_data[[col_name]], na.rm = TRUE)
            kproto_mixed_input_data[[col_name]][!is.finite(kproto_mixed_input_data[[col_name]]) | is.na(kproto_mixed_input_data[[col_name]])] <- mean_val
            cat(paste("Imputed NAs/Non-finite in", col_name, "with mean value:", mean_val, "\n"))
        }
    }
  }
}

if (ncol(kproto_mixed_input_data) == 0) {
  stop("Error: kproto_mixed_input_data has 0 columns after outlier removal.")
}
if (nrow(kproto_mixed_input_data) < k_fixed) {
  stop(paste("Error: Number of rows (", nrow(kproto_mixed_input_data), ") is less than k (", k_fixed, ") after outlier removal.", sep=""))
}

if (length(categorical_vars) > 0 && length(numerical_cols_for_kproto) > 0) {
    kproto_result_k5_no_outlier <- clustMixType::kproto(
      kproto_mixed_input_data, 
      k = k_fixed,
      verbose = FALSE
    )
} else if (length(numerical_cols_for_kproto) > 0) {
    cat("Only numerical data found. Using k-means functionality within kproto...\n")
     kproto_result_k5_no_outlier <- clustMixType::kproto(
      kproto_mixed_input_data[, numerical_cols_for_kproto, drop=FALSE],
      k = k_fixed,
      verbose = FALSE
    )
} else if (length(categorical_vars) > 0) {
    cat("Only categorical data found. Using k-modes functionality within kproto...\n")
    kproto_result_k5_no_outlier <- clustMixType::kproto(
      kproto_mixed_input_data[, categorical_vars, drop=FALSE],
      k = k_fixed,
      verbose = FALSE
    )
} else {
    stop("Error: No numerical or categorical variables available for clustering in kproto_mixed_input_data after outlier removal.")
}

cat("K-Prototype clustering (outlier removed) complete.\n")
cat("New cluster sizes:", paste(table(kproto_result_k5_no_outlier$cluster), collapse=", "), "\n")

# 6. Attach Cluster Assignments to Analysis Data ----------------------------
kproto_analysis_data_with_clusters <- kproto_analysis_data %>%
  mutate(Cluster = factor(kproto_result_k5_no_outlier$cluster))

cat("New cluster assignments added to analysis data.\n")

# 7. Analyze and Print ORG Variable Distributions per Cluster ---------------
cat("\n--- Analysis of ORG Variables by Cluster (k=5, Outlier Removed) ---\n")

for (org_var in numerical_org_vars) {
  if (!org_var %in% colnames(kproto_analysis_data_with_clusters)) {
    cat(paste0("\nSkipping ", org_var, ": not found in kproto_analysis_data_with_clusters (post outlier removal).\n"))
    next
  }
  
  cat(paste0("\nAnalysis for ", org_var, ":\n"))
  
  summary_stats <- kproto_analysis_data_with_clusters %>%
    group_by(Cluster) %>%
    summarise(
      Count = n(),
      Mean = mean(!!sym(org_var), na.rm = TRUE),
      Median = median(!!sym(org_var), na.rm = TRUE),
      SD = sd(!!sym(org_var), na.rm = TRUE),
      Min = min(!!sym(org_var), na.rm = TRUE),
      Max = max(!!sym(org_var), na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    arrange(Cluster)

  if (requireNamespace("knitr", quietly = TRUE) && nrow(summary_stats) > 0) {
    print(knitr::kable(summary_stats, format = "pipe", digits = 2))
  } else if (nrow(summary_stats) > 0) {
    print(summary_stats)
  } else {
    cat("No summary statistics generated for", org_var, "(post outlier removal).\n")
  }
  cat("\n")
}

cat("--- End of ORG Variable Analysis (Outlier Removed) ---\n")
cat("\nScript 071 execution complete.\n") 