# 070 K-Prototype ORG Variable Analysis for k=5
# This script performs K-prototype clustering with k=5 and analyzes
# the distribution of ORG variables within each cluster.

# 1. Load Setup and Data -----------------------------------------------------
# Ensure R/000_setup.R loads necessary packages like dplyr, clustMixType
if (file.exists("R/000_setup.R")) {
  source("R/000_setup.R")
} else {
  # Fallback if 000_setup.R is not found, load essential packages directly
  if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
  if (!requireNamespace("clustMixType", quietly = TRUE)) install.packages("clustMixType")
  library(dplyr)
  library(clustMixType)
  cat("Warning: R/000_setup.R not found. Loaded essential packages directly.\n")
}

# Load the enhanced dataset
if (file.exists("data/BOAT2_Data_Enhanced.csv")) {
  data <- read.csv("data/BOAT2_Data_Enhanced.csv")
  cat("Loaded dataset with", nrow(data), "rows and", ncol(data), "columns.\n")
} else {
  stop("Error: data/BOAT2_Data_Enhanced.csv not found. Please ensure the data file exists.")
}

# 2. Define Variables (consistent with 031_kprototype_optimal.R) -----------
numerical_org_vars <- c(
  "ORG_Employees",
  "ORG_Locations",
  "ORG_Departments",
  "ORG_Layers"
)

# Check if these columns exist in the loaded data
missing_org_vars <- numerical_org_vars[!numerical_org_vars %in% colnames(data)]
if (length(missing_org_vars) > 0) {
  stop(paste("Error: The following ORG variables are not found in the dataset:", paste(missing_org_vars, collapse=", ")))
}

exclude_vars <- c("PDM_Selected", 
                  "PDM_Experience_DBB", 
                  "PDM_Experience_DB", 
                  "PDM_Experience_PDB", 
                  "PDM_Experience_CMAR", 
                  "PDM_Experience_IPD")
# Ensure exclude_vars only contains columns present in 'data'
exclude_vars <- exclude_vars[exclude_vars %in% colnames(data)] 

all_vars <- setdiff(colnames(data), exclude_vars)
categorical_vars <- setdiff(all_vars, numerical_org_vars)

# Filter out any categorical_vars that are not actually in 'data' (e.g. if all_vars was empty)
categorical_vars <- categorical_vars[categorical_vars %in% colnames(data)]

# Further ensure categorical_vars are not in numerical_org_vars
categorical_vars <- setdiff(categorical_vars, numerical_org_vars)


cat("Numerical ORG variables defined:", paste(numerical_org_vars, collapse=", "), "\n")
if (length(categorical_vars) > 0) {
  cat("Categorical variables for clustering defined:", length(categorical_vars), "variables.\n")
} else {
  cat("Warning: No categorical variables defined for clustering. K-prototypes might behave like k-means if only numerical variables are present.\n")
}


# 3. Prepare data for K-prototypes clustering -------------------------------
# kproto_analysis_data includes original ORG values and selected categorical variables
# This will be used for analysis after adding cluster assignments.
# It should contain only variables that will be used in clustering or analysis.
selected_vars_for_clustering <- c(numerical_org_vars, categorical_vars)
selected_vars_for_clustering <- selected_vars_for_clustering[selected_vars_for_clustering %in% colnames(data)] # Ensure all selected vars are in data

if (length(selected_vars_for_clustering) == 0) {
    stop("Error: No variables selected for clustering. Check variable definitions.")
}

kproto_analysis_data <- data %>%
  select(all_of(selected_vars_for_clustering))

# kproto_mixed_input_data is specifically for the kproto function input
# It has numerical ORG vars and FACTORIZED categorical vars
if (length(categorical_vars) > 0) {
  kproto_mixed_input_data <- kproto_analysis_data %>%
    mutate(across(all_of(categorical_vars), ~ ordered(round(.), levels = 1:5))) # Assuming levels 1-5 as in 031
} else {
  # If no categorical vars, kproto_mixed_input_data is same as kproto_analysis_data containing only numerical_org_vars
  kproto_mixed_input_data <- kproto_analysis_data[, numerical_org_vars, drop = FALSE]
}


cat("Prepared kproto_mixed_input_data for clustering with", 
    nrow(kproto_mixed_input_data), "rows and", 
    ncol(kproto_mixed_input_data), "columns.\n")
cat("Columns in kproto_mixed_input_data:", paste(colnames(kproto_mixed_input_data), collapse=", "), "\n")

cat("Prepared kproto_analysis_data for post-cluster analysis with",
    nrow(kproto_analysis_data), "rows and",
    ncol(kproto_analysis_data), "columns.\n")
cat("Columns in kproto_analysis_data:", paste(colnames(kproto_analysis_data), collapse=", "), "\n")


# 4. Perform K-Prototype Clustering with k=5 --------------------------------
k_fixed <- 5
set.seed(123) # For reproducibility

cat(paste0("\nPerforming K-Prototype clustering with k=", k_fixed, "...\n"))

# Check for NA/NaN/Inf in numerical columns
numerical_cols_for_kproto <- intersect(numerical_org_vars, colnames(kproto_mixed_input_data))
if (length(numerical_cols_for_kproto) > 0) {
  has_issues <- sapply(kproto_mixed_input_data[, numerical_cols_for_kproto, drop=FALSE], function(x) any(!is.finite(x) | is.na(x)))
  if (any(has_issues)) {
    warning_message <- paste("Warning: Non-finite or NA values found in numerical columns:", 
                           paste(names(has_issues[has_issues]), collapse=", "), 
                           ". Consider imputation or removal before clustering.")
    cat(warning_message, "\n")
    # Simple NA imputation with mean for numerical columns as a fallback
    for(col_name in names(has_issues[has_issues])) {
        if(is.numeric(kproto_mixed_input_data[[col_name]])) {
            mean_val <- mean(kproto_mixed_input_data[[col_name]], na.rm = TRUE)
            kproto_mixed_input_data[[col_name]][!is.finite(kproto_mixed_input_data[[col_name]]) | is.na(kproto_mixed_input_data[[col_name]])] <- mean_val
            cat(paste("Imputed NAs/Non-finite in", col_name, "with mean value:", mean_val, "\n"))
        }
    }
  }
}


# Ensure there are columns to cluster on
if (ncol(kproto_mixed_input_data) == 0) {
  stop("Error: kproto_mixed_input_data has 0 columns. Cannot perform clustering.")
}
if (nrow(kproto_mixed_input_data) < k_fixed) {
  stop(paste("Error: Number of rows (", nrow(kproto_mixed_input_data), ") is less than k (", k_fixed, "). Cannot perform clustering.", sep=""))
}


# lambda is estimated by default if not provided.
# For mixed data, clustMixType::kproto is suitable.
# If only numerical data, standard k-means might be an alternative, but kproto handles it too.
if (length(categorical_vars) > 0 && length(numerical_cols_for_kproto) > 0) {
    # Mixed data type
    kproto_result_k5 <- clustMixType::kproto(
      kproto_mixed_input_data, 
      k = k_fixed,
      verbose = FALSE # Keep terminal clean during this step
    )
} else if (length(numerical_cols_for_kproto) > 0) {
    # Only numerical data
    cat("Only numerical data found. Using k-means functionality within kproto (lambda=0 equivalent)...\n")
    # kproto can handle numeric-only data (equivalent to k-means with Gower distance on numeric part)
    # Or, one could use stats::kmeans directly. For consistency, use kproto.
    # lambda parameter influences weighting between numeric and categoric. If no categoric, it might not matter or kproto handles it.
    # The clustMixType documentation or behavior for numeric-only input should be checked.
    # Assuming kproto handles this gracefully.
     kproto_result_k5 <- clustMixType::kproto(
      kproto_mixed_input_data[, numerical_cols_for_kproto, drop=FALSE], # Ensure only numerical cols if no categorical
      k = k_fixed,
      verbose = FALSE
    )
} else if (length(categorical_vars) > 0) {
    # Only categorical data (k-modes)
    cat("Only categorical data found. Using k-modes functionality within kproto (lambda very high equivalent)...\n")
    kproto_result_k5 <- clustMixType::kproto(
      kproto_mixed_input_data[, categorical_vars, drop=FALSE], # Ensure only categorical cols
      k = k_fixed,
      verbose = FALSE
    )
} else {
    stop("Error: No numerical or categorical variables available for clustering in kproto_mixed_input_data.")
}


cat("K-Prototype clustering complete.\n")
cat("Cluster sizes:", paste(table(kproto_result_k5$cluster), collapse=", "), "\n")

# 5. Attach Cluster Assignments to Analysis Data ----------------------------
# Add cluster assignments to the data that has original ORG values
# kproto_analysis_data should have the original numerical values for ORG variables.
kproto_analysis_data_with_clusters <- kproto_analysis_data %>%
  mutate(Cluster = factor(kproto_result_k5$cluster))

cat("Cluster assignments added to analysis data.\n")

# 6. Analyze and Print ORG Variable Distributions per Cluster ---------------
cat("\n--- Analysis of ORG Variables by Cluster (k=5) ---\n")

for (org_var in numerical_org_vars) {
  # Ensure the org_var is actually in the data frame being summarized
  if (!org_var %in% colnames(kproto_analysis_data_with_clusters)) {
    cat(paste0("\nSkipping ", org_var, ": not found in kproto_analysis_data_with_clusters.\n"))
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
      .groups = 'drop' # Recommended by dplyr
    ) %>%
    # Arrange by Cluster for consistent output
    arrange(Cluster) 

  # Print the summary table
  if (requireNamespace("knitr", quietly = TRUE) && nrow(summary_stats) > 0) {
    print(knitr::kable(summary_stats, format = "pipe", digits = 2))
  } else if (nrow(summary_stats) > 0) {
    print(summary_stats)
  } else {
    cat("No summary statistics generated for", org_var, "(perhaps all NA or empty groups).\n")
  }
  cat("\n") # Add a newline for separation
}

cat("--- End of ORG Variable Analysis ---\n")
cat("\nScript 070 execution complete.\n") 