# 075 Print PDM Details for Selected Cases
# This script identifies four specific cases (one outlier and three from a specific cluster)
# and prints their PDM selection and experience details.

# 1. Load Setup and Data -----------------------------------------------------
if (file.exists("R/000_setup.R")) {
  source("R/000_setup.R")
} else {
  if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
  if (!requireNamespace("clustMixType", quietly = TRUE)) install.packages("clustMixType") # For kproto logic
  library(dplyr)
  library(clustMixType)
  cat("Warning: R/000_setup.R not found. Loaded essential packages directly.\n")
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

# 2. Define PDM Variables of Interest --------------------------------------
pdm_vars_of_interest <- c(
  "PDM_Selected",
  "PDM_Experience_DBB", 
  "PDM_Experience_DB", 
  "PDM_Experience_PDB", 
  "PDM_Experience_CMAR", 
  "PDM_Experience_IPD"
)

# Check if PDM variables exist in the data
missing_pdm_vars <- pdm_vars_of_interest[!pdm_vars_of_interest %in% colnames(data_original)]
if (length(missing_pdm_vars) > 0) {
  stop(paste("Error: The following PDM variables are not found in the dataset:", paste(missing_pdm_vars, collapse=", ")))
}

# 3. Isolate the Four Selected Cases ---------------------------------------
# Case 1: Extreme Outlier
outlier_employee_count <- 3700
case_outlier <- data_original %>%
  filter(ORG_Employees == outlier_employee_count) %>%
  slice(1) # Ensure only one row if duplicates exist

cat("Identified outlier case (ORG_Employees == ", outlier_employee_count, "):
")
if(nrow(case_outlier) > 0) {
  print(case_outlier[, c("Original_Row_ID", "ORG_Employees")])
} else {
  cat("Outlier case not found. Cannot proceed with its PDM details.\n")
}

# Cases 2, 3, 4: From Cluster 3 (post-outlier removal, k=5, seed=123)
# Define variables for clustering as in previous scripts to ensure consistency
numerical_org_vars <- c("ORG_Employees", "ORG_Locations", "ORG_Departments", "ORG_Layers")
exclude_vars_clustering <- c("PDM_Selected", "PDM_Experience_DBB", "PDM_Experience_DB", 
                             "PDM_Experience_PDB", "PDM_Experience_CMAR", "PDM_Experience_IPD", "X")
exclude_vars_clustering <- exclude_vars_clustering[exclude_vars_clustering %in% colnames(data_original)]

all_categorical_vars_clustering <- setdiff(colnames(data_original), c(numerical_org_vars, exclude_vars_clustering, "Original_Row_ID"))

vars_for_kproto_input_clustering <- c(numerical_org_vars, all_categorical_vars_clustering)
vars_for_kproto_input_clustering <- vars_for_kproto_input_clustering[vars_for_kproto_input_clustering %in% colnames(data_original)]

data_for_clustering <- data_original %>%
  filter(ORG_Employees != outlier_employee_count)

kproto_mixed_input_data_clustering <- data_for_clustering %>%
  select(all_of(vars_for_kproto_input_clustering)) %>%
  mutate(across(all_of(all_categorical_vars_clustering[all_categorical_vars_clustering %in% vars_for_kproto_input_clustering]), ~ ordered(round(.), levels = 1:5)))

# We need Original_Row_ID and ORG_Employees for the analysis data frame to merge back or select from later
kproto_analysis_data_no_outlier_clustering <- data_for_clustering %>%
  select(Original_Row_ID, ORG_Employees, all_of(vars_for_kproto_input_clustering))

k_fixed_clustering <- 5
set.seed(123)

cat("\nPerforming K-Prototype clustering (k=5, outlier removed) to identify Cluster 3 cases...\n")
kproto_result_no_outlier_clustering <- clustMixType::kproto(
  kproto_mixed_input_data_clustering,
  k = k_fixed_clustering,
  verbose = FALSE
)
cat("Clustering complete. Cluster sizes:", paste(table(kproto_result_no_outlier_clustering$cluster), collapse=", "), "\n")

kproto_analysis_data_no_outlier_clustering$Cluster <- factor(kproto_result_no_outlier_clustering$cluster)

cluster_label_for_size_3_clustering <- names(which(table(kproto_result_no_outlier_clustering$cluster) == 3))[1]
target_cluster_id_clustering <- as.numeric(cluster_label_for_size_3_clustering)
cases_cluster3 <- kproto_analysis_data_no_outlier_clustering %>%
  filter(Cluster == target_cluster_id_clustering) # This gives us the IDs and clustering variables

# Now, select these cases from the *original* data to get all columns including PDM
original_ids_cluster3 <- cases_cluster3$Original_Row_ID
cases_cluster3_full_data <- data_original %>%
  filter(Original_Row_ID %in% original_ids_cluster3)

cat("\nIdentified cases from Cluster 3 (post-outlier removal, k=5):
")
print(cases_cluster3_full_data[, c("Original_Row_ID", "ORG_Employees")])

# Combine all 4 cases for PDM detail extraction
# Ensure case_outlier is not empty before trying to bind
if (nrow(case_outlier) > 0) {
  selected_cases_for_pdm <- bind_rows(
    case_outlier, # This is already from data_original, so has all columns
    cases_cluster3_full_data
  )
} else {
  # If outlier wasn't found, just use cluster 3 cases
  selected_cases_for_pdm <- cases_cluster3_full_data
  cat("\nWarning: Outlier case was not found. PDM details will only be for Cluster 3 cases.\n")
}


# 4. Select and Print PDM Details -----------------------------------------
cat("\n--- PDM Selection and Experience Details for Selected Cases ---\n")

if (nrow(selected_cases_for_pdm) > 0) {
  pdm_details_to_print <- selected_cases_for_pdm %>%
    select(Original_Row_ID, ORG_Employees, all_of(pdm_vars_of_interest))
  
  # Print the details. Using knitr::kable if available for nicer formatting.
  if (requireNamespace("knitr", quietly = TRUE)) {
    print(knitr::kable(pdm_details_to_print, format = "pipe", row.names = FALSE))
  } else {
    print(pdm_details_to_print, row.names = FALSE)
  }
} else {
  cat("No cases were selected for PDM detail printing. Please check case identification steps.\n")
}

cat("\nScript 075 execution complete.\n") 