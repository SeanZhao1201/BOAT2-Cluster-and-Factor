# BOAT2 Cluster and Factor Analysis - Exploring Extreme Value Treatment for ORG Variables
# This script explores methods to handle extreme values in organizational variables
# to potentially improve clustering outcomes.

# 1. Load Setup and Data -----------------------------------------------------
# Attempt to source the setup file, handle error if not found
tryCatch({
  source("R/000_setup.R")
  cat("Successfully sourced R/000_setup.R\n")
}, error = function(e) {
  cat("Error sourcing R/000_setup.R: ", e$message, "\n")
  cat("Please ensure R/000_setup.R is in the correct path and loads necessary packages like 'dplyr'.\n")
  cat("Attempting to load 'dplyr' manually for this script to proceed.\n")
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    install.packages("dplyr")
  }
  library(dplyr)
})

# Load the enhanced dataset
data_path <- "data/BOAT2_Data_Enhanced.csv"
if (file.exists(data_path)) {
  data <- read.csv(data_path)
  cat("Loaded dataset with", nrow(data), "rows and", ncol(data), "columns from:", data_path, "\n")
  cat("Column names:", paste(head(colnames(data), 10), collapse=", "), "...

")
} else {
  stop("Error: Data file not found at ", data_path)
}

# Make a copy of the data for modification in this script
data_modified <- data

# 2. Define ORG Variables and Explore Extreme Value Treatment --------------

# Define numerical organizational variables
# These are the variables where we observed potential issues with extreme values
org_vars <- c(
  "ORG_Employees",
  "ORG_Locations",
  "ORG_Departments",
  "ORG_Layers"
)

# Filter for org_vars that actually exist in the dataset
org_vars_exist <- org_vars[org_vars %in% colnames(data_modified)]
if (length(org_vars_exist) < length(org_vars)) {
  cat("Warning: Some defined ORG variables were not found in the dataset.\n")
  cat("Found:", paste(org_vars_exist, collapse=", "), "\n")
}
if (length(org_vars_exist) == 0) {
  stop("Error: None of the specified ORG variables found in the dataset. Please check column names.")
}

cat("--- Exploring Extreme Value Treatment for ORG Variables ---\n")

# Loop through each organizational variable to analyze and treat extremes
for (var_name in org_vars_exist) {
  cat("\n--- Variable:", var_name, "---\n")
  
  # Ensure the column is numeric
  if (!is.numeric(data_modified[[var_name]])) {
    cat("Warning: Column", var_name, "is not numeric. Skipping treatment.\n")
    next
  }
  
  # Original Summary Statistics
  cat("Original Summary Statistics for", var_name, ":\n")
  print(summary(data_modified[[var_name]]))
  cat("Original Standard Deviation:", sd(data_modified[[var_name]], na.rm = TRUE), "\n")
  
  # Define capping threshold (e.g., 99th percentile)
  # This means any value above the 99th percentile will be set to the value at the 99th percentile.
  # Using a high percentile like 99th is a common starting point.
  # You could also consider 95th or other domain-specific values.
  cap_percentile <- 0.99
  cap_value <- quantile(data_modified[[var_name]], cap_percentile, na.rm = TRUE)
  
  cat(paste0("\nProposed Capping: Values above the ", cap_percentile * 100,
             "th percentile (", round(cap_value, 2), ") will be capped.\n"))
  
  # Identify values to be capped
  values_to_cap <- data_modified[[var_name]] > cap_value & !is.na(data_modified[[var_name]])
  num_capped <- sum(values_to_cap)
  
  if (num_capped > 0) {
    cat("Number of values to be capped in", var_name, ":", num_capped, "\n")
    # Display some of the original values that will be capped (e.g., top 5 extreme values)
    cat("Original extreme values (examples):\n")
    print(head(sort(data_modified[[var_name]][values_to_cap], decreasing = TRUE), 5))
    
    # Apply capping
    data_modified[[var_name]][values_to_cap] <- cap_value
    cat("Capping applied.\n")
  } else {
    cat("No values above the capping threshold in", var_name, ". No capping applied.\n")
  }
  
  # Summary Statistics After Capping
  cat("\nSummary Statistics for", var_name, "AFTER Capping (if any):\n")
  print(summary(data_modified[[var_name]]))
  cat("Standard Deviation AFTER Capping:", sd(data_modified[[var_name]], na.rm = TRUE), "\n")
}

# 3. Further Analysis Considerations -----------------------------------------
cat("\n\n--- Further Analysis Considerations ---\n")
cat("The 'data_modified' dataframe in this script's environment now contains the capped ORG variables.\n")
cat("You can now proceed with analyses using this 'data_modified' dataframe.\n")
cat("For example, you could re-run parts of your k-prototype optimal number script (031) using this modified data to see if cluster formation improves.\n")
cat("Considerations for choosing the capping percentile (e.g., 0.99, 0.95):
")
cat(" - Domain knowledge: Are there known limits beyond which data is likely an error or extreme outlier?
")
cat(" - Distribution shape: Heavily skewed distributions might need more aggressive capping or transformations.
")
cat(" - Impact on results: Experiment with different levels and observe changes in clustering stability/interpretability.
")
cat("Alternative methods to capping include:
")
cat(" - Log transformation (if appropriate for the variable type and interpretation).
")
cat(" - Removing rows (if outliers are confirmed errors and not too numerous to cause significant data loss).
")
cat(" - Using robust clustering algorithms that are less sensitive to outliers.
")

# Example: Show summary of one modified ORG variable
if ("ORG_Employees" %in% org_vars_exist) {
  cat("\nExample: Final summary for ORG_Employees after potential capping:\n")
  print(summary(data_modified$ORG_Employees))
}

# Note: This script primarily prints to the console. 
# To use the modified data in other scripts, you would typically save it to a new CSV, for example:
# write.csv(data_modified, "data/BOAT2_Data_Enhanced_ORG_Capped.csv", row.names = FALSE)
# cat("\nIf you wish to save the modified data, uncomment the write.csv line above and specify a suitable path.\n")

cat("\nExploration of extreme value treatment for ORG variables complete.\n")
cat("Review the console output to see the impact of capping.\n") 