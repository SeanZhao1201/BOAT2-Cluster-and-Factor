# BOAT2 Cluster and Factor Analysis - Data Preparation Script
# This script imports, cleans, and prepares data for analysis

# 1. Load Setup --------------------------------------------------------------
source("R/00_setup.R")

# 2. Data Import -------------------------------------------------------------
# Read raw data
raw_data <- read.csv("data/BOAT2_Data.csv")

# Add unique ID if not present
if(!"ID" %in% colnames(raw_data)) {
  raw_data$ID <- 1:nrow(raw_data)
}

# 3. Define Variable Groups --------------------------------------------------
# 3.1 Numerical organizational characteristic variables
numerical_org_vars <- c(
  "Org_Structure_Employees",
  "Org_Structure_Locations",
  "Org_Structure_Depts",
  "Org_Structure_Layers"
)

# 3.2 Single ordinal decision characteristic variables
ordinal_single_vars <- c(
  "Style_Technocracy", 
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

# 3.3 Multiple ordinal decision characteristic variable pairs
ordinal_multi_vars <- list(
  Distribution_Centralization = c("Distribution_Centralization1", "Distribution_Centralization2"),
  Distribution_Formalization = c("Distribution_Formalization1", "Distribution_Formalization2"),
  Style_Participation = c("Style_Participation1", "Style_Participation2"),
  Style_Organicity = c("Style_Organicity1", "Style_Organicity2"),
  Style_Coercion = c("Style_Coercion1", "Style_Coercion2")
)

# 3.4 All original ordinal decision characteristic variables
ordinal_all_vars <- c(
  unlist(ordinal_multi_vars),
  ordinal_single_vars
)

# 3.5 Categorical variables
categorical_vars <- c(
  "PDM_Type"
)

# 4. Data Cleaning -----------------------------------------------------------
# 4.1 Check missing values
missing_values <- colSums(is.na(raw_data))
cat("Number of missing values for each variable:\n")
print(missing_values[missing_values > 0])

# 4.2 Check outliers
par(mfrow=c(2, 2))
for(var in numerical_org_vars) {
  boxplot(raw_data[[var]], main=var, col="lightblue")
}

# 4.3 Remove extreme outliers if needed
# Check for extreme outliers in number of employees
employee_outliers <- boxplot.stats(raw_data$Org_Structure_Employees)$out
if(length(employee_outliers) > 0) {
  cat("Found outliers in Org_Structure_Employees:", employee_outliers, "\n")
  
  # Remove the extreme outlier (3700 employees)
  if(3700 %in% employee_outliers) {
    filtered_data <- raw_data %>%
      filter(Org_Structure_Employees != 3700)
    cat("Removed extreme outlier with 3700 employees\n")
  } else {
    filtered_data <- raw_data
  }
} else {
  filtered_data <- raw_data
}

# 5. Create Datasets for Different Analyses ----------------------------------
# 5.1 Create merged variable dataset (average multi-item constructs)
merged_data <- filtered_data %>%
  mutate(
    # Calculate means for multiple constructs
    Distribution_Centralization = (Distribution_Centralization1 + Distribution_Centralization2) / 2,
    Distribution_Formalization = (Distribution_Formalization1 + Distribution_Formalization2) / 2,
    Style_Participation = (Style_Participation1 + Style_Participation2) / 2,
    Style_Organicity = (Style_Organicity1 + Style_Organicity2) / 2,
    Style_Coercion = (Style_Coercion1 + Style_Coercion2) / 2
  )

# Merged ordinal decision variables
merged_ordinal_vars <- c(
  "Distribution_Centralization", 
  "Distribution_Formalization",
  "Style_Technocracy", 
  "Style_Participation", 
  "Style_Organicity", 
  "Style_Coercion",
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

# 5.2 Create dataset for hierarchical clustering (merged variables)
hierarchical_data <- merged_data %>%
  select(
    ID, 
    all_of(merged_ordinal_vars),
    PDM_Type
  )

# 5.3 Create dataset for K-prototype clustering (original variables)
kproto_data <- filtered_data %>%
  select(
    ID,
    all_of(numerical_org_vars),
    all_of(ordinal_all_vars),
    PDM_Type
  )

# 5.4 Create dataset for fuzzy clustering
fuzzy_data <- merged_data %>%
  select(
    ID,
    all_of(numerical_org_vars),
    all_of(merged_ordinal_vars),
    PDM_Type
  )

# 6. Handle Missing Values ---------------------------------------------------
# 6.1 Function to handle missing values in a dataset
handle_missing_values <- function(dataset, method = "median") {
  # Save categorical variables
  categorical <- dataset %>% select_if(is.factor)
  
  # Handle missing values in numeric variables
  numeric_data <- dataset %>% select_if(is.numeric)
  
  # Apply imputation method
  if(method == "median") {
    numeric_data <- numeric_data %>%
      mutate(across(everything(), ~ifelse(is.na(.), median(., na.rm = TRUE), .)))
  } else if(method == "mean") {
    numeric_data <- numeric_data %>%
      mutate(across(everything(), ~ifelse(is.na(.), mean(., na.rm = TRUE), .)))
  }
  
  # Combine numeric and categorical data
  result <- bind_cols(numeric_data, categorical)
  
  return(result)
}

# 6.2 Apply missing value handling to all datasets
hierarchical_data <- handle_missing_values(hierarchical_data, "mean")
kproto_data <- handle_missing_values(kproto_data, "median")
fuzzy_data <- handle_missing_values(fuzzy_data, "mean")

# 7. Save Prepared Datasets for Further Analysis -----------------------------
# Save datasets to results folder
save_data(hierarchical_data, "hierarchical_data.csv")
save_data(kproto_data, "kproto_data.csv")
save_data(fuzzy_data, "fuzzy_data.csv")

# 8. Summary Statistics ------------------------------------------------------
# Generate summary statistics
cat("\nSummary statistics for main variables:\n")
summary_stats <- summary(merged_data[, c(numerical_org_vars, merged_ordinal_vars)])
print(summary_stats)

# Project delivery method distribution
pdm_counts <- table(filtered_data$PDM_Type)
pdm_perc <- prop.table(pdm_counts) * 100

cat("\nProject Delivery Method Distribution:\n")
pdm_distribution <- data.frame(
  PDM_Type = names(pdm_counts),
  Count = as.numeric(pdm_counts),
  Percentage = as.numeric(pdm_perc)
)
print(pdm_distribution)

# Print preparation completion message
cat("\nData preparation complete!\n")
cat("Created and saved datasets for:\n")
cat("- Hierarchical clustering\n")
cat("- K-prototype clustering\n")
cat("- Fuzzy clustering\n") 