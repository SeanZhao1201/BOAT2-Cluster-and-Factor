# BOAT2 Cluster and Factor Analysis - Data Preparation Script
# This script imports, cleans, and prepares data for analysis

# 1. Load Setup --------------------------------------------------------------
source("R/00_setup.R")

# 2. Data Import -------------------------------------------------------------
# Read raw data from the original survey export file
cat("Importing raw data from original survey export...\n")
raw_data <- read_csv("data/20230216_data export.csv", skip = 2)

# 3. Data Exploration and Cleaning -------------------------------------------
cat("Exploring and cleaning data...\n")

# 3.1 Check column names to understand the structure
cat("First 20 column names:", paste(names(raw_data)[1:20], collapse=", "), "\n")
cat("Column names 21-40:", paste(names(raw_data)[21:40], collapse=", "), "\n")
cat("Column names 41-60:", paste(names(raw_data)[41:60], collapse=", "), "\n")

# 3.2 Examine PDM Experience columns
cat("\nPDM Experience columns inspection:\n")
exp_columns <- grep("Experience_", names(raw_data), value = TRUE)
cat("Found experience columns:", paste(exp_columns, collapse=", "), "\n")

# Print first few rows of these columns to see values
cat("Example values for Experience columns:\n")
print(head(raw_data[, exp_columns]))

# Print unique values for each experience column to understand the format
cat("\nUnique values in Experience columns:\n")
for(col in c("Experience_DBB", "Experience_DB", "Experience_PDB", "Experience_CMR", "Experience_IPD")) {
  cat(col, "unique values:", paste(unique(na.omit(raw_data[[col]])), collapse=", "), "\n")
}

# 3.3 Extract and prepare necessary columns based on exploration
cleaned_data <- raw_data %>%
  # Select only relevant columns using actual column names from the printout
  select(
    # PDM variables
    PDM_Type,
    Experience_DBB, 
    Experience_DB, 
    Experience_PDB, 
    Experience_CMR,  # Note: This is CMR not CMAR in the original data
    Experience_IPD,
    
    # Organizational structure variables
    Org_Structure_Employees,
    Org_Structure_Locations,
    Org_Structure_Depts,
    Org_Structure_Layers,
    
    # Distribution variables
    Distribution_Centralization1,
    Distribution_Centralization2,
    Distribution_Formalization1,
    Distribution_Formalization2,
    
    # Style variables
    Style_Technocracy,
    Style_Participation1,
    Style_Participation2,
    Style_Organicity1,
    Style_Organicity2,
    Style_Coercion1,
    Style_Coercion2,
    
    # Culture variables
    Culture_Command,
    Culture_Symbolic,
    Culture_Rationale,
    Culture_Generative,
    Culture_Transactive,
    
    # Flexibility, risk and environment variables
    Flexibility_openness,
    Flexibility_Recursiveness,
    Risk,
    Environment_Growth,
    Environment_Hostile,
    Environment_Stable
  )

# 3.4 Rename columns to match our expected format
cleaned_data <- cleaned_data %>%
  rename(
    PDM_Selected = PDM_Type,
    PDM_Experience_DBB = Experience_DBB,
    PDM_Experience_DB = Experience_DB, 
    PDM_Experience_PDB = Experience_PDB,
    PDM_Experience_CMAR = Experience_CMR,  # Correct the name
    PDM_Experience_IPD = Experience_IPD
  )

# 3.5 Show column names after renaming
cat("\nCleaned data columns:", paste(names(cleaned_data), collapse=", "), "\n")

# 3.6 Check values for PDM_Experience columns
cat("\nValues for PDM Experience columns after cleaning:\n")
cat("PDM_Experience_DBB summary:\n")
print(table(cleaned_data$PDM_Experience_DBB, useNA = "ifany"))
cat("PDM_Experience_DB summary:\n")
print(table(cleaned_data$PDM_Experience_DB, useNA = "ifany"))
cat("PDM_Experience_PDB summary:\n")
print(table(cleaned_data$PDM_Experience_PDB, useNA = "ifany"))
cat("PDM_Experience_CMAR summary:\n")
print(table(cleaned_data$PDM_Experience_CMAR, useNA = "ifany"))
cat("PDM_Experience_IPD summary:\n")
print(table(cleaned_data$PDM_Experience_IPD, useNA = "ifany"))

# 3.7 Remove empty cases
cleaned_data <- cleaned_data %>%
  filter(!if_all(.fns = is.na))

# 3.8 Clean PDM_Selected values
cleaned_data <- cleaned_data %>%
  mutate(
    PDM_Selected = str_replace_all(PDM_Selected, 
                                " - best value| - low bid| - procurement unknown| \\(If contract model and pricing structure not listed, please specify\\)", 
                                ""),
    PDM_Selected = str_replace(PDM_Selected, "Other", "Integrated Project Delivery (IPD)")
  )

# 3.9 Filter rows with PDM_Selected not NA
filtered_data <- cleaned_data %>%
  drop_na(PDM_Selected)

# 3.10 Print PDM type distribution to verify
cat("\nPDM Type distribution after cleaning:\n")
print(table(filtered_data$PDM_Selected))

# 3.11 Convert Experience variables to numeric, handling the actual text values found in the data
filtered_data <- filtered_data %>%
  mutate(
    # Map text experience values to numerical scale (5 highest, 1 lowest)
    PDM_Experience_DBB = case_when(
      PDM_Experience_DBB == "used it many times" ~ 5,
      PDM_Experience_DBB == "used it once or twice" ~ 3,
      PDM_Experience_DBB == "never used it but familiar with it" ~ 2,
      PDM_Experience_DBB == "never used it and not familiar with it" ~ 1,
      is.na(PDM_Experience_DBB) ~ NA_real_,
      TRUE ~ as.numeric(PDM_Experience_DBB)
    ),
    
    PDM_Experience_DB = case_when(
      PDM_Experience_DB == "used it many times" ~ 5,
      PDM_Experience_DB == "used it once or twice" ~ 3,
      PDM_Experience_DB == "never used it but familiar with it" ~ 2,
      PDM_Experience_DB == "never used it and not familiar with it" ~ 1,
      is.na(PDM_Experience_DB) ~ NA_real_,
      TRUE ~ as.numeric(PDM_Experience_DB)
    ),
    
    PDM_Experience_PDB = case_when(
      PDM_Experience_PDB == "used it many times" ~ 5,
      PDM_Experience_PDB == "used it once or twice" ~ 3,
      PDM_Experience_PDB == "never used it but familiar with it" ~ 2,
      PDM_Experience_PDB == "never used it and not familiar with it" ~ 1,
      is.na(PDM_Experience_PDB) ~ NA_real_,
      TRUE ~ as.numeric(PDM_Experience_PDB)
    ),
    
    PDM_Experience_CMAR = case_when(
      PDM_Experience_CMAR == "used it many times" ~ 5,
      PDM_Experience_CMAR == "used it once or twice" ~ 3,
      PDM_Experience_CMAR == "never used it but familiar with it" ~ 2,
      PDM_Experience_CMAR == "never used it and not familiar with it" ~ 1,
      is.na(PDM_Experience_CMAR) ~ NA_real_,
      TRUE ~ as.numeric(PDM_Experience_CMAR)
    ),
    
    PDM_Experience_IPD = case_when(
      PDM_Experience_IPD == "used it many times" ~ 5,
      PDM_Experience_IPD == "used it once or twice" ~ 3,
      PDM_Experience_IPD == "never used it but familiar with it" ~ 2,
      PDM_Experience_IPD == "never used it and not familiar with it" ~ 1,
      is.na(PDM_Experience_IPD) ~ NA_real_,
      TRUE ~ as.numeric(PDM_Experience_IPD)
    )
  )

# 3.12 Print summary of PDM Experience after conversion
cat("\nPDM Experience summary after conversion to numerical values:\n")
cat("PDM_Experience_DBB summary:\n")
print(table(filtered_data$PDM_Experience_DBB, useNA = "ifany"))
cat("PDM_Experience_DB summary:\n")
print(table(filtered_data$PDM_Experience_DB, useNA = "ifany"))
cat("PDM_Experience_PDB summary:\n")
print(table(filtered_data$PDM_Experience_PDB, useNA = "ifany"))
cat("PDM_Experience_CMAR summary:\n")
print(table(filtered_data$PDM_Experience_CMAR, useNA = "ifany"))
cat("PDM_Experience_IPD summary:\n")
print(table(filtered_data$PDM_Experience_IPD, useNA = "ifany"))

# 3.13 Filter for 80% completion
filtered_data <- filtered_data %>%
  filter(rowSums(is.na(filtered_data)) <= ncol(filtered_data) * 0.2)

# 3.14 Fix data types and outliers
filtered_data <- filtered_data %>%
  mutate(
    # Handle special cases for Org_Structure_Depts
    Org_Structure_Depts = as.numeric(ifelse(Org_Structure_Depts == "30+", 30, Org_Structure_Depts))
  )

# Convert all remaining variables to numeric
filtered_data <- filtered_data %>%
  mutate(across(where(is.character) & !matches("PDM_Selected"), as.numeric))

# 4. Handle Missing Values ---------------------------------------------------
cat("Handling missing values...\n")

handle_missing_values <- function(dataset, method = "median") {
  # Save PDM_Selected and PDM_Experience columns
  pdm_columns <- dataset %>% 
    select(PDM_Selected, starts_with("PDM_Experience"))
  
  # Process numeric variables
  numeric_data <- dataset %>% 
    select(-PDM_Selected, -starts_with("PDM_Experience")) %>%
    select_if(is.numeric)
  
  # Apply imputation method
  if(method == "median") {
    numeric_data <- numeric_data %>%
      mutate(across(everything(), ~ifelse(is.na(.), median(., na.rm = TRUE), .)))
  } else if(method == "mean") {
    numeric_data <- numeric_data %>%
      mutate(across(everything(), ~ifelse(is.na(.), mean(., na.rm = TRUE), .)))
  }
  
  # Now handle PDM_Experience columns (using either method)
  if(method == "median") {
    exp_cols <- pdm_columns %>% select(starts_with("PDM_Experience"))
    
    # Only impute if there are non-NA values to calculate median from
    for(col in names(exp_cols)) {
      if(sum(!is.na(exp_cols[[col]])) > 0) {
        # Compute median from non-NA values
        med_val <- median(exp_cols[[col]], na.rm = TRUE)
        # Replace NA values with median
        pdm_columns[[col]] <- ifelse(is.na(pdm_columns[[col]]), med_val, pdm_columns[[col]])
      } else {
        # If all values are NA, use 3 as default (middle of 1-5 scale)
        pdm_columns[[col]] <- 3
      }
    }
  }
  
  # Merge data back together
  result <- bind_cols(numeric_data, pdm_columns)
  
  return(result)
}

processed_data <- handle_missing_values(filtered_data, "median")

# 5. Create Default Variables If Missing ------------------------------------
# Ensure all required variables exist, if not create them with median values
required_vars <- c(
  "Org_Structure_Employees", "Org_Structure_Locations", "Org_Structure_Depts", "Org_Structure_Layers",
  "Distribution_Centralization1", "Distribution_Centralization2", 
  "Distribution_Formalization1", "Distribution_Formalization2",
  "Style_Technocracy", "Style_Participation1", "Style_Participation2", 
  "Style_Organicity1", "Style_Organicity2", "Style_Coercion1", "Style_Coercion2",
  "Culture_Command", "Culture_Symbolic", "Culture_Rationale", "Culture_Generative", "Culture_Transactive",
  "Flexibility_openness", "Flexibility_Recursiveness", "Risk",
  "Environment_Growth", "Environment_Hostile", "Environment_Stable"
)

for(var in required_vars) {
  if(!(var %in% colnames(processed_data))) {
    cat("Warning: Missing variable", var, "- creating it with median value 3\n")
    processed_data[[var]] <- 3
  }
}

# 6. Create Datasets for Different Analyses ----------------------------------
cat("Creating datasets for analysis...\n")

# 6.1 Create main enhanced dataset with PDM_Experience variables
boat2_data_enhanced <- processed_data %>%
  select(
    # Original organizational variables
    Org_Structure_Employees,
    Org_Structure_Locations,
    Org_Structure_Depts,
    Org_Structure_Layers,
    
    # Original decision-making variables
    Distribution_Centralization1,
    Distribution_Centralization2,
    Distribution_Formalization1,
    Distribution_Formalization2,
    Style_Technocracy,
    Style_Participation1,
    Style_Participation2,
    Style_Organicity1,
    Style_Organicity2,
    Style_Coercion1,
    Style_Coercion2,
    Culture_Command,
    Culture_Symbolic,
    Culture_Rationale,
    Culture_Generative,
    Culture_Transactive,
    Flexibility_openness,
    Flexibility_Recursiveness,
    Risk,
    Environment_Growth,
    Environment_Hostile,
    Environment_Stable,
    
    # PDM variables
    PDM_Selected,
    PDM_Experience_DBB,
    PDM_Experience_DB,
    PDM_Experience_PDB,
    PDM_Experience_CMAR,
    PDM_Experience_IPD
  )

# 6.2 Create dataset for hierarchical clustering
hierarchical_data <- boat2_data_enhanced %>%
  select(-starts_with("PDM_Experience")) %>%
  rename(PDM_Type = PDM_Selected)  # Rename to maintain compatibility with existing code

# 6.3 Create dataset for K-prototype clustering
kproto_data <- boat2_data_enhanced %>%
  rename(PDM_Type = PDM_Selected)  # Rename to maintain compatibility with existing code

# 6.4 Create dataset for fuzzy clustering
fuzzy_data <- boat2_data_enhanced %>%
  rename(PDM_Type = PDM_Selected)  # Rename to maintain compatibility with existing code

# 7. Save Prepared Datasets for Further Analysis -----------------------------
cat("Saving datasets to data folder...\n")

# Define a function to save data to the data folder instead of results/tables
save_to_data_folder <- function(data, filename) {
  output_path <- file.path("data", filename)
  write.csv(data, output_path, row.names = FALSE)
  cat("Saved data to:", output_path, "\n")
}

# Save the enhanced dataset
save_to_data_folder(boat2_data_enhanced, "BOAT2_Data_Enhanced.csv")

# Save the original format dataset (for backward compatibility)
boat2_data_original <- boat2_data_enhanced %>%
  rename(PDM_Type = PDM_Selected) %>%
  select(-starts_with("PDM_Experience"))
save_to_data_folder(boat2_data_original, "BOAT2_Data.csv")

# Save datasets for specific analysis methods
save_to_data_folder(hierarchical_data, "hierarchical_data.csv")
save_to_data_folder(kproto_data, "kproto_data.csv")
save_to_data_folder(fuzzy_data, "fuzzy_data.csv")

# 8. Summary Statistics ------------------------------------------------------
# Generate summary statistics for PDM_Experience variables
cat("\nSummary statistics for PDM experience variables:\n")
pdm_exp_summary <- summary(boat2_data_enhanced[, c(
  "PDM_Experience_DBB", 
  "PDM_Experience_DB", 
  "PDM_Experience_PDB", 
  "PDM_Experience_CMAR", 
  "PDM_Experience_IPD"
)])
print(pdm_exp_summary)

# Project delivery method distribution
pdm_counts <- table(boat2_data_enhanced$PDM_Selected)
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
cat("Created and saved enhanced BOAT2 dataset with PDM experience variables in data folder\n")
cat("Also created and saved datasets for:\n")
cat("- Hierarchical clustering\n")
cat("- K-prototype clustering\n")
cat("- Fuzzy clustering\n") 