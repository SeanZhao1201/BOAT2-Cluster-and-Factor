# BOAT2 Cluster and Factor Analysis - Data Preparation Script
# This script imports, cleans, prepares, and standardizes data for analysis

# Set up output redirection to file for easier debugging
sink("data_preparation_debug.log",
     append = FALSE,
     split = TRUE)

# Add initial debug statement to verify script is running
cat("============== SCRIPT STARTING ==============\n")

# 1. Load Setup --------------------------------------------------------------
# Load packages and initialize environment
cat("Loading setup...\n")
source("R/000_setup.R")

# Debug message about fixed filtered_data issue
cat(
  "IMPORTANT DEBUG NOTE: In previous versions, 'filtered_data' was referenced but never defined.\n"
)
cat(
  "This script has been updated to use consistent dataframe naming to identify where NAs occur.\n"
)

# 2. Data Import -------------------------------------------------------------
# Read raw data from the original survey export file
cat("Importing raw data from original survey export...\n")
raw_data <- read_csv("data/20230216_data export.csv", skip = 2)
cat("Raw data dimensions:", dim(raw_data), "\n")
cat("Number of NAs in raw data:", sum(is.na(raw_data)), "\n")

# 3. Data Exploration and Cleaning -------------------------------------------
cat("Exploring and cleaning data...\n")

# 3.1 Examine column names to understand data structure
cat("First 20 column names:", paste(names(raw_data)[1:20], collapse = ", "), "\n")
cat("Column names 21-40:", paste(names(raw_data)[21:40], collapse = ", "), "\n")
cat("Column names 41-60:", paste(names(raw_data)[41:60], collapse = ", "), "\n")

# 3.2 Examine PDM Experience columns
cat("\nPDM Experience columns inspection:\n")
exp_columns <- grep("Experience_", names(raw_data), value = TRUE)
cat("Found experience columns:",
    paste(exp_columns, collapse = ", "),
    "\n")

# Print sample values for experience columns
cat("Example values for Experience columns:\n")
print(head(raw_data[, exp_columns]))

# Print unique values for each experience column
cat("\nUnique values in Experience columns:\n")
for (col in c(
  "Experience_DBB",
  "Experience_DB",
  "Experience_PDB",
  "Experience_CMR",
  "Experience_IPD"
)) {
  cat(col, "unique values:", paste(unique(na.omit(raw_data[[col]])), collapse =
                                     ", "), "\n")
}

# 3.3 Initial data cleaning: Remove irrelevant variables and empty cases
cat("\nPre-cleaning: Removing irrelevant variables and empty cases...\n")

# Remove metadata, text variables and empty columns
data_step1 <- raw_data %>%
  select(!(Start_Date:Consent)) %>%
  select(-contains("Text")) %>%
  select(-contains("Pdtext")) %>%
  select(-contains("Experience_Other")) %>%
  select(-contains("...130")) %>%
  select(-contains("...131")) %>%
  select(-contains("...132"))
cat("After removing irrelevant variables - dimensions:",
    dim(data_step1),
    "\n")
cat("Number of NAs after removing irrelevant variables:",
    sum(is.na(data_step1)),
    "\n")

# Remove all empty cases (rows)
data_step2 <- data_step1 %>%
  filter(!if_all(.fns = is.na))
cat("After removing empty cases - dimensions:", dim(data_step2), "\n")
cat("Number of NAs after removing empty cases:", sum(is.na(data_step2)), "\n")

# Remove rows where "PDM_Type" is NA (critical field)
data_step3 <- data_step2 %>%
  drop_na(PDM_Type)
cat("After removing rows with NA PDM_Type - dimensions:",
    dim(data_step3),
    "\n")
cat("Number of NAs after removing rows with NA PDM_Type:",
    sum(is.na(data_step3)),
    "\n")

# Apply 80% completion filter (retain rows with at least 80% complete data)
cat("Applying 80% completion filter...\n")
pre_cleaned_data <- data_step3[rowSums(is.na(data_step3)) <= ncol(data_step3) * 0.2, ]
cat(
  "After pre-cleaning and 80% completion filtering:",
  nrow(pre_cleaned_data),
  "rows remain\n"
)
cat("Number of NAs in pre_cleaned_data:", sum(is.na(pre_cleaned_data)), "\n")

# Print first rows to check data structure
cat("First 5 rows of pre_cleaned_data (selected columns only):\n")
print(head(pre_cleaned_data[, c(
  "PDM_Type",
  "Experience_DBB",
  "Experience_DB",
  "Experience_PDB",
  "Experience_CMR",
  "Experience_IPD"
)], 5))

# 3.4 Fix outliers and standardize values
cat("\nManually fixing outliers...\n")

# Fix Org_Structure_Depts outliers (replace "30+" with numeric value)
pre_cleaned_data_step1 <- pre_cleaned_data %>%
  mutate(Org_Structure_Depts = ifelse(Org_Structure_Depts == "30+", 30, Org_Structure_Depts))

# Fix Culture_Predictability value "6" (should be 1-5 scale)
if ("Culture_Predictability" %in% colnames(pre_cleaned_data_step1)) {
  pre_cleaned_data_step1 <- pre_cleaned_data_step1 %>%
    mutate(Culture_Predictability = ifelse(Culture_Predictability == 6, 5, Culture_Predictability))
}
cat("After manually fixing outliers - dimensions:",
    dim(pre_cleaned_data_step1),
    "\n")
cat("Number of NAs after fixing outliers:", sum(is.na(pre_cleaned_data_step1)), "\n")

# 3.5 Fix data types and convert relevant variables to numeric
pre_cleaned_data_step2 <- pre_cleaned_data_step1 %>%
  mutate(# Handle special cases for Org_Structure_Depts
    Org_Structure_Depts = as.numeric(ifelse(
      Org_Structure_Depts == "30+", 30, Org_Structure_Depts
    )))

# Define organization structure numerical variables
ORG_Num_Names <- c("ORG_Employees",
                   "ORG_Locations",
                   "ORG_Departments",
                   "ORG_Layers")

# Define decision and culture Likert scale variables
DEC_Likert_Names <- c(
  # Decision distribution variables
  "DIST_Athority_Dispersion",
  "DIST_Athority_Delegation",
  "DIST_Process_InformalCommunication",
  "DIST_Process_InformalProcedure",
  
  # Decision style variables
  "STY_DataDriven",
  "STY_Participation_Inclusion",
  "STY_Participation_Relational",
  "STY_Adaptive_Informal",
  "STY_Adaptive_Changeable",
  "STY_Authoritative_Threats",
  "STY_Authoritative_Compliance",
  
  # Organizational culture variables
  "CUL_Command",
  "CUL_Symbolic",
  "CUL_Formal",
  "CUL_Experimental",
  "CUL_Learning",
  
  # Decision flexibility variables
  "FLEX_OpenToNewIdeas",
  "FLEX_OpenToChanges",
  
  # Risk and environment variables
  "RISK_Tolerance",
  "ENV_SustainedGrowth",
  "ENV_HighriskIndustry",
  "ENV_IndustryStability"
)

# PDM experience variables
PDM_Exp_Names <- c(
  "PDM_Experience_DBB",
  "PDM_Experience_DB",
  "PDM_Experience_PDB",
  "PDM_Experience_CMAR",
  "PDM_Experience_IPD"
)

# Selectively convert relevant variables to numeric (preserving PDM_Type and Experience variables)
pre_cleaned_data_step3 <- pre_cleaned_data_step2 %>%
  mutate(
    # Organization structure variables to numeric
    Org_Structure_Employees = as.numeric(Org_Structure_Employees),
    Org_Structure_Locations = as.numeric(Org_Structure_Locations),
    Org_Structure_Layers = as.numeric(Org_Structure_Layers),
    
    # Decision distribution variables to numeric
    Distribution_Centralization1 = as.numeric(Distribution_Centralization1),
    Distribution_Centralization2 = as.numeric(Distribution_Centralization2),
    Distribution_Formalization1 = as.numeric(Distribution_Formalization1),
    Distribution_Formalization2 = as.numeric(Distribution_Formalization2),
    
    # Decision style variables to numeric
    Style_Technocracy = as.numeric(Style_Technocracy),
    Style_Participation1 = as.numeric(Style_Participation1),
    Style_Participation2 = as.numeric(Style_Participation2),
    Style_Organicity1 = as.numeric(Style_Organicity1),
    Style_Organicity2 = as.numeric(Style_Organicity2),
    Style_Coercion1 = as.numeric(Style_Coercion1),
    Style_Coercion2 = as.numeric(Style_Coercion2),
    
    # Organizational culture variables to numeric
    Culture_Command = as.numeric(Culture_Command),
    Culture_Symbolic = as.numeric(Culture_Symbolic),
    Culture_Rationale = as.numeric(Culture_Rationale),
    Culture_Generative = as.numeric(Culture_Generative),
    Culture_Transactive = as.numeric(Culture_Transactive),
    
    # Decision flexibility variables to numeric
    Flexibility_openness = as.numeric(Flexibility_openness),
    Flexibility_Recursiveness = as.numeric(Flexibility_Recursiveness),
    
    # Risk and environment variables to numeric
    Risk = as.numeric(Risk),
    Environment_Growth = as.numeric(Environment_Growth),
    Environment_Hostile = as.numeric(Environment_Hostile),
    Environment_Stable = as.numeric(Environment_Stable)
  )

cat("After fixing data types - dimensions:",
    dim(pre_cleaned_data_step3),
    "\n")
cat("Number of NAs after fixing data types:", sum(is.na(pre_cleaned_data_step3)), "\n")

# 3.6 Rename variables for better readability
pre_cleaned_data_renamed <- pre_cleaned_data_step3 %>%
  rename(
    # PDM variables
    PDM_Selected = PDM_Type,
    PDM_Experience_DBB = Experience_DBB,
    PDM_Experience_DB = Experience_DB,
    PDM_Experience_PDB = Experience_PDB,
    PDM_Experience_CMAR = Experience_CMR,
    # Correct the name
    PDM_Experience_IPD = Experience_IPD,
    
    # Organizational structure variables
    ORG_Employees = Org_Structure_Employees,
    ORG_Locations = Org_Structure_Locations,
    ORG_Departments = Org_Structure_Depts,
    ORG_Layers = Org_Structure_Layers,
    
    # Decision distribution variables
    DIST_Athority_Dispersion = Distribution_Centralization1,
    DIST_Athority_Delegation = Distribution_Centralization2,
    DIST_Process_InformalCommunication = Distribution_Formalization1,
    DIST_Process_InformalProcedure = Distribution_Formalization2,
    
    # Decision style variables
    STY_DataDriven = Style_Technocracy,
    STY_Participation_Inclusion = Style_Participation1,
    STY_Participation_Relational = Style_Participation2,
    STY_Adaptive_Informal = Style_Organicity1,
    STY_Adaptive_Changeable = Style_Organicity2,
    STY_Authoritative_Threats = Style_Coercion1,
    STY_Authoritative_Compliance = Style_Coercion2,
    
    # Organizational culture variables
    CUL_Command = Culture_Command,
    CUL_Symbolic = Culture_Symbolic,
    CUL_Formal = Culture_Rationale,
    CUL_Experimental = Culture_Generative,
    CUL_Learning = Culture_Transactive,
    
    # Decision flexibility variables
    FLEX_OpenToNewIdeas = Flexibility_openness,
    FLEX_OpenToChanges = Flexibility_Recursiveness,
    
    # Risk and environment variables
    RISK_Tolerance = Risk,
    ENV_SustainedGrowth = Environment_Growth,
    ENV_HighriskIndustry = Environment_Hostile,
    ENV_IndustryStability = Environment_Stable
  )
cat("After renaming - dimensions:",
    dim(pre_cleaned_data_renamed),
    "\n")
cat("Number of NAs after renaming:", sum(is.na(pre_cleaned_data_renamed)), "\n")

# 3.6.1 Check variable types after renaming
cat("\nChecking critical variables after renaming:\n")
cat("PDM_Selected class:",
    class(pre_cleaned_data_renamed$PDM_Selected),
    "\n")
cat(
  "PDM_Experience_DBB class:",
  class(pre_cleaned_data_renamed$PDM_Experience_DBB),
  "\n"
)
cat(
  "PDM_Experience_DB class:",
  class(pre_cleaned_data_renamed$PDM_Experience_DB),
  "\n"
)
cat(
  "PDM_Experience_PDB class:",
  class(pre_cleaned_data_renamed$PDM_Experience_PDB),
  "\n"
)
cat(
  "PDM_Experience_CMAR class:",
  class(pre_cleaned_data_renamed$PDM_Experience_CMAR),
  "\n"
)
cat(
  "PDM_Experience_IPD class:",
  class(pre_cleaned_data_renamed$PDM_Experience_IPD),
  "\n"
)

# 3.6.2 Convert PDM experience variables to numeric with specific NA handling
pre_cleaned_data_with_numeric_exp <- pre_cleaned_data_renamed %>%
  mutate(
    # DBB experience - set NA to 4
    PDM_Experience_DBB = case_when(
      PDM_Experience_DBB == "used it many times" ~ 4,
      PDM_Experience_DBB == "used it once or twice" ~ 3,
      PDM_Experience_DBB == "never used it but familiar with it" ~ 2,
      PDM_Experience_DBB == "never used it and not familiar with it" ~ 1,
      is.na(PDM_Experience_DBB) ~ 4,
      # Set NA to 4
      TRUE ~ as.numeric(PDM_Experience_DBB)  # Keep existing numeric values
    ),
    
    # DB experience - set NA to 3
    PDM_Experience_DB = case_when(
      PDM_Experience_DB == "used it many times" ~ 4,
      PDM_Experience_DB == "used it once or twice" ~ 3,
      PDM_Experience_DB == "never used it but familiar with it" ~ 2,
      PDM_Experience_DB == "never used it and not familiar with it" ~ 1,
      is.na(PDM_Experience_DB) ~ 3,
      # Set NA to 3
      TRUE ~ as.numeric(PDM_Experience_DB)  # Keep existing numeric values
    ),
    
    # PDB experience - set NA to 2
    PDM_Experience_PDB = case_when(
      PDM_Experience_PDB == "used it many times" ~ 4,
      PDM_Experience_PDB == "used it once or twice" ~ 3,
      PDM_Experience_PDB == "never used it but familiar with it" ~ 2,
      PDM_Experience_PDB == "never used it and not familiar with it" ~ 1,
      is.na(PDM_Experience_PDB) ~ 2,
      # Set NA to 2
      TRUE ~ as.numeric(PDM_Experience_PDB)  # Keep existing numeric values
    ),
    
    # CMAR experience - set NA to 4
    PDM_Experience_CMAR = case_when(
      PDM_Experience_CMAR == "used it many times" ~ 4,
      PDM_Experience_CMAR == "used it once or twice" ~ 3,
      PDM_Experience_CMAR == "never used it but familiar with it" ~ 2,
      PDM_Experience_CMAR == "never used it and not familiar with it" ~ 1,
      is.na(PDM_Experience_CMAR) ~ 4,
      # Set NA to 4
      TRUE ~ as.numeric(PDM_Experience_CMAR)  # Keep existing numeric values
    ),
    
    # IPD experience - set NA to 2
    PDM_Experience_IPD = case_when(
      PDM_Experience_IPD == "used it many times" ~ 4,
      PDM_Experience_IPD == "used it once or twice" ~ 3,
      PDM_Experience_IPD == "never used it but familiar with it" ~ 2,
      PDM_Experience_IPD == "never used it and not familiar with it" ~ 1,
      is.na(PDM_Experience_IPD) ~ 2,
      # Set NA to 2
      TRUE ~ as.numeric(PDM_Experience_IPD)  # Keep existing numeric values
    )
  )

cat(
  "After converting experience variables to numeric - dimensions:",
  dim(pre_cleaned_data_with_numeric_exp),
  "\n"
)
cat("Number of NAs after converting experience to numeric:",
    sum(is.na(pre_cleaned_data_with_numeric_exp)),
    "\n")

# Check PDM experience values after conversion
cat("\nChecking PDM experience variables after conversion:\n")
for (col in PDM_Exp_Names) {
  cat(col, "summary:\n")
  print(table(pre_cleaned_data_with_numeric_exp[[col]], useNA = "ifany"))
}

# 3.6.3 Impute remaining NAs with median values
cat("\nImputing remaining NAs with column median values...\n")

# Create a function to impute NAs with median values
impute_median <- function(x) {
  if (is.numeric(x)) {
    median_val <- median(x, na.rm = TRUE)
    x[is.na(x)] <- median_val
    return(x)
  } else {
    return(x)  # If not numeric, return as is
  }
}

# Actively select columns to impute - organization structure + decision/culture Likert variables
cols_to_impute <- c(ORG_Num_Names, DEC_Likert_Names)

# Apply median imputation
pre_cleaned_data_imputed <- pre_cleaned_data_with_numeric_exp %>%
  mutate(across(all_of(cols_to_impute), impute_median))

# Check NA count before and after imputation
cat("NA count before imputation:", sum(is.na(pre_cleaned_data_with_numeric_exp)), "\n")
cat("NA count after imputation:", sum(is.na(pre_cleaned_data_imputed)), "\n")

# Export the pre-cleaned data with numeric experience values
write_csv(pre_cleaned_data_imputed, "data/pre_cleaned_data.csv")

# 3.7 Extract and prepare necessary columns for analysis
cat("\nExtracting columns needed for analysis...\n")

# Use our defined variable groups to select columns
cleaned_data_selected <- pre_cleaned_data_imputed %>%
  select(
    # PDM type
    PDM_Selected,
    
    # PDM experience variables
    all_of(PDM_Exp_Names),
    
    # Organization structure variables
    all_of(ORG_Num_Names),
    
    # Decision and culture Likert scale variables
    all_of(DEC_Likert_Names)
  )
cat("After selecting relevant columns - dimensions:",
    dim(cleaned_data_selected),
    "\n")
cat("Number of NAs after selecting columns:", sum(is.na(cleaned_data_selected)), "\n")

# 3.8 Show column names after selection
cat("\nCleaned data columns:", paste(names(cleaned_data_selected), collapse =
                                       ", "), "\n")

# 3.9 Check values for PDM_Experience columns
cat("\nValues for PDM Experience columns after cleaning:\n")
for (col in PDM_Exp_Names) {
  cat(col, "summary:\n")
  print(table(cleaned_data_selected[[col]], useNA = "ifany"))
}

# 3.10 Clean PDM_Selected values (standardize text formats)
cleaned_data_pdm_fixed <- cleaned_data_selected %>%
  mutate(
    PDM_Selected = str_replace_all(
      PDM_Selected,
      " - best value| - low bid| - procurement unknown| \\(If contract model and pricing structure not listed, please specify\\)",
      ""
    ),
    PDM_Selected = str_replace(PDM_Selected, "Other", "Integrated Project Delivery (IPD)")
  )
cat("After cleaning PDM_Selected values - dimensions:",
    dim(cleaned_data_pdm_fixed),
    "\n")
cat("Number of NAs after cleaning PDM values:", sum(is.na(cleaned_data_pdm_fixed)), "\n")

# 3.11 Print PDM type distribution to verify
cat("\nPDM Type distribution after cleaning:\n")
print(table(cleaned_data_pdm_fixed$PDM_Selected))

# Final cleaned dataset
cleaned_data <- cleaned_data_pdm_fixed
cat("\nFinal cleaned data dimensions:", dim(cleaned_data), "\n")
cat("Number of NAs in final cleaned data:", sum(is.na(cleaned_data)), "\n")

# 4. Create Datasets for Different Analyses ----------------------------------
cat("Creating datasets for analysis...\n")

# 4.1 Create main enhanced dataset with PDM_Experience variables
boat2_data_enhanced <- cleaned_data
cat("Enhanced dataset dimensions:", dim(boat2_data_enhanced), "\n")
cat("Number of NAs in enhanced dataset:", sum(is.na(boat2_data_enhanced)), "\n")

# 5. Save Prepared Datasets for Further Analysis -----------------------------
cat("Saving datasets to data folder...\n")

# Define a function to save data to the data folder
save_to_data_folder <- function(data, filename) {
  output_path <- file.path("data", filename)
  write.csv(data, output_path, row.names = FALSE)
  cat("Saved data to:", output_path, "\n")
}

# Save the enhanced dataset
save_to_data_folder(boat2_data_enhanced, "BOAT2_Data_Enhanced.csv")

# 6. Summary Statistics ------------------------------------------------------
cat("\n============ SUMMARY STATISTICS BY VARIABLE GROUPS ============\n")

# 6.1 Generate summary statistics for PDM_Experience variables
cat("\n1) PDM Experience Variables Summary:\n")
pdm_exp_summary <- summary(boat2_data_enhanced[, PDM_Exp_Names])
print(pdm_exp_summary)

# 6.2 Generate summary for organizational structure variables
cat("\n2) Organizational Structure Variables Summary:\n")
org_structure_summary <- summary(boat2_data_enhanced[, ORG_Num_Names])
print(org_structure_summary)

# 6.3 Generate summary for decision distribution variables
cat("\n3) Decision Distribution Variables Summary:\n")
decision_dist_vars <- DEC_Likert_Names[1:4]  # First 4 are decision distribution variables
decision_dist_summary <- summary(boat2_data_enhanced[, decision_dist_vars])
print(decision_dist_summary)

# 6.4 Generate summary for decision style variables
cat("\n4) Decision Style Variables Summary:\n")
decision_style_vars <- DEC_Likert_Names[5:11]  # Positions 5-11 are decision style variables
decision_style_summary <- summary(boat2_data_enhanced[, decision_style_vars])
print(decision_style_summary)

# 6.5 Generate summary for culture variables
cat("\n5) Organizational Culture Variables Summary:\n")
culture_vars <- DEC_Likert_Names[12:16]  # Positions 12-16 are organizational culture variables
culture_summary <- summary(boat2_data_enhanced[, culture_vars])
print(culture_summary)

# 6.6 Generate summary for flexibility, risk and environment variables
cat("\n6) Flexibility, Risk and Environment Variables Summary:\n")
flex_risk_env_vars <- DEC_Likert_Names[17:22]  # Positions 17-22 are flexibility, risk and environment variables
flex_risk_env_summary <- summary(boat2_data_enhanced[, flex_risk_env_vars])
print(flex_risk_env_summary)

# Project delivery method distribution
pdm_counts <- table(boat2_data_enhanced$PDM_Selected)
pdm_perc <- prop.table(pdm_counts) * 100

cat("\n7) Project Delivery Method Selected Distribution:\n")
pdm_selected_distribution <- data.frame(
  PDM_Selected = names(pdm_counts),
  Count = as.numeric(pdm_counts),
  Percentage = as.numeric(pdm_perc)
)
print(pdm_selected_distribution)

# 6.7 Check variable correlations within groups
cat("\n8) Correlation within PDM Experience variables:\n")
pdm_exp_cor <- cor(boat2_data_enhanced[, PDM_Exp_Names], use = "pairwise.complete.obs")
print(round(pdm_exp_cor, 2))

cat("\n9) Correlation within Organizational Structure variables:\n")
org_structure_cor <- cor(boat2_data_enhanced[, ORG_Num_Names], use = "pairwise.complete.obs")
print(round(org_structure_cor, 2))

# Print preparation completion message
cat("\nData preparation complete!\n")
cat("Created and saved enhanced BOAT2 dataset with PDM experience variables in data folder\n")

# Close the sink to ensure log is written
sink()