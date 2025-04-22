# R/011_create_data_dictionary.R
# Purpose: Create a comprehensive data dictionary with original variable names,
#          renamed variables, and corresponding survey questions

# 1. Load Setup ----
# This ensures necessary packages are loaded
tryCatch({
  source("R/000_setup.R")
  # Ensure required packages are loaded
  if (!requireNamespace("readr", quietly = TRUE)) install.packages("readr")
  if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
  if (!requireNamespace("tibble", quietly = TRUE)) install.packages("tibble")
  if (!requireNamespace("stringr", quietly = TRUE)) install.packages("stringr")
  library(readr)
  library(dplyr)
  library(tibble)
  library(stringr)
  cat("Setup loaded and required packages ensured.\n")
}, error = function(e) {
  stop("Error during setup: ", e$message, 
       "\nMake sure R/000_setup.R exists or manually load required packages.")
})

# 2. Define File Paths ----
raw_data_file <- "data/20230216_data export.csv"
output_file <- "data/BOAT2_CodeBook.csv"
cat("Raw data file:", raw_data_file, "\n")
cat("Output file:", output_file, "\n")

# Check if raw data file exists
if (!file.exists(raw_data_file)) {
  stop("Raw data file not found at: ", raw_data_file, 
       "\nPlease ensure the file exists in the 'data' directory.")
}

# 3. Read Data from Raw CSV ----
cat("Reading full questions (row 2)...\n")
full_questions <- NULL
variable_tags <- NULL

tryCatch({
  # Read only the second row (skip=1 means skip the first row)
  questions_row <- readr::read_csv(raw_data_file, skip = 1, n_max = 1, col_names = FALSE, show_col_types = FALSE, progress = FALSE)
  # Extract the row as a character vector
  full_questions <- as.character(questions_row[1, ])
  cat("Successfully read", length(full_questions), "potential question headers.\n")
}, error = function(e) {
  stop("Error reading questions row (row 2): ", e$message)
})

cat("Reading variable tags (row 3)...\n")
tryCatch({
  # Read only the header row (which is row 3 after skipping 2)
  header_info <- readr::read_csv(raw_data_file, skip = 2, n_max = 0, show_col_types = FALSE, progress = FALSE)
  variable_tags <- colnames(header_info)
  cat("Successfully read", length(variable_tags), "variable tags.\n")
}, error = function(e) {
  stop("Error reading header row (row 3): ", e$message)
})

# 4. Validate and Create Initial Data Dictionary ----
cat("Validating lengths...\n")
if (length(full_questions) != length(variable_tags)) {
  warning(paste("Mismatch detected between number of questions and tags!",
                "Questions found:", length(full_questions),
                "Tags found:", length(variable_tags),
                "This might indicate an issue in the raw CSV structure.",
                "Attempting to align based on the minimum length found."))
  # Align based on the minimum length to avoid errors
  min_len <- min(length(full_questions), length(variable_tags))
  full_questions <- full_questions[1:min_len]
  variable_tags <- variable_tags[1:min_len]
  cat("Aligned to minimum length:", min_len, "\n")
} else {
  cat("Lengths match successfully.\n")
}

cat("Creating initial data dictionary table...\n")
data_dictionary <- tibble::tibble(
  Original_Variable = variable_tags,
  Full_Question = full_questions
)

# Basic cleaning: remove potential leading/trailing whitespace
data_dictionary <- data_dictionary %>%
  dplyr::mutate(dplyr::across(tidyselect::everything(), ~ stringr::str_trim(gsub("[^[:print:]]", "", .))))

# 5. Clean question text to extract meaningful parts
cat("Cleaning question text to extract meaningful parts...\n")
data_dictionary <- data_dictionary %>%
  dplyr::mutate(
    # Extract the most relevant part of the question
    Clean_Question = sapply(Full_Question, function(text) {
      # Split question by dash
      parts <- strsplit(text, " - ")[[1]]
      
      # If there are enough parts, take the third part (usually the specific question)
      # Otherwise keep the original text
      if (length(parts) >= 3) {
        return(parts[3])
      } else if (length(parts) == 2) {
        return(parts[2]) 
      } else {
        return(text)
      }
    })
  )

# 6. Define variable renaming mapping ----
cat("Defining variable renaming mapping...\n")
variable_mapping <- tibble::tribble(
  ~Original_Variable, ~Renamed_Variable,
  # Organizational structure variables
  "Org_Structure_Employees", "ORG_Employees",
  "Org_Structure_Locations", "ORG_Locations",
  "Org_Structure_Depts", "ORG_Departments",
  "Org_Structure_Layers", "ORG_Layers",
  
  # Decision distribution variables
  "Distribution_Centralization1", "DIST_Athority_Dispersion",
  "Distribution_Centralization2", "DIST_Athority_Delegation",
  "Distribution_Formalization1", "DIST_Process_InformalCommunication",
  "Distribution_Formalization2", "DIST_Process_InformalProcedure",
  
  # Decision style variables
  "Style_Technocracy", "STY_DataDriven",
  "Style_Participation1", "STY_Participation_Inclusion",
  "Style_Participation2", "STY_Participation_Relational",
  "Style_Organicity1", "STY_Adaptive_Informal",
  "Style_Organicity2", "STY_Adaptive_Changeable",
  "Style_Coercion1", "STY_Authoritative_Threats",
  "Style_Coercion2", "STY_Authoritative_Compliance",
  
  # Organizational culture variables
  "Culture_Command", "CUL_Command",
  "Culture_Symbolic", "CUL_Symbolic",
  "Culture_Rationale", "CUL_Formal",
  "Culture_Generative", "CUL_Experimental",
  "Culture_Transactive", "CUL_Learning",
  
  # Decision flexibility variables
  "Flexibility_openness", "FLEX_OpenToNewIdeas",
  "Flexibility_Recursiveness", "FLEX_OpenToChanges",
  
  # Risk and environment variables
  "Risk", "RISK_Tolerance",
  "Environment_Growth", "ENV_SustainedGrowth",
  "Environment_Hostile", "ENV_HighriskIndustry",
  "Environment_Stable", "ENV_IndustryStability"
)

# 7. Define which variables are needed for analysis
needed_variables <- variable_mapping$Original_Variable

# 8. Filter dictionary to include only variables relevant for clustering analysis
cat("Filtering dictionary to include only variables relevant for clustering analysis...\n")
filtered_dictionary <- data_dictionary %>%
  dplyr::filter(Original_Variable %in% needed_variables) %>%
  dplyr::select(Original_Variable, Clean_Question) %>%
  dplyr::rename(Question = Clean_Question)

# 9. Join with variable mapping to add renamed variables
cat("Adding renamed variables to the dictionary...\n")
complete_dictionary <- filtered_dictionary %>%
  dplyr::left_join(variable_mapping, by = "Original_Variable") %>%
  # Reorder columns to put Renamed_Variable first, then Question, then Original_Variable
  dplyr::select(Renamed_Variable, Question, Original_Variable)

# Check if any mapping is missing
missing_mappings <- filtered_dictionary$Original_Variable[!filtered_dictionary$Original_Variable %in% variable_mapping$Original_Variable]
if (length(missing_mappings) > 0) {
  warning("Missing variable mappings for: ", paste(missing_mappings, collapse = ", "))
}

# 10. Save the final dictionary
cat("Creating final codebook with", nrow(complete_dictionary), "entries...\n")
cat("Preview of the final codebook:\n")
print(head(complete_dictionary))

# 11. Ensure data directory exists
data_dir <- dirname(output_file)
if (!dir.exists(data_dir)) {
  dir.create(data_dir, recursive = TRUE)
  cat("Created directory:", data_dir, "\n")
}

# 12. Write the data dictionary to CSV
cat("Writing codebook to", output_file, "...\n")
write.csv(complete_dictionary, output_file, row.names = FALSE)
cat("BOAT2 Codebook successfully created and saved!\n")