# 073 Likert Scale Radar Plots for Selected Cases
# This script generates radar plots for the Likert scale variables of:
# 1. The extreme outlier case (ORG_Employees == 3700)
# 2. The 3 cases identified in Cluster 3 from the k=5 analysis (after removing the outlier)

# 1. Load Setup and Data -----------------------------------------------------
if (file.exists("R/000_setup.R")) {
  source("R/000_setup.R")
} else {
  # Fallback for essential packages
  if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
  if (!requireNamespace("clustMixType", quietly = TRUE)) install.packages("clustMixType")
  if (!requireNamespace("fmsb", quietly = TRUE)) install.packages("fmsb")
  library(dplyr)
  library(clustMixType)
  library(fmsb)
  cat("Warning: R/000_setup.R not found. Loaded essential packages directly.\n")
}

# Ensure fmsb is loaded if setup didn't cover it
if (!("fmsb" %in% .packages())) {
  if (!requireNamespace("fmsb", quietly = TRUE)) install.packages("fmsb")
  library(fmsb)
}

# Create subdirectory for results if it doesn't exist
plot_dir <- "results/figures/073_radar_plots"
if (!dir.exists(plot_dir)) {
  dir.create(plot_dir, recursive = TRUE)
  cat(paste("Created directory:", plot_dir, "\n"))
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

# 2. Define Variables for Radar Plot and Clustering ------------------------
numerical_org_vars <- c(
  "ORG_Employees",
  "ORG_Locations",
  "ORG_Departments",
  "ORG_Layers"
)

exclude_vars <- c("Owner_Type",
                  "PDM_Selected",
                  "PDM_Experience_DBB",
                  "PDM_Experience_DB",
                  "PDM_Experience_PDB",
                  "PDM_Experience_CMAR",
                  "PDM_Experience_IPD",
                  "X") # Assuming 'X' is an original index column from read.csv, if it exists

# Ensure exclude_vars only contains columns present in data_original
exclude_vars <- exclude_vars[exclude_vars %in% colnames(data_original)]

all_analysis_vars <- setdiff(colnames(data_original), exclude_vars)
all_analysis_vars <- setdiff(all_analysis_vars, "Original_Row_ID") # Don't use ID for clustering analysis

# Categorical variables for radar plot (Likert scales)
categorical_vars_for_radar <- setdiff(all_analysis_vars, numerical_org_vars)
cat("Categorical variables for radar plots (", length(categorical_vars_for_radar), "):", paste(categorical_vars_for_radar, collapse=", "), "\n")

# 3. Isolate the Extreme Outlier Case --------------------------------------
outlier_employee_count <- 3700
case_outlier <- data_original %>%
  filter(ORG_Employees == outlier_employee_count)

if(nrow(case_outlier) == 0){
  cat(paste("Warning: Outlier case with ORG_Employees ==", outlier_employee_count, "not found.\n"))
  # Stop or handle as per requirement if outlier is critical for this script
} else if(nrow(case_outlier) > 1){
  cat(paste("Warning: Multiple outlier cases found with ORG_Employees ==", outlier_employee_count, ". Using the first one.\n"))
  case_outlier <- case_outlier[1, , drop = FALSE]
}

# 4. Isolate the Three Cases from Cluster 3 (Post-Outlier Removal) ---------
# Prepare data for clustering by removing the outlier
data_for_clustering <- data_original %>%
  filter(ORG_Employees != outlier_employee_count)
cat(paste("Dataset for clustering (outlier removed) has", nrow(data_for_clustering), "rows.\n"))

# Define variables for clustering (numerical + categorical for radar)
# Original_Row_ID should be kept for identification but not used in kproto input matrix
vars_for_kproto_input <- c(numerical_org_vars, categorical_vars_for_radar)

# Data for kproto input (numerical + factorized categorical)
kproto_mixed_input_data <- data_for_clustering %>%
  select(all_of(vars_for_kproto_input)) %>%
  mutate(across(all_of(categorical_vars_for_radar), ~ ordered(round(.), levels = 1:5)))

# Data for analysis (to attach cluster labels to, includes Original_Row_ID and original categorical values)
kproto_analysis_data_no_outlier <- data_for_clustering %>%
  select(Original_Row_ID, all_of(vars_for_kproto_input))

# Perform K-Prototype clustering
k_fixed <- 5
set.seed(123)
cat("Performing K-Prototype clustering (k=5, outlier removed) to identify Cluster 3 cases...\n")

kproto_result_no_outlier <- clustMixType::kproto(
  kproto_mixed_input_data,
  k = k_fixed,
  verbose = FALSE
)
cat("Clustering complete. Cluster sizes:", paste(table(kproto_result_no_outlier$cluster), collapse=", "), "\n")

# Attach cluster assignments
kproto_analysis_data_no_outlier$Cluster <- factor(kproto_result_no_outlier$cluster)

# Isolate cases from Cluster 3 (assuming it's the 3rd cluster as per previous run)
# Verify this assumption: the cluster with 3 members should be cluster '3'
cluster_label_for_size_3 <- names(which(table(kproto_result_no_outlier$cluster) == 3))
if (length(cluster_label_for_size_3) == 1) {
    target_cluster_id <- as.numeric(cluster_label_for_size_3)
    cat(paste("Identified cluster with 3 members as Cluster Label:", target_cluster_id, "\n"))
} else {
    cat("Warning: Could not uniquely identify cluster with 3 members. Defaulting to Cluster ID 3. Please verify results.\n")
    target_cluster_id <- 3 # Fallback, user should verify if this warning appears
}

cases_cluster3 <- kproto_analysis_data_no_outlier %>%
  filter(Cluster == target_cluster_id) 

cat(paste("Found", nrow(cases_cluster3), "cases in the target cluster (expected 3).\n"))

# Combine all cases to plot
list_of_cases_to_plot <- list()
if (nrow(case_outlier) == 1) {
  list_of_cases_to_plot[[paste0("Outlier_ID_", case_outlier$Original_Row_ID)]] <- case_outlier
}
for (i in 1:nrow(cases_cluster3)) {
  list_of_cases_to_plot[[paste0("Cluster3_Case_ID_", cases_cluster3$Original_Row_ID[i])]] <- cases_cluster3[i, , drop = FALSE]
}

# 5. Generate and Save Radar Plots -----------------------------------------
# Ensure categorical_vars_for_radar contains valid column names from the case data

for (case_name in names(list_of_cases_to_plot)) {
  current_case_data <- list_of_cases_to_plot[[case_name]]
  
  # Select only the Likert scale variables for the plot
  radar_data_values <- current_case_data[, categorical_vars_for_radar, drop = FALSE]
  
  # Check if all selected columns are numeric (or can be coerced)
  # And if they are within the 1-5 range (or can be rounded to it for plotting)
  # For simplicity, assuming they are already correct 1-5 Likert scores.
  
  if (ncol(radar_data_values) == 0) {
    cat(paste("Skipping plot for", case_name, ": No categorical variables for radar found.\n"))
    next
  }
  if (any(sapply(radar_data_values, function(x) !is.numeric(x)))){
      cat(paste("Skipping plot for", case_name, ": Non-numeric data in categorical_vars_for_radar.\n"))
      next
  }

  # Correct data frame structure for fmsb::radarchart:
  # Rows: Max, Min, Data_point_1, Data_point_2 ...
  # Columns: Variables.
  df_for_radar <- data.frame(matrix(nrow = 3, ncol = length(categorical_vars_for_radar)))
  colnames(df_for_radar) <- categorical_vars_for_radar
  df_for_radar[1, ] <- 5 # Max value for all Likert scales
  df_for_radar[2, ] <- 1 # Min value for all Likert scales
  df_for_radar[3, ] <- as.numeric(radar_data_values[1, ]) # The actual data for the case
  rownames(df_for_radar) <- c("Max", "Min", case_name) # Assigning row names

  # Define plot title and filename
  plot_title <- paste0("Radar Plot for Case: ", case_name, "\nORG_Employees: ", current_case_data$ORG_Employees)
  file_name <- file.path(plot_dir, paste0(gsub("[^A-Za-z0-9_]", "", case_name), "_radar.pdf"))
  
  cat(paste("Generating radar plot for", case_name, "and saving to", file_name, "...\n"))
  
  # Open PDF device
  pdf(file_name, width = 8, height = 8)
  
  # Generate radar chart
  tryCatch({
    fmsb::radarchart(
      df_for_radar,
      axistype = 1, # 0=no axis label, 1=center axis label, 2=around axis label, 3=radar specific label
      # Segments
      seg = 4, # Number of segments for each axis (Max-Min)/seg = 5-1 / 4 = 1 per segment
      pty = 16, # Type of point. 16 is a filled circle.
      # Polygon
      pcol = rgb(0.2, 0.5, 0.5, 0.9), # Line color
      pfcol = rgb(0.2, 0.5, 0.5, 0.4), # Fill color
      plwd = 2, # Line width
      # Grid
      cglcol = "grey", # Grid line color
      cglty = 1, # Grid line type
      axislabcol = "grey", # Axis label color
      caxislabels = seq(1, 5, 1), # Custom axis labels if needed, aligns with seg=4
      title = plot_title,
      vlcex = 0.7 # Font size of variable labels
    )
  }, error = function(e) {
    cat(paste("Error generating radarchart for", case_name, ":", e$message, "\n"))
    # Fallback: simple plot or message if radarchart fails
    plot(1, type="n", main=paste("Error in radarchart for", case_name), xlab="", ylab="")
    text(1,1, "Radarchart failed")
  })
  
  # Close PDF device
  dev.off()
  cat("Plot saved successfully.\n")
}

cat("\nScript 073 execution complete. Radar plots saved to:", plot_dir, "\n") 