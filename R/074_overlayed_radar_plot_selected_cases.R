# 074 Overlayed Radar Plots for Selected Cases (Grouped Variables)
# This script generates overlayed radar plots for two groups of Likert scale variables,
# comparing four specific cases: 
# 1. The extreme outlier case (ORG_Employees == 3700)
# 2. The 3 cases identified in Cluster 3 from the k=5 analysis (after removing the outlier)

# 1. Load Setup and Data -----------------------------------------------------
if (file.exists("R/000_setup.R")) {
  source("R/000_setup.R")
} else {
  if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
  if (!requireNamespace("clustMixType", quietly = TRUE)) install.packages("clustMixType")
  if (!requireNamespace("fmsb", quietly = TRUE)) install.packages("fmsb")
  if (!requireNamespace("RColorBrewer", quietly = TRUE)) install.packages("RColorBrewer")
  library(dplyr)
  library(clustMixType)
  library(fmsb)
  library(RColorBrewer)
  cat("Warning: R/000_setup.R not found. Loaded essential packages directly.\n")
}

# Ensure fmsb and RColorBrewer are loaded
if (!("fmsb" %in% .packages())) library(fmsb)
if (!("RColorBrewer" %in% .packages())) library(RColorBrewer)

# Create subdirectory for results
plot_dir <- "results/figures/074_overlayed_radar_plots"
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

data_original <- data_original %>%
  mutate(Original_Row_ID = 1:n())

# 2. Define Variable Groups (from R/034_radar_charts.R) --------------------
# Define Group 1: Decision and Style variables (Likert scale)
group1_vars <- c(
  "DIST_Athority_Dispersion", "DIST_Athority_Delegation",
  "DIST_Process_InformalCommunication", "DIST_Process_InformalProcedure",
  "STY_DataDriven", "STY_Participation_Inclusion", "STY_Participation_Relational",
  "STY_Adaptive_Informal", "STY_Adaptive_Changeable", 
  "STY_Authoritative_Threats", "STY_Authoritative_Compliance"
)

# Define Group 2: Culture, Flexibility, Risk and Environment variables (Likert scale)
group2_vars <- c(
  "CUL_Command", "CUL_Symbolic", "CUL_Formal", "CUL_Experimental", "CUL_Learning",
  "FLEX_OpenToNewIdeas", "FLEX_OpenToChanges",
  "RISK_Tolerance", "ENV_SustainedGrowth", "ENV_HighriskIndustry", "ENV_IndustryStability"
)

# Numerical ORG vars (not for these radar plots, but for context/filtering)
numerical_org_vars <- c("ORG_Employees", "ORG_Locations", "ORG_Departments", "ORG_Layers")

# Exclude vars (for determining all categorical vars if needed, not directly used in radar groups)
exclude_vars <- c("Owner_Type", "PDM_Selected", "PDM_Experience_DBB", "PDM_Experience_DB",
                  "PDM_Experience_PDB", "PDM_Experience_CMAR", "PDM_Experience_IPD", "X")
exclude_vars <- exclude_vars[exclude_vars %in% colnames(data_original)]

# 3. Isolate Selected Cases ------------------------------------------------
# Case 1: Extreme Outlier
outlier_employee_count <- 3700
case_outlier <- data_original %>%
  filter(ORG_Employees == outlier_employee_count) %>%
  slice(1) # Ensure only one row if duplicates exist

# Cases 2, 3, 4: From Cluster 3 (post-outlier removal, k=5)
data_for_clustering <- data_original %>%
  filter(ORG_Employees != outlier_employee_count)

all_categorical_vars <- setdiff(colnames(data_original), c(numerical_org_vars, exclude_vars, "Original_Row_ID"))
vars_for_kproto_input <- c(numerical_org_vars, all_categorical_vars)
vars_for_kproto_input <- vars_for_kproto_input[vars_for_kproto_input %in% colnames(data_for_clustering)]

kproto_mixed_input_data <- data_for_clustering %>%
  select(all_of(vars_for_kproto_input)) %>%
  mutate(across(all_of(all_categorical_vars[all_categorical_vars %in% vars_for_kproto_input]), ~ ordered(round(.), levels = 1:5)))

kproto_analysis_data_no_outlier <- data_for_clustering %>%
  select(Original_Row_ID, ORG_Employees, all_of(vars_for_kproto_input))

k_fixed <- 5
set.seed(123)

kproto_result_no_outlier <- clustMixType::kproto(
  kproto_mixed_input_data,
  k = k_fixed,
  verbose = FALSE
)
kproto_analysis_data_no_outlier$Cluster <- factor(kproto_result_no_outlier$cluster)

cluster_label_for_size_3 <- names(which(table(kproto_result_no_outlier$cluster) == 3))[1]
target_cluster_id <- as.numeric(cluster_label_for_size_3)
cases_cluster3 <- kproto_analysis_data_no_outlier %>%
  filter(Cluster == target_cluster_id)

# Combine all 4 cases to plot
selected_cases_data <- bind_rows(
  case_outlier,
  cases_cluster3
)

cat("Selected cases for overlayed radar plots (should be 4):
")
print(selected_cases_data[, c("Original_Row_ID", "ORG_Employees")])

# 4. Variable Name Formatting Function (from R/034) -------------------------
format_variable_names <- function(var_names) {
  formatted_names <- var_names
  name_mappings <- list(
    "STY_DataDriven" = "STY\nDataDriven", # Adjusted from R/034 for current var names
    "STY_Participation_Inclusion" = "STY\nParticipation\nInclusion",
    "STY_Participation_Relational" = "STY\nParticipation\nRelational",
    "STY_Adaptive_Informal" = "STY\nAdaptive\nInformal", 
    "STY_Adaptive_Changeable" = "STY\nAdaptive\nChangeable",
    "STY_Authoritative_Threats" = "STY\nAuthoritative\nThreats",
    "STY_Authoritative_Compliance" = "STY\nAuthoritative\nCompliance",
    "DIST_Athority_Dispersion" = "DIST\nAthority\nDispersion", 
    "DIST_Athority_Delegation" = "DIST\nAthority\nDelegation",
    "DIST_Process_InformalCommunication" = "DIST\nProcess\nInformalComm", 
    "DIST_Process_InformalProcedure" = "DIST\nProcess\nInformalProc",
    "CUL_Command" = "CUL\nCommand", 
    "CUL_Symbolic" = "CUL\nSymbolic", 
    "CUL_Formal" = "CUL\nFormal",
    "CUL_Experimental" = "CUL\nExperimental", 
    "CUL_Learning" = "CUL\nLearning",
    "FLEX_OpenToNewIdeas" = "FLEX\nOpenTo\nNewIdeas", 
    "FLEX_OpenToChanges" = "FLEX\nOpenTo\nChanges",
    "RISK_Tolerance" = "RISK\nTolerance", 
    "ENV_SustainedGrowth" = "ENV\nSustained\nGrowth",
    "ENV_HighriskIndustry" = "ENV\nHighrisk\nIndustry", 
    "ENV_IndustryStability" = "ENV\nIndustry\nStability"
  )
  for (i in seq_along(var_names)) {
    if (var_names[i] %in% names(name_mappings)) {
      formatted_names[i] <- name_mappings[[var_names[i]]]
    } else {
      formatted_names[i] <- gsub("_", "\n", var_names[i]) # Default
    }
  }
  return(formatted_names)
}

# 5. Function to Create and Save Overlayed Radar Plot ----------------------
create_overlayed_radar <- function(cases_data, var_group, group_name, file_suffix) {
  
  # Filter variables that are present in the cases_data
  vars_to_plot <- var_group[var_group %in% colnames(cases_data)]
  if (length(vars_to_plot) < 3) { # Radar chart needs at least 3 variables
    cat(paste("Skipping", group_name, "plot: Less than 3 variables available.
"))
    return()
  }
  
  radar_df_values <- cases_data[, vars_to_plot]
  
  # Prepare data for fmsb::radarchart
  # First two rows are max and min values
  max_min_df <- data.frame(matrix(c(rep(5, length(vars_to_plot)), rep(1, length(vars_to_plot))), nrow = 2, byrow = TRUE))
  colnames(max_min_df) <- vars_to_plot
  
  # Combine with case data
  final_radar_df <- rbind(max_min_df, radar_df_values)
  rownames(final_radar_df) <- c("Max", "Min", paste0("ID", cases_data$Original_Row_ID, "_Emp", cases_data$ORG_Employees))
  
  # Colors for 4 cases
  plot_colors <- RColorBrewer::brewer.pal(n = max(3, nrow(cases_data)), name = "Set1")[1:nrow(cases_data)]

  # Format variable names for display
  formatted_vars <- format_variable_names(vars_to_plot)
  colnames(final_radar_df) <- formatted_vars # Apply to the data frame for radarchart
  
  file_name <- file.path(plot_dir, paste0("Overlayed_Radar_", file_suffix, ".pdf"))
  cat(paste("Generating overlayed radar for", group_name, "and saving to", file_name, "...\n"))
  
  pdf(file_name, width = 10, height = 8)
  par(mar = c(1, 1, 2, 1)) # Adjust margins
  fmsb::radarchart(
    final_radar_df,
    axistype = 1, seg = 4, pty = 16,
    pcol = plot_colors,
    pfcol = scales::alpha(plot_colors, 0.3),
    plwd = 2, plty = 1,
    cglcol = "grey", cglty = 1, axislabcol = "grey",
    caxislabels = seq(1, 5, 1),
    title = paste("Overlayed Radar Plot - Selected Cases\n", group_name),
    vlcex = 0.7 # Variable label size
  )
  legend(
    x = "bottomright", 
    legend = rownames(final_radar_df)[-c(1,2)], # Exclude Max/Min rows
    fill = scales::alpha(plot_colors, 0.3),
    col = plot_colors,
    lty = 1, lwd = 2, cex = 0.8, 
    box.lty = 0, 
    inset = c(0.02, 0.02) # Slight inset from corner
  )
  dev.off()
  cat("Plot saved successfully.\n")
}

# 6. Generate Plots for Group 1 and Group 2 --------------------------------
# Check if all selected cases were found
if (nrow(selected_cases_data) == 4) {
  create_overlayed_radar(selected_cases_data, group1_vars, "Group 1: Decision & Style", "Group1_SelectedCases")
  create_overlayed_radar(selected_cases_data, group2_vars, "Group 2: Culture, Flex & Env", "Group2_SelectedCases")
} else {
  cat("Error: Did not find exactly 4 cases for plotting. Expected 1 outlier and 3 from Cluster 3.\n")
  cat("Number of cases found:", nrow(selected_cases_data), "\n")
}

cat("\nScript 074 execution complete. Overlayed radar plots saved to:", plot_dir, "\n") 