# R/130_single_case_radar_visualization.R
# This script extracts the specific case with ORG_Employees == 3700 from BOAT2_Data_Success.csv
# and creates radar chart visualizations for this single case.

# Set user library path to avoid permission issues
user_lib <- Sys.getenv("R_LIBS_USER")
if (!dir.exists(user_lib)) {
  dir.create(user_lib, recursive = TRUE, showWarnings = FALSE)
}
.libPaths(c(user_lib, .libPaths()))

# 0. Check and Install Required Packages -------------------------------------
cat("============== SCRIPT R/130 (Single Case Radar Visualization) STARTING ==============\n")

if (!requireNamespace("fmsb", quietly = TRUE)) install.packages("fmsb")
if (!requireNamespace("RColorBrewer", quietly = TRUE)) install.packages("RColorBrewer")
if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
if (!requireNamespace("readr", quietly = TRUE)) install.packages("readr")

library(fmsb)
library(RColorBrewer)
library(dplyr)
library(readr)

# Attempt to source setup file, but proceed if not found
if (file.exists("R/000_setup.R")) {
  source("R/000_setup.R")
  cat("R/000_setup.R sourced successfully.\n")
} else {
  cat("Warning: R/000_setup.R not found. Essential packages loaded directly.\n")
}

# 1. Setup and Configuration ------------------------------------------------
TARGET_EMPLOYEE_COUNT <- 3700

# Create subdirectories for results
fig_dir_130 <- "results/figures/130_single_case_radar"
tbl_dir_130 <- "results/tables/130_single_case_radar"

for (dir_path in c(fig_dir_130, tbl_dir_130)) {
  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE)
    cat(paste("Created directory:", dir_path, "\n"))
  }
}

# 2. Load Data and Extract Target Case --------------------------------------
cat("\n--- Loading data and extracting target case (ORG_Employees =", TARGET_EMPLOYEE_COUNT, ") ---\n")

data_file_path <- "data/BOAT2_Data_Success.csv"
if (file.exists(data_file_path)) {
  data_original <- read_csv(data_file_path, show_col_types = FALSE)
  cat("Loaded dataset", data_file_path, "with", nrow(data_original), "rows and", ncol(data_original), "columns.\n")
} else {
  stop(paste("Error: Dataset not found at", data_file_path))
}

# Extract the target case
target_case <- data_original %>%
  filter(ORG_Employees == TARGET_EMPLOYEE_COUNT)

if (nrow(target_case) == 0) {
  stop(paste("Error: No case found with ORG_Employees =", TARGET_EMPLOYEE_COUNT))
} else if (nrow(target_case) > 1) {
  cat("Warning: Multiple cases found with ORG_Employees =", TARGET_EMPLOYEE_COUNT, ". Using the first one.\n")
  target_case <- target_case %>% slice(1)
}

cat("Target case extracted successfully:\n")
cat("  Project_Success:", target_case$Project_Success, "\n")
cat("  PDM_Selected:", target_case$PDM_Selected, "\n")
cat("  ORG_Employees:", target_case$ORG_Employees, "\n")
cat("  ORG_Locations:", target_case$ORG_Locations, "\n")
cat("  ORG_Departments:", target_case$ORG_Departments, "\n")
cat("  ORG_Layers:", target_case$ORG_Layers, "\n")

# 3. Define Variable Groups for Radar Charts ---------------------------------
cat("\n--- Organizing variables for radar charts ---\n")

# Organizational structure variables (numerical)
org_structure_vars <- c("ORG_Employees", "ORG_Locations", "ORG_Departments", "ORG_Layers")

# Group 1: Decision Making Distribution & Style
group1_vars <- c(
  "DIST_Athority_Dispersion", "DIST_Athority_Delegation", "DIST_Process_InformalCommunication", "DIST_Process_InformalProcedure",
  "STY_DataDriven", "STY_Participation_Inclusion", "STY_Participation_Relational", "STY_Adaptive_Informal",
  "STY_Adaptive_Changeable", "STY_Authoritative_Threats", "STY_Authoritative_Compliance"
)

# Group 2: Decision Making Culture, Flexibility & Environment
group2_vars <- c(
  "CUL_Command", "CUL_Symbolic", "CUL_Formal", "CUL_Experimental", "CUL_Learning",
  "FLEX_OpenToNewIdeas", "FLEX_OpenToChanges",
  "RISK_Tolerance", "ENV_SustainedGrowth", "ENV_HighriskIndustry", "ENV_IndustryStability"
)

# Project Success variable
project_success_var <- "Project_Success"

# All Likert-scale variables combined
likert_vars_all <- c(group1_vars, group2_vars)

# Filter variables to only include those present in the data
data_cols <- colnames(target_case)
org_structure_vars <- org_structure_vars[org_structure_vars %in% data_cols]
group1_vars <- group1_vars[group1_vars %in% data_cols]
group2_vars <- group2_vars[group2_vars %in% data_cols]
likert_vars_all <- likert_vars_all[likert_vars_all %in% data_cols]

cat("Organizational structure vars:", paste(org_structure_vars, collapse=", "), "\n")
cat("Group 1 vars (", length(group1_vars), "):", paste(group1_vars, collapse=", "), "\n")
cat("Group 2 vars (", length(group2_vars), "):", paste(group2_vars, collapse=", "), "\n")
cat("All Likert vars (", length(likert_vars_all), "):", paste(likert_vars_all, collapse=", "), "\n")

# 4. Radar Chart Functions --------------------------------------------------

# Helper function to save plots in both PDF and PNG formats
save_plot_both_formats_130 <- function(plot_fn, file_path_without_ext, width, height) {
  # Save PDF
  pdf_path <- paste0(file_path_without_ext, ".pdf")
  pdf(pdf_path, width = width, height = height, pointsize = 10)
  plot_fn()
  dev.off()
  
  # Save PNG
  png_path <- paste0(file_path_without_ext, ".png")
  png(png_path, width = width, height = height, units = "in", res = 300, pointsize = 10)
  plot_fn()
  dev.off()
  
  cat("    Saved to:", pdf_path, "and", png_path, "\n")
}

format_variable_names <- function(var_names) {
  formatted_names <- var_names
  name_mappings <- list(
    "STY_DataDriven" = "STY\nDataDriven",
    "STY_Participation_Inclusion" = "STY\nParticipation\nInclusion",
    "STY_Participation_Relational" = "STY\nParticipation\nRelational",
    "STY_Adaptive_Informal" = "STY\nAdaptive\nInformal",
    "STY_Adaptive_Changeable" = "STY\nAdaptive\nChangeable",
    "STY_Authoritative_Threats" = "STY\nAuthoritative\nThreats",
    "STY_Authoritative_Compliance" = "STY\nAuthoritative\nCompliance",
    "DIST_Athority_Dispersion" = "DIST Athority\nDispersion",
    "DIST_Athority_Delegation" = "DIST Athority\nDelegation",
    "DIST_Process_InformalCommunication" = "DIST Process\nInformal\nCommunication",
    "DIST_Process_InformalProcedure" = "DIST Process\nInformal\nProcedure",
    "CUL_Command" = "CUL\nCommand",
    "CUL_Symbolic" = "CUL\nSymbolic",
    "CUL_Formal" = "CUL\nFormal",
    "CUL_Experimental" = "CUL\nExperimental",
    "CUL_Learning" = "CUL\nLearning",
    "FLEX_OpenToNewIdeas" = "FLEX\nOpenTo\nNewIdeas",
    "FLEX_OpenToChanges" = "FLEX\nOpenToChanges",
    "RISK_Tolerance" = "RISK\nTolerance",
    "ENV_SustainedGrowth" = "ENV\nSustainedGrowth",
    "ENV_HighriskIndustry" = "ENV\nHighriskIndustry",
    "ENV_IndustryStability" = "ENV\nIndustryStability",
    "Project_Success" = "Project\nSuccess",
    "ORG_Employees" = "ORG\nEmployees", 
    "ORG_Locations" = "ORG\nLocations",
    "ORG_Departments" = "ORG\nDepartments", 
    "ORG_Layers" = "ORG\nLayers"
  )
  
  for (i in seq_along(var_names)) {
    if (var_names[i] %in% names(name_mappings)) {
      formatted_names[i] <- name_mappings[[var_names[i]]]
    } else {
      formatted_names[i] <- gsub("_", "\n", var_names[i])
    }
  }
  return(formatted_names)
}

create_single_case_radar <- function(case_data, variables, title_main = "Single Case Radar Chart", 
                                   scale_min = 1, scale_max = 5,
                                   plot_width = 4.5, plot_height = 4,
                                   vlcex_custom = 1.0, calcex_custom = 1.0) {  # 10pt font sizes
  
  vars_to_use <- variables[variables %in% colnames(case_data)]
  if (length(vars_to_use) == 0) {
    cat("Error: None of the specified variables exist in the dataset for title:", title_main, "\n")
    return(NULL)
  }
  
  # Extract values for the single case
  case_values <- as.numeric(case_data[1, vars_to_use])
  
  # Prepare data for radarchart function
  # First row: max values, Second row: min values, Third row: actual case values
  data_for_fmsb <- data.frame(
    rbind(
      rep(scale_max, length(vars_to_use)),  # max
      rep(scale_min, length(vars_to_use)),  # min
      case_values                           # actual case
    )
  )
  colnames(data_for_fmsb) <- vars_to_use
  rownames(data_for_fmsb) <- c("max", "min", "case")
  
  formatted_labels <- format_variable_names(vars_to_use)
  
  # Define color for single case (changed to black)
  case_color <- "#000000"  # Black color for the single case
  
  plot_function <- function() {
    # 边距设置为0.5
    par(mar = c(0.5, 0.5, 0.5, 0.5))
    
    radarchart(
      data_for_fmsb,
      pfcol = adjustcolor(case_color, alpha.f = 0.3),
      pcol = case_color,
      plty = 1, 
      plwd = 3,  # Thicker line for single case
      cglcol = "gray70", 
      cglty = 1,
      axislabcol = "gray30", 
      calcex = calcex_custom, 
      vlcex = vlcex_custom, 
      caxislabels = seq(scale_min, scale_max, (scale_max - scale_min) / 4),
      vlabels = formatted_labels
    )
    
    # Add simplified legend with transparent background
    legend(
      "bottom", 
      legend = "Single Case Outlier",
      fill = adjustcolor(case_color, alpha.f = 0.3),
      col = case_color,
      lty = 1, 
      lwd = 3, 
      cex = 1.0, 
      box.lty = 0,
      bg = NA,
      xpd = NA,
      ncol = 1,
      inset = c(0, -0.15)
    )
  }
  
  return(list(plot_fn = plot_function, width = plot_width, height = plot_height))
}

# 5. Generate Radar Charts for the Target Case ------------------------------
cat("\n--- Generating radar charts for target case ---\n")

# Chart 1: All Likert Variables
if (length(likert_vars_all) > 0) {
  cat("Creating radar chart for all Likert variables...\n")
  radar_all_likert <- create_single_case_radar(
    target_case, 
    likert_vars_all, 
    title_main = "All Likert Variables"
  )
  
  if (!is.null(radar_all_likert)) {
    save_plot_both_formats_130(
      radar_all_likert$plot_fn, 
      file.path(fig_dir_130, "radar_all_likert_variables"),
      radar_all_likert$width, 
      radar_all_likert$height
    )
  }
}

# Chart 2: Group 1 Variables (Decision Making Distribution & Style)
if (length(group1_vars) > 0) {
  cat("Creating radar chart for Group 1 variables...\n")
  radar_group1 <- create_single_case_radar(
    target_case, 
    group1_vars, 
    title_main = "Group 1: Decision Making Distribution & Style"
  )
  
  if (!is.null(radar_group1)) {
    save_plot_both_formats_130(
      radar_group1$plot_fn, 
      file.path(fig_dir_130, "radar_group1_decision_style"),
      radar_group1$width, 
      radar_group1$height
    )
  }
}

# Chart 3: Group 2 Variables (Culture, Flexibility & Environment)
if (length(group2_vars) > 0) {
  cat("Creating radar chart for Group 2 variables...\n")
  radar_group2 <- create_single_case_radar(
    target_case, 
    group2_vars, 
    title_main = "Group 2: Culture, Flexibility & Environment"
  )
  
  if (!is.null(radar_group2)) {
    save_plot_both_formats_130(
      radar_group2$plot_fn, 
      file.path(fig_dir_130, "radar_group2_culture_flex_env"),
      radar_group2$width, 
      radar_group2$height
    )
  }
}

# Chart 3.5: Combined Group1 and Group2 side-by-side (no legend for single case)
if (length(group1_vars) > 0 && length(group2_vars) > 0) {
  cat("Creating combined radar chart for Group 1 and Group 2...\n")
  
  plot_combined_groups_single <- function() {
    par(xpd = NA)
    par(mfrow = c(1, 2), oma = c(0, 1, 1, 1), mar = c(1.2, 1.2, 1.2, 1.2))
    
    # Define color for single case
    case_color <- "#000000"  # Black
    
    # Plot Group 1
    vars_to_use_g1 <- group1_vars[group1_vars %in% colnames(target_case)]
    if (length(vars_to_use_g1) > 0) {
      case_values_g1 <- as.numeric(target_case[1, vars_to_use_g1])
      data_for_fmsb_g1 <- data.frame(
        rbind(
          rep(5, length(vars_to_use_g1)),  # max
          rep(1, length(vars_to_use_g1)),  # min
          case_values_g1                   # actual case
        )
      )
      colnames(data_for_fmsb_g1) <- vars_to_use_g1
      formatted_labels_g1 <- format_variable_names(vars_to_use_g1)
      
      radarchart(
        data_for_fmsb_g1,
        pfcol = adjustcolor(case_color, alpha.f = 0.3),
        pcol = case_color,
        plty = 1,
        plwd = 3,
        cglcol = "gray70",
        cglty = 1,
        axislabcol = "gray30",
        calcex = 1.0,
        vlcex = 0.9,
        caxislabels = seq(1, 5, 1),
        vlabels = formatted_labels_g1
      )
    }
    
    # Plot Group 2
    vars_to_use_g2 <- group2_vars[group2_vars %in% colnames(target_case)]
    if (length(vars_to_use_g2) > 0) {
      case_values_g2 <- as.numeric(target_case[1, vars_to_use_g2])
      data_for_fmsb_g2 <- data.frame(
        rbind(
          rep(5, length(vars_to_use_g2)),  # max
          rep(1, length(vars_to_use_g2)),  # min
          case_values_g2                   # actual case
        )
      )
      colnames(data_for_fmsb_g2) <- vars_to_use_g2
      formatted_labels_g2 <- format_variable_names(vars_to_use_g2)
      
      radarchart(
        data_for_fmsb_g2,
        pfcol = adjustcolor(case_color, alpha.f = 0.3),
        pcol = case_color,
        plty = 1,
        plwd = 3,
        cglcol = "gray70",
        cglty = 1,
        axislabcol = "gray30",
        calcex = 1.0,
        vlcex = 0.9,
        caxislabels = seq(1, 5, 1),
        vlabels = formatted_labels_g2
      )
    }
  }
  
  save_plot_both_formats_130(
    plot_combined_groups_single,
    file.path(fig_dir_130, "radar_group1_group2_combined"),
    9, 4
  )
}

# Chart 4: Organizational Structure Variables (with different scale)
if (length(org_structure_vars) > 0) {
  cat("Creating radar chart for organizational structure variables...\n")
  
  # For ORG variables, we need to determine appropriate scale
  org_values <- as.numeric(target_case[1, org_structure_vars])
  org_max <- max(org_values, na.rm = TRUE)
  org_min <- min(org_values, na.rm = TRUE)
  
  # Use log scale for ORG_Employees due to its large value
  target_case_org_scaled <- target_case
  if ("ORG_Employees" %in% org_structure_vars) {
    target_case_org_scaled$ORG_Employees_Log <- log10(target_case$ORG_Employees)
    org_structure_vars_scaled <- c(setdiff(org_structure_vars, "ORG_Employees"), "ORG_Employees_Log")
  } else {
    org_structure_vars_scaled <- org_structure_vars
  }
  
  # Determine scale for organizational variables
  org_values_scaled <- as.numeric(target_case_org_scaled[1, org_structure_vars_scaled])
  scale_max_org <- ceiling(max(org_values_scaled, na.rm = TRUE))
  scale_min_org <- 0
  
  radar_org <- create_single_case_radar(
    target_case_org_scaled, 
    org_structure_vars_scaled, 
    title_main = "Organizational Structure Variables",
    scale_min = scale_min_org,
    scale_max = scale_max_org
  )
  
  if (!is.null(radar_org)) {
    save_plot_both_formats_130(
      radar_org$plot_fn, 
      file.path(fig_dir_130, "radar_organizational_structure"),
      radar_org$width, 
      radar_org$height
    )
  }
}

# 6. Export Case Data to CSV ------------------------------------------------
cat("\n--- Exporting case data ---\n")

# Export the complete case data
case_csv_path <- file.path(tbl_dir_130, paste0("target_case_ORG_Employees_", TARGET_EMPLOYEE_COUNT, ".csv"))
write.csv(target_case, case_csv_path, row.names = FALSE)
cat("Complete case data saved to:", case_csv_path, "\n")

# Export grouped data
export_case_group_data <- function(case_data, group_name, group_vars, file_suffix) {
  if (length(group_vars) == 0) return()
  
  existing_vars <- group_vars[group_vars %in% colnames(case_data)]
  if (length(existing_vars) == 0) return()
  
  group_data <- case_data[, existing_vars, drop = FALSE]
  file_path <- file.path(tbl_dir_130, paste0("target_case_", file_suffix, ".csv"))
  write.csv(group_data, file_path, row.names = FALSE)
  cat("  ", group_name, "data saved to:", file_path, "\n")
}

export_case_group_data(target_case, "All Likert Variables", likert_vars_all, "all_likert")
export_case_group_data(target_case, "Group 1 Decision Style", group1_vars, "group1_decision_style")
export_case_group_data(target_case, "Group 2 Culture Flex Env", group2_vars, "group2_culture_flex_env")
export_case_group_data(target_case, "Organizational Structure", org_structure_vars, "organizational_structure")

# 7. Summary Statistics ------------------------------------------------------
cat("\n--- Case Summary ---\n")
cat("Target Case (ORG_Employees =", TARGET_EMPLOYEE_COUNT, ") Summary:\n")
cat("  Project Success:", target_case$Project_Success, "\n")
cat("  Project Delivery Method:", target_case$PDM_Selected, "\n")
cat("  Organization Size (Employees):", target_case$ORG_Employees, "\n")
cat("  Organization Locations:", target_case$ORG_Locations, "\n")
cat("  Organization Departments:", target_case$ORG_Departments, "\n")
cat("  Organization Layers:", target_case$ORG_Layers, "\n")

if (length(likert_vars_all) > 0) {
  likert_values <- as.numeric(target_case[1, likert_vars_all])
  cat("  Likert Variables Summary:\n")
  cat("    Mean:", round(mean(likert_values, na.rm = TRUE), 2), "\n")
  cat("    Median:", round(median(likert_values, na.rm = TRUE), 2), "\n")
  cat("    Min:", min(likert_values, na.rm = TRUE), "\n")
  cat("    Max:", max(likert_values, na.rm = TRUE), "\n")
}

cat("\nRadar charts and data tables generated successfully!")
cat("\nResults saved to:")
cat("\n  Figures:", fig_dir_130)
cat("\n  Tables:", tbl_dir_130)

cat("\n============== SCRIPT R/130 (Single Case Radar Visualization) FINISHED ==============\n") 