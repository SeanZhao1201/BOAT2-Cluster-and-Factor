# R/081_radar_charts_post_4_removal_k7.R
# This script creates radar chart visualizations for the k-prototype clustering results (k=7)
# obtained from R/080_kprototype_analysis_post_4_removal.R, after removing 4 specific cases.
# It visualizes data from BOAT2_Data_Success.csv, including Project_Success.

# 0. Check and Install Required Packages -------------------------------------
cat("============== SCRIPT R/082 (Radar Charts Post 1 Removal k7) STARTING ==============\n")

if (!requireNamespace("fmsb", quietly = TRUE)) install.packages("fmsb")
if (!requireNamespace("RColorBrewer", quietly = TRUE)) install.packages("RColorBrewer")
if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2") # For box plots
if (!requireNamespace("readr", quietly = TRUE)) install.packages("readr")

library(fmsb)
library(RColorBrewer)
library(dplyr)
library(ggplot2)
library(readr)

# Attempt to source setup file, but proceed if not found
if (file.exists("R/000_setup.R")) {
  source("R/000_setup.R")
  cat("R/000_setup.R sourced successfully.\n")
} else {
  cat("Warning: R/000_setup.R not found. Essential packages loaded directly.\n")
}

# 1. Setup and Configuration ------------------------------------------------
K_VALUE_FIXED <- 7
BASE_RESULTS_DIR_081 <- paste0("results/tables/081_kprototype_post_1_removal_k", K_VALUE_FIXED) # Path to results from script 081 (R/081_kprototype_analysis_post_1_removal_k7.R)

# Create subdirectories for results from this script (082)
fig_dir_082 <- paste0("results/figures/082_radar_charts_post_1_removal_k", K_VALUE_FIXED)
tbl_dir_082 <- paste0("results/tables/082_radar_charts_post_1_removal_k", K_VALUE_FIXED)

for (dir_path in c(fig_dir_082, tbl_dir_082)) {
  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE)
    cat(paste("Created directory:", dir_path, "\n"))
  }
}

cat("Will generate radar charts and box plots for k =", K_VALUE_FIXED, "post 1 case removal.\n")

# 2. Load Clustering Results from Script 081 --------------------------------
cat("\n--- Loading k-prototype clustering results (k=", K_VALUE_FIXED, ") from script 081 ---\n")

load_data_from_081 <- function(file_suffix) {
  file_name <- paste0("kproto_", file_suffix, "_k", K_VALUE_FIXED, "_post_1_removed.csv") # Changed from _post_4_removed
  file_path <- file.path(BASE_RESULTS_DIR_081, file_name)
  if (file.exists(file_path)) {
    data <- read_csv(file_path, show_col_types = FALSE)
    cat("  Loaded:", file_path, "- Rows:", nrow(data), "Cols:", ncol(data), "\n")
    # Ensure Cluster column, if exists, is treated as numeric/factor consistently
    if("Cluster" %in% colnames(data)) data$Cluster <- as.numeric(data$Cluster)
    if(paste0("Cluster_k", K_VALUE_FIXED) %in% colnames(data)) data[[paste0("Cluster_k", K_VALUE_FIXED)]] <- factor(data[[paste0("Cluster_k", K_VALUE_FIXED)]])
    return(data)
  } else {
    cat("  Error: Could not find file:", file_path, "\n")
    stop(paste("Required data file not found:", file_path))
    return(NULL)
  }
}

centroids_k7_data <- load_data_from_081("centroids")
medians_k7_data <- load_data_from_081("medians")
clustered_full_data_k7 <- load_data_from_081("clusters")

# Rename cluster column in clustered_full_data_k7 to simply "Cluster" for consistency with plotting functions
if (paste0("Cluster_k", K_VALUE_FIXED) %in% colnames(clustered_full_data_k7)) {
    clustered_full_data_k7 <- clustered_full_data_k7 %>%
        rename(Cluster = !!sym(paste0("Cluster_k", K_VALUE_FIXED)))
} else if (!"Cluster" %in% colnames(clustered_full_data_k7)){
    stop("Cluster assignment column not found in loaded cluster data.")
}

# 3. Define Variable Groups for Radar Charts ---------------------------------
cat("\n--- Organizing variables for radar charts ---\n")

# Numerical organizational structure variables (for box plots, not typically radar charts)
org_structure_vars <- c("ORG_Employees", "ORG_Locations", "ORG_Departments", "ORG_Layers") # R/034 uses ORG_Hierarchy_Layers, check consistency
# Correcting ORG_Hierarchy_Layers to ORG_Layers if that's what in the data from 080
if (!("ORG_Hierarchy_Layers" %in% colnames(centroids_k7_data)) && ("ORG_Layers" %in% colnames(centroids_k7_data))) {
    org_structure_vars[org_structure_vars == "ORG_Hierarchy_Layers"] <- "ORG_Layers"
}


group1_vars <- c(
  "DIST_Athority_Dispersion", "DIST_Athority_Delegation", "DIST_Process_InformalCommunication", "DIST_Process_InformalProcedure",
  "STY_DataDriven", "STY_Participation_Inclusion", "STY_Participation_Relational", "STY_Adaptive_Informal",
  "STY_Adaptive_Changeable", "STY_Authoritative_Threats", "STY_Authoritative_Compliance"
)

group2_vars <- c(
  "CUL_Command", "CUL_Symbolic", "CUL_Formal", "CUL_Experimental", "CUL_Learning",
  "FLEX_OpenToNewIdeas", "FLEX_OpenToChanges",
  "RISK_Tolerance", "ENV_SustainedGrowth", "ENV_HighriskIndustry", "ENV_IndustryStability"
)

# Project_Success is a new key variable
project_success_var <- "Project_Success"

# All Likert-scale type variables for combined charts (EXCLUDING Project_Success for mean/median radar)
# Ensure Project_Success is added if it exists in the data from 080
likert_vars_all <- c(group1_vars, group2_vars)
likert_vars_all <- unique(likert_vars_all) # ensure uniqueness

if (project_success_var %in% colnames(centroids_k7_data)) {
  cat(project_success_var, "is present and will be handled for tables/summaries, but not in combined Likert radar charts.\n")
} else {
  cat("Warning:", project_success_var, "not found in loaded centroids data.\n")
}

# Filter variable lists to only include those present in the actual data (centroids/medians)
filter_vars_exist <- function(vars_list, data_columns) {
  lapply(vars_list, function(vars) vars[vars %in% data_columns])
}

data_cols_ref <- colnames(centroids_k7_data) # Assuming centroids and medians have same relevant cols
org_structure_vars <- org_structure_vars[org_structure_vars %in% data_cols_ref]
group1_vars <- group1_vars[group1_vars %in% data_cols_ref]
group2_vars <- group2_vars[group2_vars %in% data_cols_ref]
likert_vars_all <- likert_vars_all[likert_vars_all %in% data_cols_ref]
if (project_success_var %in% data_cols_ref) {
    cat("Project_Success exists in data columns and will be used.\n")
} else {
    cat("Warning: Project_Success does not exist in data columns and will be excluded from lists.\n")
    project_success_var <- NULL # Nullify if not present
}


# 4. Radar Chart and Box Plot Functions (Adapted from R/034) ----------------

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
    "DIST_Athority_Dispersion" = "DIST\nAthority\nDispersion",
    "DIST_Athority_Delegation" = "DIST\nAthority\nDelegation",
    "DIST_Process_InformalCommunication" = "DIST\nProcess\nInformalComm",
    "DIST_Process_InformalProcedure" = "DIST\nProcess\nInformalProc",
    "CUL_Command" = "CUL\nCommand",
    "CUL_Symbolic" = "CUL\nSymbolic",
    "CUL_Formal" = "CUL\nFormal",
    "CUL_Experimental" = "CUL\nExperimental",
    "CUL_Learning" = "CUL\nLearning",
    "FLEX_OpenToNewIdeas" = "FLEX\nOpenToNewIdeas",
    "FLEX_OpenToChanges" = "FLEX\nOpenToChanges",
    "RISK_Tolerance" = "RISK\nTolerance",
    "ENV_SustainedGrowth" = "ENV\nSustainedGrowth",
    "ENV_HighriskIndustry" = "ENV\nHighriskIndustry",
    "ENV_IndustryStability" = "ENV\nIndustryStability",
    "Project_Success" = "Project\nSuccess", # Added Project_Success
    "ORG_Employees" = "ORG\nEmployees", "ORG_Locations" = "ORG\nLocations",
    "ORG_Departments" = "ORG\nDepartments", "ORG_Layers" = "ORG\nLayers"
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

create_radar_chart <- function(data_input, variables, title_main = "Radar Chart", 
                               scale_min = 1, scale_max = 5,
                               plot_width = 14, plot_height = 10,
                               vlcex_custom = NULL, calcex_custom = NULL, 
                               legend_cex_custom = NULL, show_legend_custom = TRUE) {
  vars_to_use <- variables[variables %in% colnames(data_input)]
  if (length(vars_to_use) == 0) {
    cat("Error in create_radar_chart: None of the specified variables exist in the dataset for title:", title_main, "\n")
    return(NULL)
  }
  
  # Ensure data_input has 'Cluster' column and it's used for rownames
  if (!"Cluster" %in% colnames(data_input)) {
      cat("Error: 'Cluster' column missing in data_input for radar chart.\n")
      print(head(data_input))
      stop("'Cluster' column missing.")
  }
  
  # Prepare data for radarchart function
  radar_df <- data_input %>%
    select(Cluster, all_of(vars_to_use)) %>%
    arrange(Cluster) # Ensure consistent order
  
  # Set rownames to be Cluster numbers for fmsb compatibility if not already
  # The fmsb function expects the cluster data itself, not the row names for labels.
  # It takes the first N rows of data for N clusters specified.
  
  # The data to pass to radarchart should be numeric matrix/df with clusters as rows
  # and variables as columns. First two rows are max/min.
  data_for_fmsb <- radar_df %>% select(-Cluster) %>% as.data.frame()

  # Add max and min rows
  data_for_fmsb <- rbind(
    rep(scale_max, ncol(data_for_fmsb)),
    rep(scale_min, ncol(data_for_fmsb)),
    data_for_fmsb
  )
  rownames(data_for_fmsb)[1:2] <- c("max", "min")
  
  formatted_labels <- format_variable_names(vars_to_use)
  # colnames(data_for_fmsb) <- formatted_labels # fmsb uses original colnames for matching

  vlcex_to_use <- ifelse(is.null(vlcex_custom), 1.0, vlcex_custom)
  calcex_to_use <- ifelse(is.null(calcex_custom), 1.0, calcex_custom)
  legend_cex_to_use <- ifelse(is.null(legend_cex_custom), 1.0, legend_cex_custom)

  num_clusters <- nrow(radar_df)
  cluster_colors_palette <- brewer.pal(n = min(max(num_clusters, 3), 9), name = "Set1")
  if (num_clusters > length(cluster_colors_palette)) {
      cluster_colors_palette <- rep(cluster_colors_palette, length.out = num_clusters) # Extend if more clusters
  }
  
  plot_function <- function() {
    par(mar = c(2, 2, 3, 2))
    radarchart(
      data_for_fmsb,
      pfcol = adjustcolor(cluster_colors_palette[1:num_clusters], alpha.f = 0.3),
      pcol = cluster_colors_palette[1:num_clusters],
      plty = 1, plwd = 2.5, cglcol = "gray70", cglty = 1,
      axislabcol = "gray30", calcex = calcex_to_use, vlcex = vlcex_to_use, 
      caxislabels = seq(scale_min, scale_max, (scale_max - scale_min) / 4),
      title = title_main,
      vlabels = formatted_labels # Use formatted labels for display
    )
    if (show_legend_custom && num_clusters > 0) {
        legend_labels <- paste("Cluster", radar_df$Cluster)
        legend(
            "bottomright", legend = legend_labels,
            fill = adjustcolor(cluster_colors_palette[1:num_clusters], alpha.f = 0.3),
            col = cluster_colors_palette[1:num_clusters],
            lty = 1, lwd = 2, cex = legend_cex_to_use, box.lty = 0
        )
    }
  }
  return(list(plot_fn = plot_function, width = plot_width, height = plot_height))
}

create_box_plots_081 <- function(k_val, data_for_boxplot, output_dir) {
  cat("\nCreating box plots for k =", k_val, "...\n")
  if (is.null(data_for_boxplot) || !is.data.frame(data_for_boxplot) || !("Cluster" %in% colnames(data_for_boxplot))) {
    cat("  Error: Invalid or NULL cluster data, or missing 'Cluster' column for box plots.\n")
    return(NULL)
  }
  
  box_plot_dir_specific <- file.path(output_dir, paste0("k", k_val, "_box_plots"))
  if (!dir.exists(box_plot_dir_specific)) dir.create(box_plot_dir_specific, recursive = TRUE)
  
  # Use only existing org_structure_vars
  current_org_vars <- org_structure_vars[org_structure_vars %in% colnames(data_for_boxplot)]
  cat("  Generating box plots for organization structure variables:", paste(current_org_vars, collapse=", "), "\n")
  if (length(current_org_vars) == 0) {
    cat("  Error: No ORG_ variables found in the dataset for box plots.\n")
    return(NULL)
  }
  
  # Log transform ORG_Employees if it exists
  data_for_boxplot_mod <- data_for_boxplot
  vars_for_plot <- current_org_vars
  if ("ORG_Employees" %in% current_org_vars) {
    data_for_boxplot_mod$ORG_Employees_Log <- log1p(data_for_boxplot_mod$ORG_Employees)
    vars_for_plot <- c(setdiff(current_org_vars, "ORG_Employees"), "ORG_Employees_Log")
  }
  
  num_plots <- length(vars_for_plot)
  num_rows_layout <- ceiling(num_plots / 2)
  
  combined_pdf_path <- file.path(box_plot_dir_specific, "organization_structure_combined_boxplot.pdf")
  pdf(combined_pdf_path, width = 16, height = 6 * num_rows_layout)
  par(mfrow = c(num_rows_layout, 2), mar = c(5, 5, 4, 2) + 0.1)
  
  cluster_colors_bp <- brewer.pal(n = min(max(k_val, 3), 9), name = "Set1")
   if (k_val > length(cluster_colors_bp)) {
      cluster_colors_bp <- rep(cluster_colors_bp, length.out = k_val)
  }

  for (var_name in vars_for_plot) {
    var_display_name <- gsub("_Log$", " (Log Scale)", var_name) %>% gsub("_", " ", .)
    
    formula_bp <- reformulate("factor(Cluster)", response = var_name)
    boxplot(formula_bp, data = data_for_boxplot_mod, 
            main = var_display_name, xlab = "Cluster", ylab = var_display_name, 
            col = cluster_colors_bp, cex.axis = 1.2, cex.lab = 1.3, cex.main = 1.4, outline = TRUE)
    
    if (grepl("Log Scale", var_display_name)) {
        log_rng <- range(data_for_boxplot_mod[[var_name]], na.rm = TRUE)
        log_brks <- pretty(log_rng, n=5)
        orig_vals_brks <- round(expm1(log_brks))
        axis(2, at = log_brks, labels = paste0(round(log_brks,1), "\n(", orig_vals_brks, ")"), las = 1, cex.axis = 1.0)
    }
    
    med_values <- tapply(data_for_boxplot_mod[[var_name]], data_for_boxplot_mod$Cluster, median, na.rm = TRUE)
    text_labels_bp <- if (grepl("Log Scale", var_display_name)) sprintf("%.1f\n(%.0f)", med_values, expm1(med_values)) else sprintf("%.1f", med_values)
    text(1:k_val, med_values + 0.05 * diff(range(data_for_boxplot_mod[[var_name]], na.rm=TRUE)), labels=text_labels_bp, cex=0.9)
  }
  dev.off()
  par(mfrow = c(1,1)) # Reset layout
  cat("  Combined ORG structure box plot saved to:", combined_pdf_path, "\n")
}

# 5. Generate and Save Charts for k=7 ---------------------------------------
cat("\n--- Generating charts for k=", K_VALUE_FIXED, " ---\n")

generate_all_charts_for_k <- function(k_val, centroids_df, medians_df, full_cluster_df, output_fig_dir) {
  cat("\nProcessing charts for k =", k_val, "\n")
  
  # Charts based on MEANS (centroids)
  cat("  Generating charts based on MEANS (centroids)...\n")
  dir_means <- file.path(output_fig_dir, paste0("k", k_val, "_means"))
  if (!dir.exists(dir_means)) dir.create(dir_means, recursive = TRUE)
  
  if (length(likert_vars_all) > 0) {
      radar_all_likert_means <- create_radar_chart(centroids_df, likert_vars_all, title = paste0("All Likert Vars (k=", k_val, ") - Means"))
      if(!is.null(radar_all_likert_means)) {
        pdf(file.path(dir_means, "radar_all_likert_means.pdf"), width = radar_all_likert_means$width, height = radar_all_likert_means$height); radar_all_likert_means$plot_fn(); dev.off()
      }
  }
  if (length(group1_vars) > 0) {
      radar_g1_means <- create_radar_chart(centroids_df, group1_vars, title = paste0("Group 1: Decision & Style (k=", k_val, ") - Means"))
      if(!is.null(radar_g1_means)) {
        pdf(file.path(dir_means, "radar_group1_means.pdf"), width = radar_g1_means$width, height = radar_g1_means$height); radar_g1_means$plot_fn(); dev.off()
      }
  }
  if (length(group2_vars) > 0) {
      radar_g2_means <- create_radar_chart(centroids_df, group2_vars, title = paste0("Group 2: Culture, Flex & Env (k=", k_val, ") - Means"))
      if(!is.null(radar_g2_means)) {
        pdf(file.path(dir_means, "radar_group2_means.pdf"), width = radar_g2_means$width, height = radar_g2_means$height); radar_g2_means$plot_fn(); dev.off()
      }
  }
  if (!is.null(project_success_var) && project_success_var %in% colnames(centroids_df)){
      cat("  (Skipping mean-based radar chart for Project_Success as it's categorical text)\n")
  }
  
  # Charts based on MEDIANS
  cat("  Generating charts based on MEDIANS...\n")
  dir_medians <- file.path(output_fig_dir, paste0("k", k_val, "_medians"))
  if (!dir.exists(dir_medians)) dir.create(dir_medians, recursive = TRUE)
  
  if (length(likert_vars_all) > 0) {
      radar_all_likert_medians <- create_radar_chart(medians_df, likert_vars_all, title = paste0("All Likert Vars (k=", k_val, ") - Medians"))
      if(!is.null(radar_all_likert_medians)) {
        pdf(file.path(dir_medians, "radar_all_likert_medians.pdf"), width = radar_all_likert_medians$width, height = radar_all_likert_medians$height); radar_all_likert_medians$plot_fn(); dev.off()
      }
  }
  if (length(group1_vars) > 0) {
      radar_g1_medians <- create_radar_chart(medians_df, group1_vars, title = paste0("Group 1: Decision & Style (k=", k_val, ") - Medians"))
      if(!is.null(radar_g1_medians)) {
        pdf(file.path(dir_medians, "radar_group1_medians.pdf"), width = radar_g1_medians$width, height = radar_g1_medians$height); radar_g1_medians$plot_fn(); dev.off()
      }
  }
  if (length(group2_vars) > 0) {
      radar_g2_medians <- create_radar_chart(medians_df, group2_vars, title = paste0("Group 2: Culture, Flex & Env (k=", k_val, ") - Medians"))
      if(!is.null(radar_g2_medians)) {
        pdf(file.path(dir_medians, "radar_group2_medians.pdf"), width = radar_g2_medians$width, height = radar_g2_medians$height); radar_g2_medians$plot_fn(); dev.off()
      }
  }
   if (!is.null(project_success_var) && project_success_var %in% colnames(medians_df)){
      cat("  (Skipping median-based radar chart for Project_Success as it's categorical text)\n")
  }

  # Individual cluster radar charts (e.g. for All Likert Vars)
  cat("  Generating individual radar charts for each cluster (All Likert Vars - Means & Medians)...\n")
  for (cl_idx in 1:k_val) {
    # Means - All Likert
    cluster_centroid_data_all_likert <- centroids_df %>% filter(Cluster == cl_idx)
    if(nrow(cluster_centroid_data_all_likert) > 0 && length(likert_vars_all) > 0) {
      ind_radar_means_all_likert <- create_radar_chart(cluster_centroid_data_all_likert, likert_vars_all, title = paste0("All Likert Vars - Profile for Cluster ", cl_idx, " (k=", k_val, ") - Means"))
      if(!is.null(ind_radar_means_all_likert)) {
        pdf(file.path(dir_means, paste0("radar_cluster", cl_idx, "_all_likert_means.pdf")), width=ind_radar_means_all_likert$width, height=ind_radar_means_all_likert$height); ind_radar_means_all_likert$plot_fn(); dev.off()
      }
    }
    # Medians - All Likert
    cluster_median_data_all_likert <- medians_df %>% filter(Cluster == cl_idx)
    if(nrow(cluster_median_data_all_likert) > 0 && length(likert_vars_all) > 0) {
      ind_radar_medians_all_likert <- create_radar_chart(cluster_median_data_all_likert, likert_vars_all, title = paste0("All Likert Vars - Profile for Cluster ", cl_idx, " (k=", k_val, ") - Medians"))
      if(!is.null(ind_radar_medians_all_likert)) {
        pdf(file.path(dir_medians, paste0("radar_cluster", cl_idx, "_all_likert_medians.pdf")), width=ind_radar_medians_all_likert$width, height=ind_radar_medians_all_likert$height); ind_radar_medians_all_likert$plot_fn(); dev.off()
      }
    }
  }

  # NEW SECTION: Non-overlayed (grid) plots for Group1 and Group2 by cluster
  cat("  Generating non-overlayed grid plots for Group1 & Group2 by cluster (Means & Medians)...\n")
  # Define layout for 7 clusters (k_val)
  # Adjust num_cols and num_rows if k_val is different from 7 in other uses of this script
  num_plot_cols <- 3
  num_plot_rows <- ceiling(k_val / num_plot_cols)
  grid_plot_width <- 18
  grid_plot_height <- 5 + (num_plot_rows * 3.5) # Base height + per row
  small_plot_vlcex <- 0.7 # Variable label cex for small plots
  small_plot_calcex <- 0.7 # Axis label cex for small plots

  # --- For MEANS data ---
  # Group 1 - Means - Grid
  if (length(group1_vars) > 0) {
      pdf_path_g1_means_grid <- file.path(dir_means, paste0("grid_all_clusters_group1_means.pdf"))
      pdf(pdf_path_g1_means_grid, width = grid_plot_width, height = grid_plot_height)
      par(mfrow = c(num_plot_rows, num_plot_cols), oma = c(0, 0, 3, 0), mar = c(1,1,3,1))
      for (cl_idx in 1:k_val) {
          cluster_data_single <- centroids_df %>% filter(Cluster == cl_idx)
          if(nrow(cluster_data_single) > 0){
              plot_title_g1m <- paste0("Cl. ", cl_idx, ": Grp 1 - Means")
              plot_obj <- create_radar_chart(cluster_data_single, group1_vars, title_main = plot_title_g1m, 
                                             vlcex_custom = small_plot_vlcex, calcex_custom = small_plot_calcex, show_legend_custom = FALSE)
              if(!is.null(plot_obj)) plot_obj$plot_fn() else { plot.new(); text(0.5,0.5, "Error/No Data"); title(plot_title_g1m)}
          } else {
              plot.new(); text(0.5,0.5, paste0("No Data for Cl. ", cl_idx)); title(paste0("Cl. ", cl_idx, ": Grp 1 - Means"))
          }
      }
      title(paste0("Group 1 (Decision & Style) - Means by Cluster (k=", k_val, ")"), outer = TRUE, cex.main = 1.5)
      dev.off()
      par(mfrow = c(1, 1)) # Reset layout
      cat("    Saved Group 1 (Means) grid plot to:", pdf_path_g1_means_grid, "\n")
  }

  # Group 2 - Means - Grid
  if (length(group2_vars) > 0) {
      pdf_path_g2_means_grid <- file.path(dir_means, paste0("grid_all_clusters_group2_means.pdf"))
      pdf(pdf_path_g2_means_grid, width = grid_plot_width, height = grid_plot_height)
      par(mfrow = c(num_plot_rows, num_plot_cols), oma = c(0, 0, 3, 0), mar = c(1,1,3,1))
      for (cl_idx in 1:k_val) {
          cluster_data_single <- centroids_df %>% filter(Cluster == cl_idx)
          if(nrow(cluster_data_single) > 0){
              plot_title_g2m <- paste0("Cl. ", cl_idx, ": Grp 2 - Means")
              plot_obj <- create_radar_chart(cluster_data_single, group2_vars, title_main = plot_title_g2m, 
                                             vlcex_custom = small_plot_vlcex, calcex_custom = small_plot_calcex, show_legend_custom = FALSE)
              if(!is.null(plot_obj)) plot_obj$plot_fn() else { plot.new(); text(0.5,0.5, "Error/No Data"); title(plot_title_g2m)}
          } else {
              plot.new(); text(0.5,0.5, paste0("No Data for Cl. ", cl_idx)); title(paste0("Cl. ", cl_idx, ": Grp 2 - Means"))
          }
      }
      title(paste0("Group 2 (Culture, Flex & Env) - Means by Cluster (k=", k_val, ")"), outer = TRUE, cex.main = 1.5)
      dev.off()
      par(mfrow = c(1, 1)) # Reset layout
      cat("    Saved Group 2 (Means) grid plot to:", pdf_path_g2_means_grid, "\n")
  }

  # --- For MEDIANS data ---
  # Group 1 - Medians - Grid
  if (length(group1_vars) > 0) {
      pdf_path_g1_medians_grid <- file.path(dir_medians, paste0("grid_all_clusters_group1_medians.pdf"))
      pdf(pdf_path_g1_medians_grid, width = grid_plot_width, height = grid_plot_height)
      par(mfrow = c(num_plot_rows, num_plot_cols), oma = c(0, 0, 3, 0), mar = c(1,1,3,1))
      for (cl_idx in 1:k_val) {
          cluster_data_single <- medians_df %>% filter(Cluster == cl_idx)
          if(nrow(cluster_data_single) > 0){
              plot_title_g1med <- paste0("Cl. ", cl_idx, ": Grp 1 - Medians")
              plot_obj <- create_radar_chart(cluster_data_single, group1_vars, title_main = plot_title_g1med, 
                                             vlcex_custom = small_plot_vlcex, calcex_custom = small_plot_calcex, show_legend_custom = FALSE)
              if(!is.null(plot_obj)) plot_obj$plot_fn() else { plot.new(); text(0.5,0.5, "Error/No Data"); title(plot_title_g1med)}
          } else {
              plot.new(); text(0.5,0.5, paste0("No Data for Cl. ", cl_idx)); title(paste0("Cl. ", cl_idx, ": Grp 1 - Medians"))
          }
      }
      title(paste0("Group 1 (Decision & Style) - Medians by Cluster (k=", k_val, ")"), outer = TRUE, cex.main = 1.5)
      dev.off()
      par(mfrow = c(1, 1)) # Reset layout
      cat("    Saved Group 1 (Medians) grid plot to:", pdf_path_g1_medians_grid, "\n")
  }

  # Group 2 - Medians - Grid
  if (length(group2_vars) > 0) {
      pdf_path_g2_medians_grid <- file.path(dir_medians, paste0("grid_all_clusters_group2_medians.pdf"))
      pdf(pdf_path_g2_medians_grid, width = grid_plot_width, height = grid_plot_height)
      par(mfrow = c(num_plot_rows, num_plot_cols), oma = c(0, 0, 3, 0), mar = c(1,1,3,1))
      for (cl_idx in 1:k_val) {
          cluster_data_single <- medians_df %>% filter(Cluster == cl_idx)
          if(nrow(cluster_data_single) > 0){
              plot_title_g2med <- paste0("Cl. ", cl_idx, ": Grp 2 - Medians")
              plot_obj <- create_radar_chart(cluster_data_single, group2_vars, title_main = plot_title_g2med, 
                                             vlcex_custom = small_plot_vlcex, calcex_custom = small_plot_calcex, show_legend_custom = FALSE)
              if(!is.null(plot_obj)) plot_obj$plot_fn() else { plot.new(); text(0.5,0.5, "Error/No Data"); title(plot_title_g2med)}
          } else {
              plot.new(); text(0.5,0.5, paste0("No Data for Cl. ", cl_idx)); title(paste0("Cl. ", cl_idx, ": Grp 2 - Medians"))
          }
      }
      title(paste0("Group 2 (Culture, Flex & Env) - Medians by Cluster (k=", k_val, ")"), outer = TRUE, cex.main = 1.5)
      dev.off()
      par(mfrow = c(1, 1)) # Reset layout
      cat("    Saved Group 2 (Medians) grid plot to:", pdf_path_g2_medians_grid, "\n")
  }

  # Box Plots for ORG_ variables
  create_box_plots_081(k_val = k_val, data_for_boxplot = full_cluster_df, output_dir = output_fig_dir)
  
  cat("Finished charts for k =", k_val, "\n")
}

# Generate all charts for k=7
if (!is.null(centroids_k7_data) && !is.null(medians_k7_data) && !is.null(clustered_full_data_k7)) {
  generate_all_charts_for_k(K_VALUE_FIXED, centroids_k7_data, medians_k7_data, clustered_full_data_k7, fig_dir_082)
} else {
  cat("Error: One or more required dataframes (centroids, medians, clustered_full_data) for k=", K_VALUE_FIXED, " is NULL. Skipping chart generation.\n")
}

# 6. Export Data Tables (Means/Medians for groups) --------------------------
cat("\n--- Exporting data tables (means/medians for variable groups) to:", tbl_dir_082, " ---\n")

export_grouped_data <- function(k_val, data_df, group_name, group_vars_list, id_col = "Cluster", file_prefix, output_tbl_dir) {
  if (is.null(data_df) || nrow(data_df) == 0) return()
  
  # Select only existing group variables plus the ID column
  existing_group_vars <- group_vars_list[group_vars_list %in% colnames(data_df)]
  if (length(existing_group_vars) == 0) {
      cat("    No variables from group '", group_name, "' exist in the data. Skipping export for this group.\n")
      return()
  }
  
  data_to_export <- data_df[, c(id_col, existing_group_vars), drop = FALSE]
  
  file_path_export <- file.path(output_tbl_dir, paste0(file_prefix, "_k", k_val, "_", gsub(" ", "_", tolower(group_name)), ".csv"))
  write.csv(data_to_export, file_path_export, row.names = FALSE)
  cat("  Exported", group_name, file_prefix, "to:", file_path_export, "\n")
}

# Export for k=7
# Means
export_grouped_data(K_VALUE_FIXED, centroids_k7_data, "All Likert Vars", likert_vars_all, file_prefix = "means", output_tbl_dir = tbl_dir_082)
export_grouped_data(K_VALUE_FIXED, centroids_k7_data, "Group 1 Decision Style", group1_vars, file_prefix = "means", output_tbl_dir = tbl_dir_082)
export_grouped_data(K_VALUE_FIXED, centroids_k7_data, "Group 2 Culture Flex Env", group2_vars, file_prefix = "means", output_tbl_dir = tbl_dir_082)
if(!is.null(project_success_var) && project_success_var %in% colnames(centroids_k7_data)) export_grouped_data(K_VALUE_FIXED, centroids_k7_data, "Project Success", project_success_var, file_prefix = "means", output_tbl_dir = tbl_dir_082) # This will show modes

# Medians
export_grouped_data(K_VALUE_FIXED, medians_k7_data, "All Likert Vars", likert_vars_all, file_prefix = "medians", output_tbl_dir = tbl_dir_082)
export_grouped_data(K_VALUE_FIXED, medians_k7_data, "Group 1 Decision Style", group1_vars, file_prefix = "medians", output_tbl_dir = tbl_dir_082)
export_grouped_data(K_VALUE_FIXED, medians_k7_data, "Group 2 Culture Flex Env", group2_vars, file_prefix = "medians", output_tbl_dir = tbl_dir_082)
if(!is.null(project_success_var) && project_success_var %in% colnames(medians_k7_data)) export_grouped_data(K_VALUE_FIXED, medians_k7_data, "Project Success", project_success_var, file_prefix = "medians", output_tbl_dir = tbl_dir_082) # This will show modes

cat("
Chart and table generation for k=", K_VALUE_FIXED, " post 1 case removal completed.
Results saved to respective subdirectories under results/figures and results/tables.
")
cat("============== SCRIPT R/082 (Radar Charts Post 1 Removal k7) FINISHED ==============\n") 