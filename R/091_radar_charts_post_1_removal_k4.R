# R/091_radar_charts_post_1_removal_k4.R
# This script creates radar chart visualizations for the k-prototype clustering results (k=4)
# obtained from R/090_kprototype_analysis_post_1_removal_k4.R, after removing 1 specific case.
# It visualizes data from BOAT2_Data_Success.csv, including Project_Success.

# Set user library path to avoid permission issues
user_lib <- Sys.getenv("R_LIBS_USER")
if (!dir.exists(user_lib)) {
  dir.create(user_lib, recursive = TRUE, showWarnings = FALSE)
}
.libPaths(c(user_lib, .libPaths()))

# 0. Check and Install Required Packages -------------------------------------
cat("============== SCRIPT R/091 (Radar Charts Post 1 Removal k4) STARTING ==============\n")

# Set CRAN mirror to Tsinghua University
options(repos = c(CRAN = "https://mirrors.tuna.tsinghua.edu.cn/CRAN/"))
cat("CRAN mirror set to Tsinghua University: https://mirrors.tuna.tsinghua.edu.cn/CRAN/\n")

if (!requireNamespace("fmsb", quietly = TRUE)) install.packages("fmsb")
if (!requireNamespace("RColorBrewer", quietly = TRUE)) install.packages("RColorBrewer")
if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2") # For box plots
library(fmsb)
library(RColorBrewer)
library(dplyr)
library(ggplot2)

# Attempt to source setup file, but proceed if not found
if (file.exists("R/000_setup.R")) {
  source("R/000_setup.R")
  cat("R/000_setup.R sourced successfully.\n")
} else {
  cat("Warning: R/000_setup.R not found. Essential packages loaded directly.\n")
}

# 1. Setup and Configuration ------------------------------------------------
K_VALUE_FIXED <- 4
BASE_RESULTS_DIR_090 <- paste0("results/tables/090_kprototype_post_1_removal_k", K_VALUE_FIXED) # Path to results from script 090

# Create subdirectories for results from this script (091)
fig_dir_091 <- paste0("results/figures/091_radar_charts_post_1_removal_k", K_VALUE_FIXED)
tbl_dir_091 <- paste0("results/tables/091_radar_charts_post_1_removal_k", K_VALUE_FIXED)

for (dir_path in c(fig_dir_091, tbl_dir_091)) {
  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE)
    cat(paste("Created directory:", dir_path, "\n"))
  }
}

cat("Will generate radar charts and box plots for k =", K_VALUE_FIXED, "post 1 case removal.\n")

# 2. Load Clustering Results from Script 090 --------------------------------
cat("\n--- Loading k-prototype clustering results (k=", K_VALUE_FIXED, ") from script 090 ---\n")

load_data_from_090 <- function(file_suffix) {
  file_name <- paste0("kproto_", file_suffix, "_k", K_VALUE_FIXED, "_post_1_removed.csv")
  file_path <- file.path(BASE_RESULTS_DIR_090, file_name)
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

centroids_k4_data <- load_data_from_090("centroids")
medians_k4_data <- load_data_from_090("medians")
clustered_full_data_k4 <- load_data_from_090("clusters")

# Rename cluster column in clustered_full_data_k4 to simply "Cluster" for consistency with plotting functions
if (paste0("Cluster_k", K_VALUE_FIXED) %in% colnames(clustered_full_data_k4)) {
    clustered_full_data_k4 <- clustered_full_data_k4 %>%
        rename(Cluster = !!sym(paste0("Cluster_k", K_VALUE_FIXED)))
} else if (!"Cluster" %in% colnames(clustered_full_data_k4)){
    stop("Cluster assignment column not found in loaded cluster data.")
}

# 3. Define Variable Groups for Radar Charts ---------------------------------
cat("\n--- Organizing variables for radar charts ---\n")

# Numerical organizational structure variables (for box plots, not typically radar charts)
org_structure_vars <- c("ORG_Employees", "ORG_Locations", "ORG_Departments", "ORG_Layers")
# Correcting ORG_Hierarchy_Layers to ORG_Layers if that's what in the data from 090
if (!("ORG_Hierarchy_Layers" %in% colnames(centroids_k4_data)) && ("ORG_Layers" %in% colnames(centroids_k4_data))) {
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
# Ensure Project_Success is added if it exists in the data from 090
likert_vars_all <- c(group1_vars, group2_vars)
likert_vars_all <- unique(likert_vars_all) # ensure uniqueness

if (project_success_var %in% colnames(centroids_k4_data)) {
  cat(project_success_var, "is present and will be handled for tables/summaries, but not in combined Likert radar charts.\n")
} else {
  cat("Warning:", project_success_var, "not found in loaded centroids data.\n")
}

# Filter variable lists to only include those present in the actual data (centroids/medians)
filter_vars_exist <- function(vars_list, data_columns) {
  lapply(vars_list, function(vars) vars[vars %in% data_columns])
}

data_cols_ref <- colnames(centroids_k4_data) # Assuming centroids and medians have same relevant cols
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

# Helper function to save plots in both PDF and PNG formats
save_plot_both_formats <- function(plot_fn, file_path_without_ext, width, height) {
  # Save PDF with 10pt font
  pdf_path <- paste0(file_path_without_ext, ".pdf")
  pdf(pdf_path, width = width, height = height, pointsize = 10)
  plot_fn()
  dev.off()
  
  # Save PNG with 10pt font
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
                               plot_width = 4.5, plot_height = 4,
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

  # Font sizes set to 1.0 (10pt base font size set in device)
  vlcex_to_use <- ifelse(is.null(vlcex_custom), 1.0, vlcex_custom)
  calcex_to_use <- ifelse(is.null(calcex_custom), 1.0, calcex_custom)
  legend_cex_to_use <- ifelse(is.null(legend_cex_custom), 1.0, legend_cex_custom)

  num_clusters <- nrow(radar_df)
  
  # Define the color palette
  if (num_clusters == 4) {
    # Use RColorBrewer's "Set2" palette for K=4 for distinct colors
    cluster_colors_palette <- brewer.pal(4, "Set2") 
  } else {
    # Default palette for other numbers of clusters
    cluster_colors_palette <- brewer.pal(n = min(max(num_clusters, 3), 9), name = "Set1")
    if (num_clusters > length(cluster_colors_palette)) {
        cluster_colors_palette <- rep(cluster_colors_palette, length.out = num_clusters)
    }
  }
  
  plot_function <- function() {
    par(mar = c(0.5, 0.5, 0.5, 0.5))
    radarchart(
      data_for_fmsb,
      pfcol = adjustcolor(cluster_colors_palette[1:num_clusters], alpha.f = 0.3),
      pcol = cluster_colors_palette[1:num_clusters],
      plty = 1, plwd = 2.5, cglcol = "gray70", cglty = 1,
      axislabcol = "gray30", calcex = calcex_to_use, vlcex = vlcex_to_use, 
      caxislabels = seq(scale_min, scale_max, (scale_max - scale_min) / 4),
      vlabels = formatted_labels # Use formatted labels for display
    )
    # Title removed as requested
    if (show_legend_custom && num_clusters > 0) {
        # Define cluster names with sample sizes
        cluster_names <- c(
          "Cluster 1: Adaptive Learning (n=3)",
          "Cluster 2: Integrated-Innovative (n=24)", 
          "Cluster 3: Moderate Collaborative (n=53)",
          "Cluster 4: Risk-averse Command (n=28)"
        )
        legend_labels <- cluster_names[radar_df$Cluster]
        legend(
            "bottom", legend = legend_labels,
            fill = adjustcolor(cluster_colors_palette[1:num_clusters], alpha.f = 0.3),
            col = cluster_colors_palette[1:num_clusters],
            lty = 1, lwd = 2, cex = legend_cex_to_use, box.lty = 0, bg = NA,
            ncol = 2, xpd = NA, inset = c(0, -0.15)
        )
    }
  }
  return(list(plot_fn = plot_function, width = plot_width, height = plot_height))
}

create_box_plots_091 <- function(k_val, data_for_boxplot, output_dir) {
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
  
  # Adjusted box plot dimensions
  boxplot_width <- 8.5  # 2 columns
  boxplot_height <- 3 * num_rows_layout  # 3 inches per row
  
  # Define box plot function for reuse
  plot_boxplots <- function() {
    par(mfrow = c(num_rows_layout, 2), mar = c(4, 4, 2, 1))
    
    cluster_colors_bp <- brewer.pal(n = min(max(k_val, 3), 9), name = "Set1")
     if (k_val > length(cluster_colors_bp)) {
        cluster_colors_bp <- rep(cluster_colors_bp, length.out = k_val)
    }

    for (var_name in vars_for_plot) {
      var_display_name <- gsub("_Log$", " (Log Scale)", var_name) %>% gsub("_", " ", .)
      
      formula_bp <- reformulate("factor(Cluster)", response = var_name)
      boxplot(formula_bp, data = data_for_boxplot_mod, 
              main = var_display_name, xlab = "Cluster", ylab = var_display_name, 
              col = cluster_colors_bp, cex.axis = 1.0, cex.lab = 1.0, cex.main = 1.0, outline = TRUE)
      
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
  }
  
  save_plot_both_formats(plot_boxplots, file.path(box_plot_dir_specific, "organization_structure_combined_boxplot"), boxplot_width, boxplot_height)
  par(mfrow = c(1,1)) # Reset layout
} 

# 5. Generate and Save Charts for k=4 ---------------------------------------
cat("\n--- Generating charts for k=", K_VALUE_FIXED, " ---\n")

generate_all_charts_for_k <- function(k_val, centroids_df, medians_df, full_cluster_df, output_fig_dir) {
  cat("\nProcessing charts for k =", k_val, "\n")
  
  # Charts based on MEANS (centroids)
  cat("  Generating charts based on MEANS (centroids)...\n")
  dir_means <- file.path(output_fig_dir, paste0("k", k_val, "_means"))
  if (!dir.exists(dir_means)) dir.create(dir_means, recursive = TRUE)
  
  if (length(likert_vars_all) > 0) {
      radar_all_likert_means <- create_radar_chart(centroids_df, likert_vars_all, title_main = paste0("All Likert Vars (k=", k_val, ") - Means"))
      if(!is.null(radar_all_likert_means)) {
        save_plot_both_formats(radar_all_likert_means$plot_fn, file.path(dir_means, "radar_all_likert_means"), radar_all_likert_means$width, radar_all_likert_means$height)
      }
  }
  if (length(group1_vars) > 0) {
      radar_g1_means <- create_radar_chart(centroids_df, group1_vars, title_main = paste0("Group 1: Decision Making Distribution & Style (k=", k_val, ") - Means"))
      if(!is.null(radar_g1_means)) {
        save_plot_both_formats(radar_g1_means$plot_fn, file.path(dir_means, "radar_group1_means"), radar_g1_means$width, radar_g1_means$height)
      }
  }
  if (length(group2_vars) > 0) {
      radar_g2_means <- create_radar_chart(centroids_df, group2_vars, title_main = paste0("Group 2: Decision Making Culture, Flex & Env (k=", k_val, ") - Means"))
      if(!is.null(radar_g2_means)) {
        save_plot_both_formats(radar_g2_means$plot_fn, file.path(dir_means, "radar_group2_means"), radar_g2_means$width, radar_g2_means$height)
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
      radar_all_likert_medians <- create_radar_chart(medians_df, likert_vars_all, title_main = paste0("All Likert Vars (k=", k_val, ") - Medians"))
      if(!is.null(radar_all_likert_medians)) {
        save_plot_both_formats(radar_all_likert_medians$plot_fn, file.path(dir_medians, "radar_all_likert_medians"), radar_all_likert_medians$width, radar_all_likert_medians$height)
      }
  }
  if (length(group1_vars) > 0) {
      radar_g1_medians <- create_radar_chart(medians_df, group1_vars, title_main = paste0("Group 1: Decision Making Distribution & Style (k=", k_val, ") - Medians"))
      if(!is.null(radar_g1_medians)) {
        save_plot_both_formats(radar_g1_medians$plot_fn, file.path(dir_medians, "radar_group1_medians"), radar_g1_medians$width, radar_g1_medians$height)
      }
  }
  if (length(group2_vars) > 0) {
      radar_g2_medians <- create_radar_chart(medians_df, group2_vars, title_main = paste0("Group 2: Decision Making Culture, Flex & Env (k=", k_val, ") - Medians"))
      if(!is.null(radar_g2_medians)) {
        save_plot_both_formats(radar_g2_medians$plot_fn, file.path(dir_medians, "radar_group2_medians"), radar_g2_medians$width, radar_g2_medians$height)
      }
  }
   if (!is.null(project_success_var) && project_success_var %in% colnames(medians_df)){
      cat("  (Skipping median-based radar chart for Project_Success as it's categorical text)\n")
  }
  
  # Combined grid plot for Group1 and Group2 medians with shared legend
  cat("  Generating combined grid plot for Group1 and Group2 (Medians) with shared legend...\n")
  if (length(group1_vars) > 0 && length(group2_vars) > 0) {
    plot_combined_groups_medians <- function() {
      # ★★★ 关键修复：允许所有图形绘制在整个画布，而不是被剪切 ★★★
      par(xpd = NA)
      
      # 保持整体布局，稍微增加绘图区而非外边距
      par(mfrow = c(1, 2), oma = c(2, 1, 1, 1), mar = c(1.2, 1.2, 1.2, 1.2))
      
      # Define cluster colors
      if (k_val == 4) {
        cluster_colors_palette <- brewer.pal(4, "Set2")
      } else {
        cluster_colors_palette <- brewer.pal(n = min(max(k_val, 3), 9), name = "Set1")
        if (k_val > length(cluster_colors_palette)) {
          cluster_colors_palette <- rep(cluster_colors_palette, length.out = k_val)
        }
      }
      
      ##### --- Plot Group 1 --- #####
      vars_to_use_g1 <- group1_vars[group1_vars %in% colnames(medians_df)]
      if (length(vars_to_use_g1) > 0) {
        radar_df_g1 <- medians_df %>% select(Cluster, all_of(vars_to_use_g1)) %>% arrange(Cluster)
        data_for_fmsb_g1 <- radar_df_g1 %>% select(-Cluster) %>% as.data.frame()
        data_for_fmsb_g1 <- rbind(rep(5, ncol(data_for_fmsb_g1)),
                                  rep(1, ncol(data_for_fmsb_g1)),
                                  data_for_fmsb_g1)
        formatted_labels_g1 <- format_variable_names(vars_to_use_g1)
        
        radarchart(data_for_fmsb_g1,
                   pfcol = adjustcolor(cluster_colors_palette[1:k_val], alpha.f = 0.3),
                   pcol = cluster_colors_palette[1:k_val],
                   plty = 1, plwd = 2.5,
                   cglcol = "gray70", cglty = 1,
                   axislabcol = "gray30",
                   calcex = 1.0, vlcex = 0.9,
                   caxislabels = seq(1, 5, 1),
                   vlabels = formatted_labels_g1)
      }
      
      ##### --- Plot Group 2 --- #####
      vars_to_use_g2 <- group2_vars[group2_vars %in% colnames(medians_df)]
      if (length(vars_to_use_g2) > 0) {
        radar_df_g2 <- medians_df %>% select(Cluster, all_of(vars_to_use_g2)) %>% arrange(Cluster)
        data_for_fmsb_g2 <- radar_df_g2 %>% select(-Cluster) %>% as.data.frame()
        data_for_fmsb_g2 <- rbind(rep(5, ncol(data_for_fmsb_g2)),
                                  rep(1, ncol(data_for_fmsb_g2)),
                                  data_for_fmsb_g2)
        formatted_labels_g2 <- format_variable_names(vars_to_use_g2)
        
        radarchart(data_for_fmsb_g2,
                   pfcol = adjustcolor(cluster_colors_palette[1:k_val], alpha.f = 0.3),
                   pcol = cluster_colors_palette[1:k_val],
                   plty = 1, plwd = 2.5,
                   cglcol = "gray70", cglty = 1,
                   axislabcol = "gray30",
                   calcex = 1.0, vlcex = 0.9,
                   caxislabels = seq(1, 5, 1),
                   vlabels = formatted_labels_g2)
      }
      
      ##### --- Add shared legend (不会被裁切) --- #####
      par(fig = c(0, 1, 0, 1), new = TRUE, oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0))
      plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
      
      cluster_names <- c(
        "Cluster 1: Adaptive Learning (n=3)",
        "Cluster 2: Integrated-Innovative (n=24)",
        "Cluster 3: Moderate Collaborative (n=53)",
        "Cluster 4: Risk-averse Command (n=28)"
      )
      
      legend("bottom",
             legend = cluster_names,
             fill = adjustcolor(cluster_colors_palette[1:k_val], alpha.f = 0.3),
             col = cluster_colors_palette[1:k_val],
             lty = 1, lwd = 2,
             cex = 0.95,
             ncol = 2,
             box.lty = 0,
             xpd = NA,      # ★关键：传奇永不被裁切
             inset = c(0, 0))
    }
    
    save_plot_both_formats(
      plot_combined_groups_medians,
      file.path(dir_medians, "radar_group1_group2_combined_medians"),
      9, 4
    )
  }
  
  # Individual cluster combined radar charts (Group1 + Group2 for each cluster, no legend)
  cat("  Generating individual cluster combined radar charts (Group1 + Group2 side-by-side, Medians, no legend)...\n")
  if (length(group1_vars) > 0 && length(group2_vars) > 0) {
    # Define cluster colors
    if (k_val == 4) {
      cluster_colors_palette <- brewer.pal(4, "Set2")
    } else {
      cluster_colors_palette <- brewer.pal(n = min(max(k_val, 3), 9), name = "Set1")
      if (k_val > length(cluster_colors_palette)) {
        cluster_colors_palette <- rep(cluster_colors_palette, length.out = k_val)
      }
    }
    
    for (cl_idx in 1:k_val) {
      plot_cluster_combined <- function() {
        par(xpd = NA)
        par(mfrow = c(1, 2), oma = c(0, 1, 1, 1), mar = c(1.2, 1.2, 1.2, 1.2))
        
        # Get data for this cluster
        cluster_data_single <- medians_df %>% filter(Cluster == cl_idx)
        
        if (nrow(cluster_data_single) > 0) {
          # Plot Group 1 for this cluster
          vars_to_use_g1 <- group1_vars[group1_vars %in% colnames(cluster_data_single)]
          if (length(vars_to_use_g1) > 0) {
            case_values_g1 <- as.numeric(cluster_data_single[1, vars_to_use_g1])
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
              pfcol = adjustcolor(cluster_colors_palette[cl_idx], alpha.f = 0.3),
              pcol = cluster_colors_palette[cl_idx],
              plty = 1,
              plwd = 2.5,
              cglcol = "gray70",
              cglty = 1,
              axislabcol = "gray30",
              calcex = 1.0,
              vlcex = 0.9,
              caxislabels = seq(1, 5, 1),
              vlabels = formatted_labels_g1
            )
          }
          
          # Plot Group 2 for this cluster
          vars_to_use_g2 <- group2_vars[group2_vars %in% colnames(cluster_data_single)]
          if (length(vars_to_use_g2) > 0) {
            case_values_g2 <- as.numeric(cluster_data_single[1, vars_to_use_g2])
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
              pfcol = adjustcolor(cluster_colors_palette[cl_idx], alpha.f = 0.3),
              pcol = cluster_colors_palette[cl_idx],
              plty = 1,
              plwd = 2.5,
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
      }
      
      save_plot_both_formats(
        plot_cluster_combined,
        file.path(dir_medians, paste0("radar_cluster", cl_idx, "_group1_group2_combined_medians")),
        9, 4
      )
    }
  }

  # Individual cluster radar charts (e.g. for All Likert Vars)
  cat("  Generating individual radar charts for each cluster (All Likert Vars - Means & Medians)...\n")
  for (cl_idx in 1:k_val) {
    # Means - All Likert
    cluster_centroid_data_all_likert <- centroids_df %>% filter(Cluster == cl_idx)
    if(nrow(cluster_centroid_data_all_likert) > 0 && length(likert_vars_all) > 0) {
      ind_radar_means_all_likert <- create_radar_chart(cluster_centroid_data_all_likert, likert_vars_all, title_main = paste0("All Likert Vars - Profile for Cluster ", cl_idx, " (k=", k_val, ") - Means"))
      if(!is.null(ind_radar_means_all_likert)) {
        save_plot_both_formats(ind_radar_means_all_likert$plot_fn, file.path(dir_means, paste0("radar_cluster", cl_idx, "_all_likert_means")), ind_radar_means_all_likert$width, ind_radar_means_all_likert$height)
      }
    }
    # Medians - All Likert
    cluster_median_data_all_likert <- medians_df %>% filter(Cluster == cl_idx)
    if(nrow(cluster_median_data_all_likert) > 0 && length(likert_vars_all) > 0) {
      ind_radar_medians_all_likert <- create_radar_chart(cluster_median_data_all_likert, likert_vars_all, title_main = paste0("All Likert Vars - Profile for Cluster ", cl_idx, " (k=", k_val, ") - Medians"))
      if(!is.null(ind_radar_medians_all_likert)) {
        save_plot_both_formats(ind_radar_medians_all_likert$plot_fn, file.path(dir_medians, paste0("radar_cluster", cl_idx, "_all_likert_medians")), ind_radar_medians_all_likert$width, ind_radar_medians_all_likert$height)
      }
    }
  }

  # NEW SECTION: Non-overlayed (grid) plots for Group1 and Group2 by cluster
  cat("  Generating non-overlayed grid plots for Group1 & Group2 by cluster (Means & Medians)...\n")
  # Define layout for 4 clusters (k_val)
  # Adjust num_cols and num_rows if k_val is different from 4 in other uses of this script
  num_plot_cols <- 2
  num_plot_rows <- ceiling(k_val / num_plot_cols)
  grid_plot_width <- 8.5  # 2 columns, each 4.25 inches
  grid_plot_height <- 6   # 2 rows, each 3 inches
  small_plot_vlcex <- 0.8 # Variable label cex for small plots (10pt base)
  small_plot_calcex <- 0.8 # Axis label cex for small plots (10pt base)

  # Define the same color palette as used in the main radar charts
  if (k_val == 4) {
    # Use RColorBrewer's "Set2" palette for K=4 for distinct colors
    cluster_colors_grid <- brewer.pal(4, "Set2") 
  } else {
    # Default palette for other numbers of clusters
    cluster_colors_grid <- brewer.pal(n = min(max(k_val, 3), 9), name = "Set1")
    if (k_val > length(cluster_colors_grid)) {
        cluster_colors_grid <- rep(cluster_colors_grid, length.out = k_val)
    }
  }

  # Define cluster names with sample sizes
  cluster_names <- c(
    "Cluster 1: Adaptive Learning (n=3)",
    "Cluster 2: Integrated-Innovative (n=24)", 
    "Cluster 3: Moderate Collaborative (n=53)",
    "Cluster 4: Risk-averse Command (n=28)"
  )

  # --- For MEANS data ---
  # Group 1 - Means - Grid
  if (length(group1_vars) > 0) {
      # Define plot function for reuse
      plot_g1_means_grid <- function() {
        par(mfrow = c(num_plot_rows, num_plot_cols), oma = c(0, 0, 2, 0), mar = c(0.5, 0.5, 2, 0.5))
        for (cl_idx in 1:k_val) {
            cluster_data_single <- centroids_df %>% filter(Cluster == cl_idx)
            if(nrow(cluster_data_single) > 0){
                plot_title_g1m <- cluster_names[cl_idx]
                
                # Create single-cluster radar chart with cluster-specific color
                vars_to_use <- group1_vars[group1_vars %in% colnames(cluster_data_single)]
                if (length(vars_to_use) > 0) {
                  case_values <- as.numeric(cluster_data_single[1, vars_to_use])
                  data_for_fmsb <- data.frame(
                    rbind(
                      rep(5, length(vars_to_use)),  # max
                      rep(1, length(vars_to_use)),  # min
                      case_values                   # actual case
                    )
                  )
                  colnames(data_for_fmsb) <- vars_to_use
                  formatted_labels <- format_variable_names(vars_to_use)
                  
                  radarchart(
                    data_for_fmsb,
                    pfcol = adjustcolor(cluster_colors_grid[cl_idx], alpha.f = 0.3),
                    pcol = cluster_colors_grid[cl_idx],
                    plty = 1, 
                    plwd = 2.5,
                    cglcol = "gray70", 
                    cglty = 1,
                    axislabcol = "gray30", 
                    calcex = small_plot_calcex, 
                    vlcex = small_plot_vlcex, 
                    caxislabels = seq(1, 5, 1),
                    vlabels = formatted_labels
                  )
                  title(main = plot_title_g1m, cex.main = 1.0)
                } else {
                  plot.new(); text(0.5,0.5, "No Data"); title(plot_title_g1m)
                }
            } else {
                plot.new(); text(0.5,0.5, paste0("No Data for Cl. ", cl_idx)); title(cluster_names[cl_idx])
            }
        }
        title(paste0("Group 1 (Decision Making Distribution & Style) - Means by Cluster (k=", k_val, ")"), outer = TRUE, cex.main = 1.2)
      }
      
      save_plot_both_formats(plot_g1_means_grid, file.path(dir_means, "grid_all_clusters_group1_means"), grid_plot_width, grid_plot_height)
      par(mfrow = c(1, 1)) # Reset layout
  }

  # Group 2 - Means - Grid
  if (length(group2_vars) > 0) {
      # Define plot function for reuse
      plot_g2_means_grid <- function() {
        par(mfrow = c(num_plot_rows, num_plot_cols), oma = c(0, 0, 2, 0), mar = c(0.5, 0.5, 2, 0.5))
        for (cl_idx in 1:k_val) {
            cluster_data_single <- centroids_df %>% filter(Cluster == cl_idx)
            if(nrow(cluster_data_single) > 0){
                plot_title_g2m <- cluster_names[cl_idx]
                
                # Create single-cluster radar chart with cluster-specific color
                vars_to_use <- group2_vars[group2_vars %in% colnames(cluster_data_single)]
                if (length(vars_to_use) > 0) {
                  case_values <- as.numeric(cluster_data_single[1, vars_to_use])
                  data_for_fmsb <- data.frame(
                    rbind(
                      rep(5, length(vars_to_use)),  # max
                      rep(1, length(vars_to_use)),  # min
                      case_values                   # actual case
                    )
                  )
                  colnames(data_for_fmsb) <- vars_to_use
                  formatted_labels <- format_variable_names(vars_to_use)
                  
                  radarchart(
                    data_for_fmsb,
                    pfcol = adjustcolor(cluster_colors_grid[cl_idx], alpha.f = 0.3),
                    pcol = cluster_colors_grid[cl_idx],
                    plty = 1, 
                    plwd = 2.5,
                    cglcol = "gray70", 
                    cglty = 1,
                    axislabcol = "gray30", 
                    calcex = small_plot_calcex, 
                    vlcex = small_plot_vlcex, 
                    caxislabels = seq(1, 5, 1),
                    vlabels = formatted_labels
                  )
                  title(main = plot_title_g2m, cex.main = 1.0)
                } else {
                  plot.new(); text(0.5,0.5, "No Data"); title(plot_title_g2m)
                }
            } else {
                plot.new(); text(0.5,0.5, paste0("No Data for Cl. ", cl_idx)); title(cluster_names[cl_idx])
            }
        }
        title(paste0("Group 2 (Decision Making Culture, Flex & Env) - Means by Cluster (k=", k_val, ")"), outer = TRUE, cex.main = 1.2)
      }
      
      save_plot_both_formats(plot_g2_means_grid, file.path(dir_means, "grid_all_clusters_group2_means"), grid_plot_width, grid_plot_height)
      par(mfrow = c(1, 1)) # Reset layout
  }

  # --- For MEDIANS data ---
  # Group 1 - Medians - Grid
  if (length(group1_vars) > 0) {
      # Define plot function for reuse
      plot_g1_medians_grid <- function() {
        par(mfrow = c(num_plot_rows, num_plot_cols), oma = c(0, 0, 2, 0), mar = c(0.5, 0.5, 2, 0.5))
        for (cl_idx in 1:k_val) {
            cluster_data_single <- medians_df %>% filter(Cluster == cl_idx)
            if(nrow(cluster_data_single) > 0){
                plot_title_g1med <- cluster_names[cl_idx]
                
                # Create single-cluster radar chart with cluster-specific color
                vars_to_use <- group1_vars[group1_vars %in% colnames(cluster_data_single)]
                if (length(vars_to_use) > 0) {
                  case_values <- as.numeric(cluster_data_single[1, vars_to_use])
                  data_for_fmsb <- data.frame(
                    rbind(
                      rep(5, length(vars_to_use)),  # max
                      rep(1, length(vars_to_use)),  # min
                      case_values                   # actual case
                    )
                  )
                  colnames(data_for_fmsb) <- vars_to_use
                  formatted_labels <- format_variable_names(vars_to_use)
                  
                  radarchart(
                    data_for_fmsb,
                    pfcol = adjustcolor(cluster_colors_grid[cl_idx], alpha.f = 0.3),
                    pcol = cluster_colors_grid[cl_idx],
                    plty = 1, 
                    plwd = 2.5,
                    cglcol = "gray70", 
                    cglty = 1,
                    axislabcol = "gray30", 
                    calcex = small_plot_calcex, 
                    vlcex = small_plot_vlcex, 
                    caxislabels = seq(1, 5, 1),
                    vlabels = formatted_labels
                  )
                  title(main = plot_title_g1med, cex.main = 1.0)
                } else {
                  plot.new(); text(0.5,0.5, "No Data"); title(plot_title_g1med)
                }
            } else {
                plot.new(); text(0.5,0.5, paste0("No Data for Cl. ", cl_idx)); title(cluster_names[cl_idx])
            }
        }
        title(paste0("Group 1 (Decision Making Distribution & Style) - Medians by Cluster (k=", k_val, ")"), outer = TRUE, cex.main = 1.2)
      }
      
      save_plot_both_formats(plot_g1_medians_grid, file.path(dir_medians, "grid_all_clusters_group1_medians"), grid_plot_width, grid_plot_height)
      par(mfrow = c(1, 1)) # Reset layout
  }

  # Group 2 - Medians - Grid
  if (length(group2_vars) > 0) {
      # Define plot function for reuse
      plot_g2_medians_grid <- function() {
        par(mfrow = c(num_plot_rows, num_plot_cols), oma = c(0, 0, 2, 0), mar = c(0.5, 0.5, 2, 0.5))
        for (cl_idx in 1:k_val) {
            cluster_data_single <- medians_df %>% filter(Cluster == cl_idx)
            if(nrow(cluster_data_single) > 0){
                plot_title_g2med <- cluster_names[cl_idx]
                
                # Create single-cluster radar chart with cluster-specific color
                vars_to_use <- group2_vars[group2_vars %in% colnames(cluster_data_single)]
                if (length(vars_to_use) > 0) {
                  case_values <- as.numeric(cluster_data_single[1, vars_to_use])
                  data_for_fmsb <- data.frame(
                    rbind(
                      rep(5, length(vars_to_use)),  # max
                      rep(1, length(vars_to_use)),  # min
                      case_values                   # actual case
                    )
                  )
                  colnames(data_for_fmsb) <- vars_to_use
                  formatted_labels <- format_variable_names(vars_to_use)
                  
                  radarchart(
                    data_for_fmsb,
                    pfcol = adjustcolor(cluster_colors_grid[cl_idx], alpha.f = 0.3),
                    pcol = cluster_colors_grid[cl_idx],
                    plty = 1, 
                    plwd = 2.5,
                    cglcol = "gray70", 
                    cglty = 1,
                    axislabcol = "gray30", 
                    calcex = small_plot_calcex, 
                    vlcex = small_plot_vlcex, 
                    caxislabels = seq(1, 5, 1),
                    vlabels = formatted_labels
                  )
                  title(main = plot_title_g2med, cex.main = 1.0)
                } else {
                  plot.new(); text(0.5,0.5, "No Data"); title(plot_title_g2med)
                }
            } else {
                plot.new(); text(0.5,0.5, paste0("No Data for Cl. ", cl_idx)); title(cluster_names[cl_idx])
            }
        }
        title(paste0("Group 2 (Decision Making Culture, Flex & Env) - Medians by Cluster (k=", k_val, ")"), outer = TRUE, cex.main = 1.2)
      }
      
      save_plot_both_formats(plot_g2_medians_grid, file.path(dir_medians, "grid_all_clusters_group2_medians"), grid_plot_width, grid_plot_height)
      par(mfrow = c(1, 1)) # Reset layout
  }

  # Box Plots for ORG_ variables
  create_box_plots_091(k_val = k_val, data_for_boxplot = full_cluster_df, output_dir = output_fig_dir)
  
  cat("Finished charts for k =", k_val, "\n")
}

# Generate all charts for k=4
if (!is.null(centroids_k4_data) && !is.null(medians_k4_data) && !is.null(clustered_full_data_k4)) {
  generate_all_charts_for_k(K_VALUE_FIXED, centroids_k4_data, medians_k4_data, clustered_full_data_k4, fig_dir_091)
} else {
  cat("Error: One or more required dataframes (centroids, medians, clustered_full_data) for k=", K_VALUE_FIXED, " is NULL. Skipping chart generation.\n")
}

# 6. Export Data Tables (Means/Medians for groups) --------------------------
cat("\n--- Exporting data tables (means/medians for variable groups) to:", tbl_dir_091, " ---\n")

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

# Export for k=4
# Means
export_grouped_data(K_VALUE_FIXED, centroids_k4_data, "All Likert Vars", likert_vars_all, file_prefix = "means", output_tbl_dir = tbl_dir_091)
export_grouped_data(K_VALUE_FIXED, centroids_k4_data, "Group 1 Decision Style", group1_vars, file_prefix = "means", output_tbl_dir = tbl_dir_091)
export_grouped_data(K_VALUE_FIXED, centroids_k4_data, "Group 2 Culture Flex Env", group2_vars, file_prefix = "means", output_tbl_dir = tbl_dir_091)
if(!is.null(project_success_var) && project_success_var %in% colnames(centroids_k4_data)) export_grouped_data(K_VALUE_FIXED, centroids_k4_data, "Project Success", project_success_var, file_prefix = "means", output_tbl_dir = tbl_dir_091) # This will show modes

# Medians
export_grouped_data(K_VALUE_FIXED, medians_k4_data, "All Likert Vars", likert_vars_all, file_prefix = "medians", output_tbl_dir = tbl_dir_091)
export_grouped_data(K_VALUE_FIXED, medians_k4_data, "Group 1 Decision Style", group1_vars, file_prefix = "medians", output_tbl_dir = tbl_dir_091)
export_grouped_data(K_VALUE_FIXED, medians_k4_data, "Group 2 Culture Flex Env", group2_vars, file_prefix = "medians", output_tbl_dir = tbl_dir_091)
if(!is.null(project_success_var) && project_success_var %in% colnames(medians_k4_data)) export_grouped_data(K_VALUE_FIXED, medians_k4_data, "Project Success", project_success_var, file_prefix = "medians", output_tbl_dir = tbl_dir_091) # This will show modes

# 7. Export ORG Structure Variables by Cluster (4 separate tables) ----------
cat("\n--- Exporting ORG structure variables by cluster (4 separate tables) ---\n")

export_org_vars_by_cluster <- function(k_val, cluster_data, org_vars_list, output_tbl_dir) {
  if (is.null(cluster_data) || nrow(cluster_data) == 0) {
    cat("  Error: cluster_data is NULL or empty. Cannot export ORG variables by cluster.\n")
    return()
  }
  
  if (!"Cluster" %in% colnames(cluster_data)) {
    cat("  Error: 'Cluster' column not found in cluster_data. Cannot export ORG variables by cluster.\n")
    return()
  }
  
  # Filter to only existing ORG variables
  existing_org_vars <- org_vars_list[org_vars_list %in% colnames(cluster_data)]
  if (length(existing_org_vars) == 0) {
    cat("  No ORG variables found in the data. Skipping ORG variables export by cluster.\n")
    return()
  }
  
  cat("  Exporting ORG structure variables:", paste(existing_org_vars, collapse = ", "), "\n")
  
  # Create summary table for all clusters first
  org_summary_all <- data.frame()
  
  for (cl_idx in 1:k_val) {
    cluster_subset <- cluster_data %>% filter(Cluster == cl_idx)
    
    if (nrow(cluster_subset) == 0) {
      cat("    Warning: No data found for Cluster", cl_idx, "\n")
      next
    }
    
    cat("    Processing Cluster", cl_idx, "- Sample size:", nrow(cluster_subset), "\n")
    
    # Calculate medians for each ORG variable
    cluster_medians <- cluster_subset %>%
      select(all_of(existing_org_vars)) %>%
      summarise(across(everything(), ~ median(.x, na.rm = TRUE))) %>%
      mutate(
        Cluster = cl_idx,
        Sample_Size = nrow(cluster_subset),
        .before = 1
      )
    
    # Add to summary table
    org_summary_all <- rbind(org_summary_all, cluster_medians)
    
    # Export individual cluster table
    individual_cluster_data <- cluster_subset %>%
      select(all_of(c("Cluster", existing_org_vars))) %>%
      arrange(across(all_of(existing_org_vars)))
    
    # Add summary row with medians at the top
    summary_row <- data.frame(
      Cluster = paste0("Cluster_", cl_idx, "_Median"),
      stringsAsFactors = FALSE
    )
    
    for (var in existing_org_vars) {
      summary_row[[var]] <- median(cluster_subset[[var]], na.rm = TRUE)
    }
    
    # Combine summary and individual data
    individual_cluster_export <- rbind(summary_row, individual_cluster_data)
    
    # Export individual cluster file
    individual_file_path <- file.path(output_tbl_dir, paste0("org_structure_cluster", cl_idx, "_k", k_val, "_medians.csv"))
    write.csv(individual_cluster_export, individual_file_path, row.names = FALSE)
    cat("      Exported Cluster", cl_idx, "ORG structure data to:", individual_file_path, "\n")
  }
  
  # Export combined summary table with all clusters
  if (nrow(org_summary_all) > 0) {
    # Add cluster names for better readability
    cluster_names_mapping <- c(
      "Cluster 1: Adaptive Learning",
      "Cluster 2: Integrated-Innovative", 
      "Cluster 3: Moderate Collaborative",
      "Cluster 4: Risk-averse Command"
    )
    
    org_summary_all$Cluster_Name <- cluster_names_mapping[org_summary_all$Cluster]
    org_summary_all <- org_summary_all %>%
      select(Cluster, Cluster_Name, Sample_Size, everything())
    
    summary_file_path <- file.path(output_tbl_dir, paste0("org_structure_all_clusters_k", k_val, "_medians_summary.csv"))
    write.csv(org_summary_all, summary_file_path, row.names = FALSE)
    cat("  Exported combined ORG structure summary for all clusters to:", summary_file_path, "\n")
    
    # Print summary to console
    cat("\n  ORG Structure Variables Summary (Medians by Cluster):\n")
    print(org_summary_all)
  }
}

# Export ORG structure variables by cluster for k=4
if (!is.null(clustered_full_data_k4)) {
  export_org_vars_by_cluster(K_VALUE_FIXED, clustered_full_data_k4, org_structure_vars, tbl_dir_091)
} else {
  cat("  Error: clustered_full_data_k4 is NULL. Cannot export ORG variables by cluster.\n")
}

cat("
Chart and table generation for k=", K_VALUE_FIXED, " post 1 case removal completed.
Results saved to respective subdirectories under results/figures and results/tables.
ORG structure variables exported as 4 separate cluster tables plus 1 summary table.
")
cat("============== SCRIPT R/091 (Radar Charts Post 1 Removal k4) FINISHED ==============\n") 