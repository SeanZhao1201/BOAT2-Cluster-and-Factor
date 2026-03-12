# BOAT2 Cluster and Factor Analysis - Radar Chart Visualization
# This script creates radar chart visualizations for the k-prototype clustering results

# 0. Check and Install Required Packages -------------------------------------
if (!requireNamespace("fmsb", quietly = TRUE)) {
  cat("Installing fmsb package...\n")
  install.packages("fmsb")
}
library(fmsb)
library(RColorBrewer) # For color palettes

# Add test function before main function
test_csv_reading <- function() {
  cat("\n---------- Starting CSV file reading test ----------\n")
  
  # Try reading the medians file
  file_path <- "results/tables/032_kprototype_analysis/kproto_medians_k2.csv"
  
  if (file.exists(file_path)) {
    cat("File exists:", file_path, "\n")
    
    # Directly read the first few lines of the file
    file_lines <- readLines(file_path, n = 5)
    cat("File content preview:\n")
    for (line in file_lines) {
      cat("  ", line, "\n") 
    }
    
    # Try different CSV reading parameters
    cat("\nMethod 1 - Standard read.csv:\n")
    df1 <- tryCatch({
      result <- read.csv(file_path, stringsAsFactors = FALSE)
      print(str(result))
      print(colnames(result))
      result
    }, error = function(e) {
      cat("Error:", e$message, "\n")
      NULL
    })
    
    cat("\nMethod 2 - read.csv with quote handling:\n")
    df2 <- tryCatch({
      result <- read.csv(file_path, stringsAsFactors = FALSE, quote = "\"")
      print(str(result))
      print(colnames(result))
      result
    }, error = function(e) {
      cat("Error:", e$message, "\n")
      NULL
    })
    
    cat("\nMethod 3 - Using read.table:\n")
    df3 <- tryCatch({
      result <- read.table(file_path, header = TRUE, sep = ",", quote = "\"", stringsAsFactors = FALSE)
      print(str(result))
      print(colnames(result))
      result
    }, error = function(e) {
      cat("Error:", e$message, "\n")
      NULL
    })
    
    cat("\nMethod 4 - Using readr package:\n")
    # Check if readr is loaded
    if(requireNamespace("readr", quietly = TRUE)) {
      df4 <- tryCatch({
        result <- readr::read_csv(file_path)
        print(str(result))
        print(colnames(result))
        result
      }, error = function(e) {
        cat("Error:", e$message, "\n")
        NULL
      })
    } else {
      cat("readr package not loaded, skipping method 4\n")
    }
    
    # Return successful dataframe
    successful_df <- NULL
    if (!is.null(df1) && is.data.frame(df1)) successful_df <- df1
    else if (!is.null(df2) && is.data.frame(df2)) successful_df <- df2
    else if (!is.null(df3) && is.data.frame(df3)) successful_df <- df3
    else if (exists("df4") && !is.null(df4)) successful_df <- as.data.frame(df4)
    
    if (!is.null(successful_df)) {
      cat("\nSuccessfully read dataframe example (first 2 rows):\n")
      print(head(successful_df, 2))
      return(successful_df)
    } else {
      cat("\nAll reading methods failed\n")
      return(NULL)
    }
  } else {
    cat("File does not exist:", file_path, "\n")
    return(NULL)
  }
}

# 1. Load Setup and Data -----------------------------------------------------
source("R/000_setup.R")

# Create subdirectories for results if they don't exist
dirs <- c(
  "results/figures/034_radar_charts",
  "results/tables/034_radar_charts"
)

for (dir in dirs) {
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE)
    cat(paste("Created directory:", dir, "\n"))
  }
}

# Define k values to analyze (same as in the k-prototype analysis)
kproto_k_values <- c(2, 3, 4, 5)
cat("Will generate radar charts for k values:", paste(kproto_k_values, collapse = ", "), "\n")

# Execute CSV test reading
test_result <- test_csv_reading()
if (!is.null(test_result)) {
  cat("CSV test reading successful, continuing script execution\n")
} else {
  cat("CSV test reading failed, script may not work properly\n")
}

# 2. Load Clustering Results -------------------------------------------------
cat("\nLoading k-prototype clustering results...\n")

# Function to load centroids data for a specific k value
load_centroids <- function(k) {
  # Set the correct file path based on k value
  if (k == 5) {
    file_path <- paste0("results/tables/032_A_kprototype_analysis/kproto_centroids_k", k, ".csv")
  } else {
    file_path <- paste0("results/tables/032_kprototype_analysis/kproto_centroids_k", k, ".csv")
  }
  
  if (file.exists(file_path)) {
    # Add more debug information
    cat("  Reading centroids file:", file_path, "\n")
    
    # Directly read and print the first few lines of the file for debugging
    file_lines <- readLines(file_path, n = 5)
    cat("  File content preview:\n")
    for (line in file_lines) {
      cat("    ", line, "\n") 
    }
    
    # Ensure read.csv uses standard parameters
    centroids <- read.csv(file_path, stringsAsFactors = FALSE, check.names = FALSE)
    
    # Print the structure of the data after reading
    cat("  Loaded data structure:\n")
    print(str(centroids))
    cat("  Column names:\n")
    print(colnames(centroids))
    
    # Ensure Cluster column is numeric
    centroids$Cluster <- as.numeric(centroids$Cluster)
    cat("  Loaded centroids for k =", k, "\n")
    return(centroids)
  } else {
    cat("  Error: Could not find centroids file for k =", k, "\n")
    return(NULL)
  }
}

# Function to load medians data for a specific k value
load_medians <- function(k) {
  # Set the correct file path based on k value
  if (k == 5) {
    file_path <- paste0("results/tables/032_A_kprototype_analysis/kproto_medians_k", k, ".csv")
  } else {
    file_path <- paste0("results/tables/032_kprototype_analysis/kproto_medians_k", k, ".csv")
  }
  
  if (file.exists(file_path)) {
    # Add more debug information
    cat("  Reading medians file:", file_path, "\n")
    
    # Directly read and print the first few lines of the file for debugging
    file_lines <- readLines(file_path, n = 5)
    cat("  File content preview:\n")
    for (line in file_lines) {
      cat("    ", line, "\n") 
    }
    
    # Ensure read.csv uses standard parameters
    medians <- read.csv(file_path, stringsAsFactors = FALSE, check.names = FALSE)
    
    # Print the structure of the data after reading
    cat("  Loaded data structure:\n")
    print(str(medians))
    cat("  Column names:\n")
    print(colnames(medians))
    
    # Ensure Cluster column is numeric
    medians$Cluster <- as.numeric(medians$Cluster)
    cat("  Loaded medians for k =", k, "\n")
    return(medians)
  } else {
    cat("  Error: Could not find medians file for k =", k, "\n")
    return(NULL)
  }
}

# Function to load full cluster data (for box plots)
load_cluster_data <- function(k) {
  # Set the correct file path based on k value
  if (k == 5) {
    file_path <- paste0("results/tables/032_A_kprototype_analysis/kproto_clusters_k", k, ".csv")
  } else {
    file_path <- paste0("results/tables/032_kprototype_analysis/kproto_clusters_k", k, ".csv")
  }
  
  if (file.exists(file_path)) {
    clusters <- read.csv(file_path)
    cat("  Loaded cluster data for k =", k, "\n")
    return(clusters)
  } else {
    cat("  Error: Could not find cluster data file for k =", k, "\n")
    return(NULL)
  }
}

# New: Create specialized function for Box Plot
create_box_plots <- function(k, cluster_data) {
  cat("\nCreating box plots for k =", k, "...\n")
  
  # Confirm if cluster_data is valid
  if (is.null(cluster_data) || !is.data.frame(cluster_data)) {
    cat("  Error: Invalid or NULL cluster data\n")
    return(NULL)
  }
  
  # Create Box Plot specific directory
  box_dir <- paste0("results/figures/034_radar_charts/k", k, "_box_plot")
  if (!dir.exists(box_dir)) {
    dir.create(box_dir, recursive = TRUE)
    cat("  Created box plot directory:", box_dir, "\n")
  }
  
  # Only get organization structure variables starting with ORG_
  org_vars <- grep("^ORG_", names(cluster_data), value = TRUE)
  cat("  Only generating box plots for organization structure variables:", paste(org_vars, collapse=", "), "\n")
  
  if (length(org_vars) == 0) {
    cat("  Error: No organization structure variables (ORG_) found in the dataset\n")
    return(NULL)
  }
  
  # Handle discrete organization size variables
  # For better visualization, compress large organization sizes to a reasonable range
  if ("ORG_Employees" %in% org_vars) {
    # Create log-transformed version of organization size for better box plot display
    cat("  Creating log-transformed version of ORG_Employees for better visualization\n")
    cluster_data$ORG_Employees_Log <- log1p(cluster_data$ORG_Employees)
    # Add this variable to org_vars
    org_vars <- c(org_vars, "ORG_Employees_Log")
  }
  
  # Create combined box plot for all ORG_ variables
  combined_pdf <- paste0(box_dir, "/organization_structure_combined_boxplot.pdf")
  
  # Calculate needed rows
  n_vars <- length(org_vars)
  if ("ORG_Employees" %in% org_vars && "ORG_Employees_Log" %in% org_vars) {
    n_vars <- n_vars - 1  # Subtract one as we won't use the original ORG_Employees
  }
  
  n_rows <- ceiling(n_vars / 2)  # 2 plots per row
  
  # Create combined plot
  pdf(combined_pdf, width = 16, height = 6 * n_rows)
  par(mfrow = c(n_rows, 2), mar = c(5, 5, 4, 2) + 0.1)
  
  for (var in org_vars) {
    # Skip the original ORG_Employees if we have the log version
    if (var == "ORG_Employees" && "ORG_Employees_Log" %in% org_vars) {
      next  # Skip, only use the log-transformed version
    }
    
    var_display <- gsub("_Log$", " (Log Scale)", var)
    var_display <- gsub("_", " ", var_display)  # Replace underscores with spaces for friendlier labels
    
    # Prepare data
    plot_data <- cluster_data[, c("Cluster", var)]
    
    # Handle outliers
    upper_limit <- NULL
    if (var != "ORG_Employees_Log" && var %in% c("ORG_Employees", "ORG_Locations", "ORG_Departments")) {
      # Apply cap to large organization variables
      q3 <- quantile(plot_data[[var]], 0.75, na.rm = TRUE)
      iqr <- IQR(plot_data[[var]], na.rm = TRUE)
      upper_limit <- q3 + 1.5 * iqr
      
      # Create capped version for visualization
      plot_data$capped_value <- pmin(plot_data[[var]], upper_limit)
      var_to_plot <- "capped_value"
      var_display <- paste0(var_display, " (Capped)")
    } else {
      var_to_plot <- var
    }
    
    # First draw the basic boxplot without outliers
    boxplot(
      reformulate("Cluster", var_to_plot), 
      data = plot_data,
      main = var_display,
      xlab = "Cluster",
      ylab = var_display,
      col = brewer.pal(n = min(max(k, 3), 9), name = "Set1"),
      cex.axis = 1.2,
      cex.lab = 1.3,
      cex.main = 1.4,
      outline = FALSE,  # No outliers in the initial boxplot
      axes = (var != "ORG_Employees_Log") # Don't show default axes for log variable
    )
    
    # Add jittered outliers manually
    for (i in 1:k) {
      # Get data for this cluster
      cluster_i_data <- plot_data[plot_data$Cluster == i, ]
      
      # Calculate boxplot stats to identify outliers
      box_stats <- boxplot.stats(cluster_i_data[[var_to_plot]])
      outliers <- box_stats$out
      
      if (length(outliers) > 0) {
        # Create x positions with jitter
        x_pos <- jitter(rep(i, length(outliers)), amount = 0.2)
        
        # Plot outliers with jitter
        points(x_pos, outliers, pch = 19, col = adjustcolor("black", alpha.f = 0.5), cex = 0.8)
      }
    }
    
    # Add more meaningful labels for log axis
    if (var == "ORG_Employees_Log") {
      # Get current axis range
      log_range <- range(plot_data[[var]], na.rm = TRUE)
      # Create appropriate ticks
      log_breaks <- seq(from = floor(log_range[1]), to = ceiling(log_range[2]), by = 1)
      # Original values (exponentiate back)
      orig_values <- round(expm1(log_breaks))
      # Custom labels
      axis(2, at = log_breaks, labels = paste0(round(log_breaks, 1), "\n(", orig_values, ")"), las = 1, cex.axis = 1.1)
      # Add grid lines
      abline(h = log_breaks, col = "lightgray", lty = 3)
    }
    
    # If there's an upper limit, add limit annotation
    if (!is.null(upper_limit)) {
      mtext(paste("Values capped at", round(upper_limit, 1)), side = 3, line = 0.5, cex = 0.9, col = "red")
    }
    
    # Add median value labels
    med_vals <- tapply(plot_data[[var]], plot_data$Cluster, median, na.rm = TRUE)
    if (var == "ORG_Employees_Log") {
      # For log values, show both transformed and original values
      med_pos <- med_vals + 0.1 * diff(range(plot_data[[var_to_plot]], na.rm = TRUE))
      text_labels <- sprintf("%.1f\n(%.0f)", med_vals, expm1(med_vals))
      text(1:k, med_pos, text_labels, cex = 1.1)
    } else {
      # For normal values, just show original
      text(1:k, med_vals + 0.1 * diff(range(plot_data[[var_to_plot]], na.rm = TRUE)), 
           sprintf("%.2f", med_vals), cex = 1.1)
    }
  }
  
  dev.off()
  cat("  Combined organization structure box plot saved to:", combined_pdf, "\n")
  
  cat("  Box plots creation completed for k =", k, "\n")
}

# Load data for each k value
centroids_list <- list()
medians_list <- list()  # New medians list
cluster_data_list <- list()

for (k in kproto_k_values) {
  centroids_list[[paste0("k", k)]] <- load_centroids(k)
  medians_list[[paste0("k", k)]] <- load_medians(k)  # Load medians data
  cluster_data_list[[paste0("k", k)]] <- load_cluster_data(k)
}

# 3. Define Variable Groups for Radar Charts ---------------------------------
cat("\nOrganizing variables for radar charts...\n")

# Define numeric organizational structure variables (separate visualization)
org_structure_vars <- c(
  "ORG_Employees",
  "ORG_Locations",
  "ORG_Departments",
  "ORG_Hierarchy_Layers"
)

# Define Group 1: Decision and Style variables (Likert scale)
group1_vars <- c(
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
  "STY_Authoritative_Compliance"
)

# Define Group 2: Culture, Flexibility, Risk and Environment variables (Likert scale)
group2_vars <- c(
  # Organization culture variables
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

# Create a list of variable groups (new organization)
variable_groups <- list(
  "Organization Structure (Numeric)" = org_structure_vars,
  "Group 1 (Decision & Style)" = group1_vars,
  "Group 2 (Culture, Flexibility & Environment)" = group2_vars
)

# Also create a list of all Likert scale variables for combined charts
likert_vars <- c(group1_vars, group2_vars)

# 4. Create Improved Radar Chart Function with Variable Name Formatting -----------------

# Function to format variable names with line breaks
format_variable_names <- function(var_names) {
  # Replace underscore with newline character
  formatted_names <- var_names
  
  # Create mapping for specific variable pattern replacements
  name_mappings <- list(
    "STY_Analytical_DataDriven" = "STY\nAnalytical\nDataDriven",
    "STY_Participative_Inclusion" = "STY\nParticipative\nInclusion",
    "STY_Participative_Relational" = "STY\nParticipative\nRelational",
    "STY_Organic_InformalStructure" = "STY\nOrganic\nInformalStructure",
    "STY_Organic_Adaptability" = "STY\nOrganic\nAdaptability",
    "STY_Directive_Threats" = "STY\nDirective\nThreats",
    "STY_Directive_Compliance" = "STY\nDirective\nCompliance",
    
    "DEC_Authority_Dispersion" = "DEC\nAuthority\nDispersion",
    "DEC_Authority_Delegation" = "DEC\nAuthority\nDelegation",
    "DEC_Process_InformalCommunication" = "DEC\nProcess\nInformalComm",
    "DEC_Process_InformalProcedures" = "DEC\nProcess\nInformalProc",
    
    "CUL_Authority_Hierarchical" = "CUL\nAuthority\nHierarchical",
    "CUL_Integration_Vision" = "CUL\nIntegration\nVision",
    "CUL_Integration_Systematic" = "CUL\nIntegration\nSystematic",
    "CUL_Innovation_Experimental" = "CUL\nInnovation\nExperimental",
    "CUL_Collaboration_Stakeholder" = "CUL\nCollaboration\nStakeholder",
    
    "FLEX_Cognitive_Receptivity" = "FLEX\nCognitive\nReceptivity",
    "FLEX_Behavioral_Adaptability" = "FLEX\nBehavioral\nAdaptability",
    
    "RISK_Appetite_Investment" = "RISK\nAppetite\nInvestment",
    
    "ENV_Context_Growth" = "ENV\nContext\nGrowth",
    "ENV_Context_Volatility" = "ENV\nContext\nVolatility",
    "ENV_Context_Stability" = "ENV\nContext\nStability",
    
    "ORG_Employees" = "ORG\nEmployees",
    "ORG_Locations" = "ORG\nLocations",
    "ORG_Departments" = "ORG\nDepartments",
    "ORG_Hierarchy_Layers" = "ORG\nHierarchy\nLayers"
  )
  
  # Apply mappings for original variable names
  for (i in seq_along(var_names)) {
    if (var_names[i] %in% names(name_mappings)) {
      formatted_names[i] <- name_mappings[[var_names[i]]]
    } else {
      # Default formatting for other variables
      formatted_names[i] <- gsub("_", "\n", var_names[i])
    }
  }
  
  return(formatted_names)
}

# Function to create a radar chart for a given dataset and variables
create_radar_chart <- function(data, variables, 
                               title = "Radar Chart", 
                               scale_min = 1, 
                               scale_max = 5,
                               width = 14,  # Increased width for better variable name display
                               height = 10) {
  
  # Simplify: Keep only variables that exist in the data
  vars_to_use <- variables[variables %in% colnames(data)]
  original_vars <- vars_to_use  # Keep original names for reference
  
  if (length(vars_to_use) == 0) {
    stop("None of the specified variables exist in the dataset")
  }
  
  # Create data frame for radar chart
  n_clusters <- nrow(data)
  radar_data <- data.frame(matrix(NA, nrow = n_clusters, ncol = length(vars_to_use)))
  colnames(radar_data) <- vars_to_use
  rownames(radar_data) <- paste("Cluster", data$Cluster)
  
  # Fill in the data
  for (i in 1:nrow(data)) {
    for (j in 1:length(vars_to_use)) {
      var <- vars_to_use[j]
      radar_data[i, j] <- data[i, var]
    }
  }
  
  # Add max and min rows
  radar_data <- rbind(
    rep(scale_max, ncol(radar_data)),
    rep(scale_min, ncol(radar_data)),
    radar_data
  )
  rownames(radar_data)[1:2] <- c("max", "min")
  
  # Create formatted labels with line breaks
  display_labels <- format_variable_names(colnames(radar_data))
  colnames(radar_data) <- display_labels
  
  # Define colors for clusters
  if (n_clusters <= 2) {
    cluster_colors <- c("#4285F4", "#EA4335")
  } else {
    cluster_colors <- brewer.pal(n = min(max(n_clusters, 3), 9), name = "Set1")
  }
  
  # Create plotting function
  plot_fn <- function() {
    # Set up plot margins - increased for wider display
    par(mar = c(2, 2, 3, 2))
    
    # Plot the radar chart
    radarchart(
      radar_data,
      pfcol = adjustcolor(cluster_colors[1:n_clusters], alpha.f = 0.3),
      pcol = cluster_colors[1:n_clusters],
      plty = 1,
      plwd = 2.5,
      cglcol = "gray70",
      cglty = 1,
      axislabcol = "gray30",
      calcex = 1.2,  # 增大坐标轴标签字体大小，从0.8到1.2
      vlcex = 1.3,  # 增大变量标签字体大小，从0.9到1.3
      caxislabels = seq(scale_min, scale_max, (scale_max - scale_min) / 4),
      title = title,
      axistype = 1,  # Use axis labels
      titlecex = 1.4  # 增大标题字体大小
    )
    
    # NOTE: Removed value labels as requested
    
    # Add legend
    if (n_clusters > 1) {
      legend(
        "bottomright",
        legend = paste("Cluster", data$Cluster),
        fill = adjustcolor(cluster_colors[1:n_clusters], alpha.f = 0.3),
        col = cluster_colors[1:n_clusters],
        lty = 1,
        lwd = 2,
        cex = 1.2,  # 增大图例字体大小，从0.8到1.2
        box.lty = 0
      )
    }
  }
  
  # Return both the plotting function and the dimensions
  return(list(
    plot_fn = plot_fn,
    width = width,
    height = height
  ))
}

# 5. Generate Radar Charts and Box Plots for Each k Value --------------------
cat("\nGenerating charts...\n")

# Function to generate radar charts and box plots for a specific k value
generate_charts <- function(k, centroids, medians, cluster_data, use_medians = FALSE) {
  cat("\nCreating charts for k =", k, "...\n")
  
  # Determine which data source to use based on the parameter
  data_type <- ifelse(use_medians, "medians", "centroids")
  # Directly assign the data source based on the parameter
  data_source <- if (use_medians) medians else centroids
  dir_suffix <- ifelse(use_medians, "_median", "")
  
  # Check if data_source is valid
  if (is.null(data_source) || !is.data.frame(data_source)) {
    cat("  Error: Invalid or NULL data source for", data_type, "\n")
    return(NULL)
  }
  
  # Print column names of data_source for debugging
  cat("  Data source column names:\n")
  print(colnames(data_source))
  cat("  Data source structure:\n")
  print(str(data_source))
  cat("  Data source head:\n")
  print(head(data_source))
  
  # Create directory for k-specific charts
  k_dir <- paste0("results/figures/034_radar_charts/k", k, dir_suffix)
  if (!dir.exists(k_dir)) {
    dir.create(k_dir, recursive = TRUE)
  }
  
  # 1. Create overall radar chart with all Likert scale variables
  cat("  Creating charts for all Likert scale variables...\n")
  
  # Check if all variables exist in the dataset
  missing_vars <- likert_vars[!likert_vars %in% colnames(data_source)]
  if (length(missing_vars) > 0) {
    cat("  Warning: Missing variables:", paste(missing_vars, collapse = ", "), "\n")
    likert_vars_filtered <- likert_vars[likert_vars %in% colnames(data_source)]
    
    if (length(likert_vars_filtered) == 0) {
      cat("  Error: No Likert variables found in dataset. Skipping Likert radar charts.\n")
    } else {
      cat("  Using", length(likert_vars_filtered), "available Likert variables for chart.\n")
      
      # Create radar chart for all Likert variables
      likert_radar <- create_radar_chart(
        data_source,
        likert_vars_filtered,
        title = paste0("Likert Scale Variables (k=", k, ")", ifelse(use_medians, " - Medians", " - Means"))
      )
      
      # Save the Likert variables radar chart
      pdf(paste0(k_dir, "/likert_variables_radar.pdf"), width = likert_radar$width, height = likert_radar$height)
      likert_radar$plot_fn()
      dev.off()
      cat("  Likert scale variables radar chart saved to:", paste0(k_dir, "/likert_variables_radar.pdf"), "\n")
    }
  } else {
    # Create radar chart for all Likert variables
    likert_radar <- create_radar_chart(
      data_source,
      likert_vars,
      title = paste0("Likert Scale Variables (k=", k, ")", ifelse(use_medians, " - Medians", " - Means"))
    )
    
    # Save the Likert variables radar chart
    pdf(paste0(k_dir, "/likert_variables_radar.pdf"), width = likert_radar$width, height = likert_radar$height)
    likert_radar$plot_fn()
    dev.off()
    cat("  Likert scale variables radar chart saved to:", paste0(k_dir, "/likert_variables_radar.pdf"), "\n")
  }
  
  # 2. Create radar chart for Group 1 variables (Decision & Style)
  cat("  Creating charts for Group 1 (Decision & Style) variables...\n")
  group1_vars_filtered <- group1_vars[group1_vars %in% colnames(data_source)]
  if (length(group1_vars_filtered) > 0) {
    group1_radar <- create_radar_chart(
      data_source,
      group1_vars_filtered,
      title = paste0("Group 1: Decision & Style (k=", k, ")", ifelse(use_medians, " - Medians", " - Means"))
    )
    
    # Save the Group 1 variables radar chart
    pdf(paste0(k_dir, "/group1_radar.pdf"), width = group1_radar$width, height = group1_radar$height)
    group1_radar$plot_fn()
    dev.off()
    cat("  Group 1 variables radar chart saved to:", paste0(k_dir, "/group1_radar.pdf"), "\n")
  } else {
    cat("  Warning: No Group 1 variables found in dataset. Skipping Group 1 radar charts.\n")
  }
  
  # 3. Create radar chart for Group 2 variables (Culture, Flexibility & Environment)
  cat("  Creating charts for Group 2 (Culture, Flexibility & Environment) variables...\n")
  group2_vars_filtered <- group2_vars[group2_vars %in% colnames(data_source)]
  if (length(group2_vars_filtered) > 0) {
    group2_radar <- create_radar_chart(
      data_source,
      group2_vars_filtered,
      title = paste0("Group 2: Culture, Flexibility & Environment (k=", k, ")", ifelse(use_medians, " - Medians", " - Means"))
    )
    
    # Save the Group 2 variables radar chart
    pdf(paste0(k_dir, "/group2_radar.pdf"), width = group2_radar$width, height = group2_radar$height)
    group2_radar$plot_fn()
    dev.off()
    cat("  Group 2 variables radar chart saved to:", paste0(k_dir, "/group2_radar.pdf"), "\n")
  } else {
    cat("  Warning: No Group 2 variables found in dataset. Skipping Group 2 radar charts.\n")
  }
  
  # 4. Create individual radar charts for each cluster (using Likert variables only)
  for (cluster_num in 1:k) {
    cat("  Processing cluster", cluster_num, "...\n")
    
    # Filter data for this cluster - using safer subsetting method
    if ("Cluster" %in% names(data_source)) {
      cluster_data_filtered <- subset(data_source, Cluster == cluster_num)
      cat("    Found", nrow(cluster_data_filtered), "rows for cluster", cluster_num, "\n")
    } else {
      cat("    Error: No 'Cluster' column found in data source\n")
      print(names(data_source))
      next
    }
    
    # If there are matching Likert variables, create radar chart
    likert_vars_available <- likert_vars[likert_vars %in% colnames(data_source)]
    if (length(likert_vars_available) > 0) {
      # Create radar chart for this cluster (Likert variables only)
      cluster_radar <- create_radar_chart(
        cluster_data_filtered,
        likert_vars_available,
        title = paste0("Cluster ", cluster_num, " Profile (k=", k, ")",
                     ifelse(use_medians, " - Medians", " - Means"))
      )
      
      # Save the cluster radar chart
      pdf_file <- paste0(k_dir, "/cluster", cluster_num, "_radar.pdf")
      pdf(pdf_file, width = cluster_radar$width, height = cluster_radar$height)
      cluster_radar$plot_fn()
      dev.off()
      cat("  Cluster", cluster_num, "radar chart saved to:", pdf_file, "\n")
      
      # Create Group 1 radar chart for this cluster
      if (length(group1_vars_filtered) > 0) {
        cluster_group1_radar <- create_radar_chart(
          cluster_data_filtered,
          group1_vars_filtered,
          title = paste0("Cluster ", cluster_num, " - Group 1 (k=", k, ")",
                       ifelse(use_medians, " - Medians", " - Means"))
        )
        
        # Save the cluster Group 1 radar chart
        pdf_file <- paste0(k_dir, "/cluster", cluster_num, "_group1_radar.pdf")
        pdf(pdf_file, width = cluster_group1_radar$width, height = cluster_group1_radar$height)
        cluster_group1_radar$plot_fn()
        dev.off()
        cat("  Cluster", cluster_num, "Group 1 radar chart saved to:", pdf_file, "\n")
      }
      
      # Create Group 2 radar chart for this cluster
      if (length(group2_vars_filtered) > 0) {
        cluster_group2_radar <- create_radar_chart(
          cluster_data_filtered,
          group2_vars_filtered,
          title = paste0("Cluster ", cluster_num, " - Group 2 (k=", k, ")",
                       ifelse(use_medians, " - Medians", " - Means"))
        )
        
        # Save the cluster Group 2 radar chart
        pdf_file <- paste0(k_dir, "/cluster", cluster_num, "_group2_radar.pdf")
        pdf(pdf_file, width = cluster_group2_radar$width, height = cluster_group2_radar$height)
        cluster_group2_radar$plot_fn()
        dev.off()
        cat("  Cluster", cluster_num, "Group 2 radar chart saved to:", pdf_file, "\n")
      }
    } else {
      cat("  Warning: No Likert variables found for Cluster", cluster_num, ". Skipping radar chart.\n")
    }
  }
  
  # Return radars (if they exist)
  return(list(
    likert_radar = if(exists("likert_radar")) likert_radar else NULL,
    group1_radar = if(exists("group1_radar")) group1_radar else NULL,
    group2_radar = if(exists("group2_radar")) group2_radar else NULL
  ))
}

# Generate charts for each k value
chart_results <- list()
chart_results_median <- list()  # Store medians radar chart results

for (k in kproto_k_values) {
  centroids_data <- centroids_list[[paste0("k", k)]]
  medians_data <- medians_list[[paste0("k", k)]]
  cluster_data <- cluster_data_list[[paste0("k", k)]]
  
  if (!is.null(centroids_data) && !is.null(cluster_data)) {
    # Generate radar chart based on means (centroids)
    cat("\nGenerating radar chart based on means (centroids) for k =", k, "...\n")
    chart_results[[paste0("k", k)]] <- generate_charts(
      k = k,
      centroids = centroids_data,
      medians = medians_data,
      cluster_data = cluster_data,
      use_medians = FALSE
    )
    
    # Generate radar chart based on medians
    if (!is.null(medians_data)) {
      cat("\nGenerating radar chart based on medians for k =", k, "...\n")
      chart_results_median[[paste0("k", k)]] <- generate_charts(
        k = k,
        centroids = centroids_data,
        medians = medians_data,
        cluster_data = cluster_data,
        use_medians = TRUE
      )
    }
    
    # Generate box plot
    cat("\nGenerating Box Plot visualization for k =", k, "...\n")
    create_box_plots(
      k = k,
      cluster_data = cluster_data
    )
  } else {
    cat("\nSkipping k =", k, "because data is missing\n")
  }
}

# 6. Create Comparative Visualizations ---------------------------------------
cat("\nCreating comparative visualizations...\n")

# Generate three comparative visualizations: one based on all Likert variables, one based on Group 1 variables, one based on Group 2 variables
# Each comparative visualization has two versions: one based on means, one based on medians
for (data_type in c("means", "medians")) {
  use_medians <- data_type == "medians"
  results_list <- if(use_medians) chart_results_median else chart_results
  
  # Skip if there's no data for this type
  if (length(results_list) == 0) {
    cat("   No", data_type, "type data available for comparative visualization\n")
    next
  }
  
  # Create comparative visualization for all Likert variables
  create_comparative_visualization <- function(chart_type, title_suffix, width = 18, height = 14) {
    pdf_file <- paste0("results/figures/034_radar_charts/comparative_", chart_type, "_radar_", data_type, ".pdf")
    
    # Check if any of the results have the necessary radar chart
    has_charts <- FALSE
    for (k in kproto_k_values) {
      key <- paste0("k", k)
      if (key %in% names(results_list) && !is.null(results_list[[key]]) && !is.null(results_list[[key]][[chart_type]])) {
        has_charts <- TRUE
        break
      }
    }
    
    if (!has_charts) {
      cat("  No", chart_type, "charts available for", data_type, "visualization\n")
      return(invisible(NULL))
    }
    
    pdf(pdf_file, width = width, height = height)
    
    # Set up multi-panel layout based on number of charts
    n_panels <- length(kproto_k_values)
    if (n_panels <= 2) {
      par(mfrow = c(1, 2))  # 1x2 layout for 1-2 charts
    } else if (n_panels <= 4) {
      par(mfrow = c(2, 2))  # 2x2 layout for 3-4 charts
    } else {
      par(mfrow = c(2, 3))  # 2x3 layout for 5-6 charts
    }
    
    # Plot each k value radar chart
    for (k in kproto_k_values) {
      key <- paste0("k", k)
      if (key %in% names(results_list) && !is.null(results_list[[key]]) && !is.null(results_list[[key]][[chart_type]])) {
        results_list[[key]][[chart_type]]$plot_fn()
        title(main = paste0("k=", k, title_suffix), line = -1)
      }
    }
    
    # Reset layout
    par(mfrow = c(1, 1))
    
    # Close the PDF
    dev.off()
    cat("  Comparative", chart_type, "radar chart (", data_type, ") saved to:", pdf_file, "\n")
  }
  
  # Create three comparative visualizations
  create_comparative_visualization("likert_radar", " - All Variables")
  create_comparative_visualization("group1_radar", " - Decision & Style")
  create_comparative_visualization("group2_radar", " - Culture, Flex & Env")
}

# 7. Export data tables for further analysis -----------------------------------
cat("\nExporting data tables for further analysis...\n")

# Make sure the tables directory exists
tables_dir <- "results/tables/034_radar_charts"
if (!dir.exists(tables_dir)) {
  dir.create(tables_dir, recursive = TRUE)
  cat("Created directory:", tables_dir, "\n")
}

# Function to export data tables for a specific k value
export_data_tables <- function(k) {
  cat("Exporting data tables for k =", k, "...\n")
  
  # Export means (centroids) data
  centroids_data <- centroids_list[[paste0("k", k)]]
  if (!is.null(centroids_data)) {
    centroids_file <- paste0(tables_dir, "/k", k, "_means.csv")
    write.csv(centroids_data, centroids_file, row.names = FALSE)
    cat("  Means (centroids) data for k =", k, "saved to:", centroids_file, "\n")
    
    # Also export means data for individual variable groups
    # Group 1 variables
    group1_vars_filtered <- group1_vars[group1_vars %in% colnames(centroids_data)]
    if (length(group1_vars_filtered) > 0) {
      group1_data <- centroids_data[, c("Cluster", group1_vars_filtered)]
      group1_file <- paste0(tables_dir, "/k", k, "_means_group1.csv")
      write.csv(group1_data, group1_file, row.names = FALSE)
      cat("  Group 1 means data for k =", k, "saved to:", group1_file, "\n")
    }
    
    # Group 2 variables
    group2_vars_filtered <- group2_vars[group2_vars %in% colnames(centroids_data)]
    if (length(group2_vars_filtered) > 0) {
      group2_data <- centroids_data[, c("Cluster", group2_vars_filtered)]
      group2_file <- paste0(tables_dir, "/k", k, "_means_group2.csv")
      write.csv(group2_data, group2_file, row.names = FALSE)
      cat("  Group 2 means data for k =", k, "saved to:", group2_file, "\n")
    }
  } else {
    cat("  No means (centroids) data available for k =", k, "\n")
  }
  
  # Export medians data
  medians_data <- medians_list[[paste0("k", k)]]
  if (!is.null(medians_data)) {
    medians_file <- paste0(tables_dir, "/k", k, "_medians.csv")
    write.csv(medians_data, medians_file, row.names = FALSE)
    cat("  Medians data for k =", k, "saved to:", medians_file, "\n")
    
    # Also export medians data for individual variable groups
    # Group 1 variables
    group1_vars_filtered <- group1_vars[group1_vars %in% colnames(medians_data)]
    if (length(group1_vars_filtered) > 0) {
      group1_data <- medians_data[, c("Cluster", group1_vars_filtered)]
      group1_file <- paste0(tables_dir, "/k", k, "_medians_group1.csv")
      write.csv(group1_data, group1_file, row.names = FALSE)
      cat("  Group 1 medians data for k =", k, "saved to:", group1_file, "\n")
    }
    
    # Group 2 variables
    group2_vars_filtered <- group2_vars[group2_vars %in% colnames(medians_data)]
    if (length(group2_vars_filtered) > 0) {
      group2_data <- medians_data[, c("Cluster", group2_vars_filtered)]
      group2_file <- paste0(tables_dir, "/k", k, "_medians_group2.csv")
      write.csv(group2_data, group2_file, row.names = FALSE)
      cat("  Group 2 medians data for k =", k, "saved to:", group2_file, "\n")
    }
  } else {
    cat("  No medians data available for k =", k, "\n")
  }
}

# Export data tables for each k value
for (k in kproto_k_values) {
  export_data_tables(k)
}

# Print final message
cat("\nChart visualization completed!\n")
cat("Visualization results saved to: results/figures/034_radar_charts/\n")
cat("Data tables saved to: results/tables/034_radar_charts/\n")

# Print radar chart customization options
cat("\nRadar chart customization options include:\n")
cat("1. Aspect ratio: Adjust width and height parameters\n")
cat("2. Variable name format: Modify name mappings in format_variable_names function\n")
cat("3. Color scheme: Modify cluster_colors variable\n")
cat("4. Transparency: Adjust alpha.f parameter in adjustcolor function\n")
cat("5. Line thickness: Adjust plwd parameter\n")
cat("6. Text size: Adjust vlcex (variable label) and calcex (axis label) parameters\n")
cat("7. Chart title: Set through title parameter\n")
cat("8. Grid line style: Set through cglcol and cglty parameters\n")
cat("9. Axis scale: Set through caxislabels parameter\n")
cat("10. Legend position and style: Set through legend function parameters\n")
cat("\nData types available:\n")
cat("- Means (centroids): results/figures/034_radar_charts/k2, k3/\n")
cat("- Medians: results/figures/034_radar_charts/k2_median, k3_median/\n") 