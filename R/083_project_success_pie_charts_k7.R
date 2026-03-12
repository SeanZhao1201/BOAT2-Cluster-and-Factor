# R/083_project_success_pie_charts_k7.R
# This script visualizes the distribution of Project_Success for each of the k=7 clusters
# using pie charts. Data is loaded from the output of script 081.

# 0. Load Setup and Data -----------------------------------------------------
cat("============== SCRIPT R/083 (Project Success Pie Charts k7) STARTING ==============\n")

# Ensure essential packages are loaded
if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
if (!requireNamespace("readr", quietly = TRUE)) install.packages("readr")
if (!requireNamespace("RColorBrewer", quietly = TRUE)) install.packages("RColorBrewer")
if (!requireNamespace("gridExtra", quietly = TRUE)) install.packages("gridExtra")
if (!requireNamespace("tidyr", quietly = TRUE)) install.packages("tidyr")

library(dplyr)
library(ggplot2)
library(readr)
library(RColorBrewer)
library(gridExtra)
library(tidyr)

# Attempt to source setup file
if (file.exists("R/000_setup.R")) {
  source("R/000_setup.R")
  cat("R/000_setup.R sourced successfully.\n")
} else {
  cat("Warning: R/000_setup.R not found. Essential packages loaded directly.\n")
}

# 1. Configuration ----------------------------------------------------------
K_VALUE_FIXED <- 7
INPUT_TABLES_DIR_081 <- paste0("results/tables/081_kprototype_post_1_removal_k", K_VALUE_FIXED)
CLUSTER_DATA_FILENAME <- paste0("kproto_clusters_k", K_VALUE_FIXED, "_post_1_removed.csv")
CLUSTER_DATA_FILEPATH <- file.path(INPUT_TABLES_DIR_081, CLUSTER_DATA_FILENAME)

# Create subdirectory for results from this script (083)
FIG_DIR_083 <- paste0("results/figures/083_project_success_pie_charts_k", K_VALUE_FIXED)
if (!dir.exists(FIG_DIR_083)) {
  dir.create(FIG_DIR_083, recursive = TRUE)
  cat(paste("Created directory:", FIG_DIR_083, "\n"))
}

# Project Success levels and color palette
PROJECT_SUCCESS_LEVELS <- c("very successful", "more successful", "moderately successful", "more unsuccessful", "very unsuccessful")
SUCCESS_COLOR_PALETTE <- rev(brewer.pal(length(PROJECT_SUCCESS_LEVELS), "RdYlGn"))
names(SUCCESS_COLOR_PALETTE) <- PROJECT_SUCCESS_LEVELS

cat("Configuration complete. Output will be saved to:", FIG_DIR_083, "\n")

# 2. Load Data --------------------------------------------------------------
cat("\n--- Loading cluster data from:", CLUSTER_DATA_FILEPATH, " ---\n")
if (!file.exists(CLUSTER_DATA_FILEPATH)) {
  stop(paste("Error: Cluster data file not found at", CLUSTER_DATA_FILEPATH))
}
clustered_data <- read_csv(CLUSTER_DATA_FILEPATH, show_col_types = FALSE)
cat("Loaded clustered data with", nrow(clustered_data), "rows and", ncol(clustered_data), "columns.\n")

# Standardize cluster column name
cluster_col_original <- paste0("Cluster_k", K_VALUE_FIXED)
if (cluster_col_original %in% colnames(clustered_data)) {
  clustered_data <- clustered_data %>%
    rename(Cluster = !!sym(cluster_col_original))
  cat("Renamed cluster column '", cluster_col_original, "' to 'Cluster'.\n")
} else if (!"Cluster" %in% colnames(clustered_data)){
  stop("Cluster assignment column not found in the loaded data.")
}

# Ensure Project_Success is a factor with the specified order
if (!"Project_Success" %in% colnames(clustered_data)) {
  stop("Error: 'Project_Success' column not found in the data.")
}
clustered_data$Project_Success <- factor(clustered_data$Project_Success, levels = PROJECT_SUCCESS_LEVELS, ordered = TRUE)
cat("'Project_Success' column converted to ordered factor.\n")

# 3. Generate Pie Charts for each Cluster ------------------------------------
cat("\n--- Generating Project Success pie charts for each cluster ---\n")
plots_list <- list()

for (cluster_num in 1:K_VALUE_FIXED) {
  cat("  Processing Cluster", cluster_num, "...\n")
  
  cluster_subset <- clustered_data %>%
    filter(Cluster == cluster_num)
  
  if (nrow(cluster_subset) == 0) {
    cat("    Warning: No data found for Cluster", cluster_num, ". Skipping pie chart.\n")
    next
  }
  
  success_summary <- cluster_subset %>%
    group_by(Project_Success) %>%
    summarise(count = n(), .groups = 'drop') %>%
    tidyr::complete(Project_Success = PROJECT_SUCCESS_LEVELS, fill = list(count = 0)) %>%
    mutate(
      percentage = count / sum(count) * 100,
      Project_Success = factor(Project_Success, levels = PROJECT_SUCCESS_LEVELS),
      label = ifelse(count > 0, paste0(count, " (", sprintf("%.1f%%", percentage), ")"), "")
    ) %>%
    arrange(Project_Success)

  # Create pie chart with improved label positioning and style
  pie_chart <- ggplot(success_summary, aes(x = "", y = percentage, fill = Project_Success)) +
    geom_bar(stat = "identity", width = 0.9, color = "white") +
    coord_polar("y", start = 0) +  # Start at 3 o'clock position
    geom_text(data = subset(success_summary, count > 0),
              aes(label = label),
              position = position_stack(vjust = 0.5),
              color = "black",
              size = 4,
              fontface = "bold") +
    scale_fill_manual(values = SUCCESS_COLOR_PALETTE, 
                     name = "Project Success",
                     breaks = PROJECT_SUCCESS_LEVELS,
                     labels = PROJECT_SUCCESS_LEVELS,
                     drop = FALSE) +
    labs(title = paste0("Project Success Distribution\nCluster ", cluster_num),
         subtitle = paste0("Total Projects: n=", sum(success_summary$count)),
         x = NULL, y = NULL) +
    theme_void() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 16, face = "bold", margin = margin(b = 10)),
      plot.subtitle = element_text(hjust = 0.5, size = 12, margin = margin(b = 20)),
      legend.title = element_text(size = 12, face = "bold"),
      legend.text = element_text(size = 10),
      legend.position = "right",
      legend.key.size = unit(0.8, "cm"),
      legend.margin = margin(l = 0, r = 10, t = 0, b = 0),
      legend.spacing.y = unit(0.3, "cm"),
      plot.margin = unit(c(1, 1, 1, 1), "cm")
    )
  
  # Save individual pie chart
  individual_plot_filename <- file.path(FIG_DIR_083, 
                                      paste0("cluster_", cluster_num, "_pie_chart.pdf"))
  
  # Save with specific dimensions for better quality
  ggsave(individual_plot_filename, 
         plot = pie_chart,
         width = 10, 
         height = 8,
         units = "in",
         device = "pdf")
  
  cat(paste("  Saved pie chart for Cluster", cluster_num, "to:", individual_plot_filename, "\n"))
  
  # Store plot for combined view (optional)
  plots_list[[paste0("cluster_", cluster_num)]] <- pie_chart
}

# 4. Save Combined Plot (Optional) ------------------------------------------
cat("\n--- Saving combined plot (optional) ---\n")

if (length(plots_list) > 0) {
  num_cols_grid <- 3
  num_rows_grid <- ceiling(length(plots_list) / num_cols_grid)
  
  combined_plot_filename <- file.path(FIG_DIR_083, 
                                    paste0("all_clusters_pie_charts_k", K_VALUE_FIXED, ".pdf"))
  
  # Save combined plot with larger dimensions
  pdf(combined_plot_filename, width = num_cols_grid * 7, height = num_rows_grid * 6)
  
  # 将所有图形的图例设置为右侧且紧凑
  for (i in 1:length(plots_list)) {
    plots_list[[i]] <- plots_list[[i]] + 
      theme(legend.position = "right",
            legend.key.size = unit(0.8, "cm"),
            legend.margin = margin(l = 0, r = 10, t = 0, b = 0),
            legend.spacing.y = unit(0.3, "cm"))
  }
  
  grid.arrange(
    grobs = plots_list, 
    ncol = num_cols_grid,
    top = textGrob(paste0("Project Success Distribution by Cluster (k=", K_VALUE_FIXED, ")"), 
                   gp = gpar(fontsize = 20, fontface = "bold"))
  )
  dev.off()
  
  cat("Combined pie chart plot saved to:", combined_plot_filename, "\n")
} else {
  cat("No plots were generated to save.\n")
}

cat("\n============== SCRIPT R/083 (Project Success Pie Charts k7) FINISHED ==============\n")