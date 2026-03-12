# R/101_pdm_by_success_level_pie_charts_k4.R
# This script visualizes PDM_Selected distribution for k=4 clusters, 
# further broken down by Project_Success levels (successful vs. unsuccessful).
# Data is loaded from the output of script 090.

# 0. Load Setup and Data -----------------------------------------------------
cat("============== SCRIPT R/101 (PDM by Success Level Pie Charts k4) STARTING ==============\n")

# Ensure essential packages are loaded
if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
if (!requireNamespace("readr", quietly = TRUE)) install.packages("readr")
if (!requireNamespace("RColorBrewer", quietly = TRUE)) install.packages("RColorBrewer")
if (!requireNamespace("gridExtra", quietly = TRUE)) install.packages("gridExtra")
if (!requireNamespace("grid", quietly = TRUE)) install.packages("grid")
if (!requireNamespace("tidyr", quietly = TRUE)) install.packages("tidyr")

library(dplyr)
library(ggplot2)
library(readr)
library(RColorBrewer)
library(gridExtra)
library(grid)
library(tidyr)

# Attempt to source setup file
if (file.exists("R/000_setup.R")) {
  source("R/000_setup.R")
  cat("R/000_setup.R sourced successfully.\n")
} else {
  cat("Warning: R/000_setup.R not found. Essential packages loaded directly.\n")
}

# 1. Configuration ----------------------------------------------------------
K_VALUE_FIXED <- 4
INPUT_TABLES_DIR_090 <- paste0("results/tables/090_kprototype_post_1_removal_k", K_VALUE_FIXED)
CLUSTER_DATA_FILENAME <- paste0("kproto_clusters_k", K_VALUE_FIXED, "_post_1_removed.csv")
CLUSTER_DATA_FILEPATH <- file.path(INPUT_TABLES_DIR_090, CLUSTER_DATA_FILENAME)

FIG_DIR_101 <- paste0("results/figures/101_pdm_by_success_k4/")
if (!dir.exists(FIG_DIR_101)) {
  dir.create(FIG_DIR_101, recursive = TRUE)
  cat(paste("Created directory:", FIG_DIR_101, "\n"))
}

# PDM_Selected levels in desired order of innovativeness (from R/094)
PDM_SELECTED_LEVELS <- c(
  "Design-Bid-Build", 
  "Construction Manager @ Risk",
  "Design-Build", 
  "Progressive Design-Build", 
  "Integrated Project Delivery (IPD)" 
)

# Custom progressive palette for PDM_Selected (from R/094)
custom_pdm_colors <- c(
  "#D73027", "#FC8D59", "#FEE08B", "#91BFDB", "#4575B4"
)
if (length(custom_pdm_colors) == length(PDM_SELECTED_LEVELS)) {
  PDM_SELECTED_COLOR_PALETTE <- custom_pdm_colors
  names(PDM_SELECTED_COLOR_PALETTE) <- PDM_SELECTED_LEVELS
} else {
  stop("Mismatch between PDM levels and custom colors.")
}

# Project Success Levels for filtering
PROJECT_SUCCESS_LEVELS_ALL <- c("very successful", "more successful", "moderately successful", "more unsuccessful", "very unsuccessful")
SUCCESSFUL_PROJECT_LEVELS <- c("very successful", "more successful", "moderately successful")
UNSUCCESSFUL_PROJECT_LEVELS <- c("very unsuccessful", "more unsuccessful")

pdm_palette_name_for_fallback <- "Set2" # For any unexpected PDM levels not in the custom list

cat("Configuration complete. Output will be saved to:", FIG_DIR_101, "\n")

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
  stop("Cluster assignment column '", cluster_col_original, "' or 'Cluster' not found in the loaded data.")
}

# Ensure Project_Success and PDM_Selected are present
if (!"Project_Success" %in% colnames(clustered_data)) stop("Error: 'Project_Success' column not found.")
if (!"PDM_Selected" %in% colnames(clustered_data)) stop("Error: 'PDM_Selected' column not found.")

# --- Display Data Types ---
cat("\n--- Data types for key columns: ---\n")
cat("Data type of Project_Success:", class(clustered_data$Project_Success), "\n")
cat("Data type of PDM_Selected:", class(clustered_data$PDM_Selected), "\n")
cat("Data type of Cluster:", class(clustered_data$Cluster), "\n")
cat("Unique values in Project_Success before factoring:", paste(unique(clustered_data$Project_Success), collapse=", "), "\n")
# --- End Display Data Types ---

# Factorize Project_Success
# Ensure the levels in the data match PROJECT_SUCCESS_LEVELS_ALL for successful factoring.
# If data uses lowercase, convert PROJECT_SUCCESS_LEVELS_ALL to lowercase or convert data column to title case.
# For now, assuming data matches the specified title case.
clustered_data$Project_Success <- factor(clustered_data$Project_Success, levels = PROJECT_SUCCESS_LEVELS_ALL)
# Check if all Project_Success values were mapped or became NA
if(any(is.na(clustered_data$Project_Success)) && !all(is.na(clustered_data$Project_Success))) {
  cat("Warning: Some Project_Success values became NA after factoring. Check levels.
")
  cat("Original unique values:", paste(unique(read_csv(CLUSTER_DATA_FILEPATH, show_col_types = FALSE)$Project_Success), collapse=", "), "\n")
  cat("Levels used for factoring:", paste(PROJECT_SUCCESS_LEVELS_ALL, collapse=", "), "\n")
}

# --- Logic for PDM_SELECTED_LEVELS_FINAL and dynamic palette (adapted from 094) ---
PDM_SELECTED_LEVELS_FINAL <- PDM_SELECTED_LEVELS
actual_pdm_levels_in_data <- unique(clustered_data$PDM_Selected)
missing_from_final_list <- setdiff(actual_pdm_levels_in_data, PDM_SELECTED_LEVELS_FINAL)
FINAL_PDM_COLOR_PALETTE <- PDM_SELECTED_COLOR_PALETTE

if(length(missing_from_final_list) > 0) {
  cat("Warning: The following PDM_Selected values from data were not in the predefined ordered list and will be appended:", 
      paste(missing_from_final_list, collapse=", "), "\n")
  PDM_SELECTED_LEVELS_FINAL <- c(PDM_SELECTED_LEVELS_FINAL, missing_from_final_list)
  
  num_pdm_levels_final <- length(PDM_SELECTED_LEVELS_FINAL)
  base_palette <- FINAL_PDM_COLOR_PALETTE[names(FINAL_PDM_COLOR_PALETTE) %in% PDM_SELECTED_LEVELS]
  newly_added_levels <- missing_from_final_list
  additional_colors_for_new_levels <- c()
  if(length(newly_added_levels) > 0) {
      num_additional_needed = length(newly_added_levels)
      if (num_additional_needed > brewer.pal.info[pdm_palette_name_for_fallback, "maxcolors"]) {
          additional_colors_for_new_levels <- colorRampPalette(brewer.pal(brewer.pal.info[pdm_palette_name_for_fallback, "maxcolors"], pdm_palette_name_for_fallback))(num_additional_needed)
      } else {
          available_additional_colors <- brewer.pal(max(3, num_additional_needed), pdm_palette_name_for_fallback)
          additional_colors_for_new_levels <- available_additional_colors[1:num_additional_needed]
      }
      names(additional_colors_for_new_levels) <- newly_added_levels
  }
  FINAL_PDM_COLOR_PALETTE <- c(base_palette, additional_colors_for_new_levels)
  FINAL_PDM_COLOR_PALETTE <- FINAL_PDM_COLOR_PALETTE[PDM_SELECTED_LEVELS_FINAL]
}

Pdem_levels_actually_in_data_ordered <- PDM_SELECTED_LEVELS_FINAL[PDM_SELECTED_LEVELS_FINAL %in% actual_pdm_levels_in_data]
if(length(Pdem_levels_actually_in_data_ordered) == 0 && length(actual_pdm_levels_in_data) > 0) { 
    Pdem_levels_actually_in_data_ordered <- actual_pdm_levels_in_data
}
PDM_SELECTED_LEVELS_FINAL <- Pdem_levels_actually_in_data_ordered
FINAL_PDM_COLOR_PALETTE <- FINAL_PDM_COLOR_PALETTE[names(FINAL_PDM_COLOR_PALETTE) %in% PDM_SELECTED_LEVELS_FINAL]
# --- End of PDM level/palette logic ---

clustered_data$PDM_Selected <- factor(clustered_data$PDM_Selected, levels = PDM_SELECTED_LEVELS_FINAL)
cat("'PDM_Selected' and 'Project_Success' columns processed and factored.\n")


# 3. Generate Pie Charts -----------------------------------------------------
cat("\n--- Generating PDM Selected pie charts by Success Level for each cluster ---\n")
successful_plots_list <- list()
unsuccessful_plots_list <- list()

# Function to generate a single pie chart
make_pdm_pie_chart <- function(data_subset, chart_title_suffix, cluster_num_text) {
  if (nrow(data_subset) == 0) {
    cat("    No data for", chart_title_suffix, "in", cluster_num_text, ". Skipping pie chart.\n")
    # Create an empty plot with a message
    empty_plot <- ggplot() + annotate("text", x=0.5, y=0.5, label=paste("No data for", chart_title_suffix, "\nin", cluster_num_text)) + theme_void()
    return(empty_plot)
  }

  pdm_summary <- data_subset %>%
    group_by(PDM_Selected) %>%
    summarise(count = n(), .groups = 'drop') %>%
    tidyr::complete(PDM_Selected = PDM_SELECTED_LEVELS_FINAL, fill = list(count = 0)) %>%
    mutate(
      total_group_count = sum(count),
      percentage = ifelse(total_group_count == 0, 0, (count / total_group_count) * 100),
      PDM_Selected = factor(PDM_Selected, levels = PDM_SELECTED_LEVELS_FINAL),
      label = ifelse(count > 0, paste0(count, " (", sprintf("%.1f%%", percentage), ")"), "")
    ) %>%
    arrange(PDM_Selected)

  pie_chart <- ggplot(pdm_summary, aes(x = "", y = percentage, fill = PDM_Selected)) +
    geom_bar(stat = "identity", width = 0.9, color = "white") +
    coord_polar("y", start = 0) +
    geom_text(data = subset(pdm_summary, count > 0),
              aes(label = label), position = position_stack(vjust = 0.5),
              color = "black", size = 3.5, fontface = "bold") +
    scale_fill_manual(values = FINAL_PDM_COLOR_PALETTE, name = "PDM Selected (by Innovativeness)",
                     breaks = PDM_SELECTED_LEVELS_FINAL, labels = PDM_SELECTED_LEVELS_FINAL, drop = FALSE) +
    labs(title = paste0("PDM Selected - ", cluster_num_text, "\n(", chart_title_suffix, ")"),
         subtitle = paste0("Total Projects in Group: n=", sum(pdm_summary$count)), x = NULL, y = NULL) +
    theme_void() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 14, face = "bold", margin = margin(b = 5), lineheight=1),
      plot.subtitle = element_text(hjust = 0.5, size = 10, margin = margin(b = 10)),
      legend.title = element_text(size = 10, face = "bold"), legend.text = element_text(size = 8),
      legend.position = "right", legend.key.size = unit(0.6, "cm"),
      legend.margin = margin(l = 0, r = 5, t = 0, b = 0), legend.spacing.y = unit(0.2, "cm"),
      plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm")
    )
  return(pie_chart)
}

for (cluster_num in 1:K_VALUE_FIXED) {
  cluster_num_text <- paste("Cluster", cluster_num)
  cat("  Processing", cluster_num_text, "...\n")
  
  cluster_data_subset <- clustered_data %>% filter(Cluster == cluster_num)

  # Successful Projects
  successful_subset <- cluster_data_subset %>% filter(Project_Success %in% SUCCESSFUL_PROJECT_LEVELS)
  plot_successful <- make_pdm_pie_chart(successful_subset, "Successful Projects", cluster_num_text)
  successful_plots_list[[paste0("cluster_", cluster_num)]] <- plot_successful
  ggsave(file.path(FIG_DIR_101, paste0("pdm_successful_cluster_", cluster_num, ".pdf")), plot_successful, width = 8, height = 6)
  cat(paste("    Saved pie chart for Successful Projects in", cluster_num_text, "\n"))

  # Unsuccessful Projects
  unsuccessful_subset <- cluster_data_subset %>% filter(Project_Success %in% UNSUCCESSFUL_PROJECT_LEVELS)
  plot_unsuccessful <- make_pdm_pie_chart(unsuccessful_subset, "Unsuccessful Projects", cluster_num_text)
  unsuccessful_plots_list[[paste0("cluster_", cluster_num)]] <- plot_unsuccessful
  ggsave(file.path(FIG_DIR_101, paste0("pdm_unsuccessful_cluster_", cluster_num, ".pdf")), plot_unsuccessful, width = 8, height = 6)
  cat(paste("    Saved pie chart for Unsuccessful Projects in", cluster_num_text, "\n"))
}

# 4. Save Combined Plots -----------------------------------------------------
cat("\n--- Saving combined PDM Selected plots by success level ---\n")

num_cols_grid <- if (K_VALUE_FIXED == 4) 2 else 3
num_rows_grid <- ceiling(K_VALUE_FIXED / num_cols_grid)
pdf_width <- if (K_VALUE_FIXED == 4) num_cols_grid * 7 else num_cols_grid * 6
pdf_height <- if (K_VALUE_FIXED == 4) num_rows_grid * 6 else num_rows_grid * 5

# Combined plot for Successful projects
if (length(successful_plots_list) > 0) {
  combined_filename_succ <- file.path(FIG_DIR_101, paste0("all_clusters_pdm_successful_k", K_VALUE_FIXED, ".pdf"))
  pdf(combined_filename_succ, width = pdf_width, height = pdf_height)
  grid.arrange(grobs = successful_plots_list, ncol = num_cols_grid, nrow = num_rows_grid,
               top = textGrob(paste0("PDM Selected for Successful Projects by Cluster (k=", K_VALUE_FIXED, ")"), 
                              gp = gpar(fontsize = 16, fontface = "bold")))
  dev.off()
  cat("Combined PDM Selected plot for Successful projects saved to:", combined_filename_succ, "\n")
} else {
  cat("No plots for Successful projects were generated to save.\n")
}

# Combined plot for Unsuccessful projects
if (length(unsuccessful_plots_list) > 0) {
  combined_filename_unsucc <- file.path(FIG_DIR_101, paste0("all_clusters_pdm_unsuccessful_k", K_VALUE_FIXED, ".pdf"))
  pdf(combined_filename_unsucc, width = pdf_width, height = pdf_height)
  grid.arrange(grobs = unsuccessful_plots_list, ncol = num_cols_grid, nrow = num_rows_grid,
               top = textGrob(paste0("PDM Selected for Unsuccessful Projects by Cluster (k=", K_VALUE_FIXED, ")"), 
                              gp = gpar(fontsize = 16, fontface = "bold")))
  dev.off()
  cat("Combined PDM Selected plot for Unsuccessful projects saved to:", combined_filename_unsucc, "\n")
} else {
  cat("No plots for Unsuccessful projects were generated to save.\n")
}

cat("\n============== SCRIPT R/101 (PDM by Success Level Pie Charts k4) FINISHED ==============\n") 