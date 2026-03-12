# R/094_pdm_selected_pie_charts_k4.R
# This script visualizes the distribution of PDM_Selected for each of the k=4 clusters
# using pie charts. Data is loaded from the output of script 090.

# 0. Load Setup and Data -----------------------------------------------------
cat("============== SCRIPT R/094 (PDM Selected Pie Charts k4) STARTING ==============\n")

# Ensure essential packages are loaded
if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
if (!requireNamespace("readr", quietly = TRUE)) install.packages("readr")
if (!requireNamespace("RColorBrewer", quietly = TRUE)) install.packages("RColorBrewer")
if (!requireNamespace("gridExtra", quietly = TRUE)) install.packages("gridExtra") # For grid.arrange
if (!requireNamespace("grid", quietly = TRUE)) install.packages("grid") # For textGrob
if (!requireNamespace("tidyr", quietly = TRUE)) install.packages("tidyr")

library(dplyr)
library(ggplot2)
library(readr)
library(RColorBrewer)
library(gridExtra) # For grid.arrange
library(grid)      # For textGrob
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

# Create subdirectory for results from this script (094)
FIG_DIR_094 <- paste0("results/figures/094_pdm_selected_pie_charts_k", K_VALUE_FIXED)
if (!dir.exists(FIG_DIR_094)) {
  dir.create(FIG_DIR_094, recursive = TRUE)
  cat(paste("Created directory:", FIG_DIR_094, "\n"))
}

# PDM_Selected levels in desired order of innovativeness
PDM_SELECTED_LEVELS <- c(
  "Design-Bid-Build", 
  "Construction Manager @ Risk",
  "Design-Build", 
  "Progressive Design-Build", 
  "Integrated Project Delivery (IPD)" 
)

# Custom progressive palette: Red -> Orange -> Yellow -> Light Blue -> Darker Blue
custom_pdm_colors <- c(
  "#D73027", # Red for Design-Bid-Build
  "#FC8D59", # Orange for Construction Manager @ Risk
  "#FEE08B", # Yellow for Design-Build
  "#91BFDB", # Light Blue for Progressive Design-Build
  "#4575B4"  # Darker Blue for Integrated Project Delivery (IPD)
)

if (length(custom_pdm_colors) == length(PDM_SELECTED_LEVELS)) {
  PDM_SELECTED_COLOR_PALETTE <- custom_pdm_colors
  names(PDM_SELECTED_COLOR_PALETTE) <- PDM_SELECTED_LEVELS
} else {
  # Fallback or error if lengths don't match - this should not be hit with fixed levels
  stop("Mismatch between number of PDM levels and custom colors defined. Please check PDM_SELECTED_LEVELS and custom_pdm_colors.")
}

# Store the original palette name for dynamic additions if needed (though less likely with custom base)
pdm_palette_name <- "Set2" # Still used as a fallback for *additional* unexpected levels

cat("Configuration complete. Output will be saved to:", FIG_DIR_094, "\n")

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

# Ensure PDM_Selected is present and factorize using the defined levels
if (!"PDM_Selected" %in% colnames(clustered_data)) {
  stop("Error: 'PDM_Selected' column not found in the data.")
}

# Final list of PDM levels to use for factors and plotting (initially our ordered list)
PDM_SELECTED_LEVELS_FINAL <- PDM_SELECTED_LEVELS

# Check for levels in data not in our predefined PDM_SELECTED_LEVELS_FINAL
actual_pdm_levels_in_data <- unique(clustered_data$PDM_Selected)
missing_from_final_list <- setdiff(actual_pdm_levels_in_data, PDM_SELECTED_LEVELS_FINAL)

if(length(missing_from_final_list) > 0) {
  cat("Warning: The following PDM_Selected values from data were not in the predefined ordered list and will be appended:", 
      paste(missing_from_final_list, collapse=", "), "\n")
  PDM_SELECTED_LEVELS_FINAL <- c(PDM_SELECTED_LEVELS_FINAL, missing_from_final_list)
  
  # Re-generate palette if new levels were added
  num_pdm_levels_final <- length(PDM_SELECTED_LEVELS_FINAL)
  
  # Get the initially defined custom colors for the base levels
  base_palette <- PDM_SELECTED_COLOR_PALETTE[names(PDM_SELECTED_COLOR_PALETTE) %in% PDM_SELECTED_LEVELS]
  
  # For new (missing) levels, generate additional distinct colors
  newly_added_levels <- missing_from_final_list # these are levels in data but not in PDM_SELECTED_LEVELS
  additional_colors_for_new_levels <- c()
  if(length(newly_added_levels) > 0) {
      # Try to get distinct colors, avoiding those already used if possible
      # Using a different brewer palette for any unexpected additional levels
      num_additional_needed = length(newly_added_levels)
      if (num_additional_needed > brewer.pal.info["Paired", "maxcolors"]) {
          additional_colors_for_new_levels <- colorRampPalette(brewer.pal(brewer.pal.info["Paired", "maxcolors"], "Paired"))(num_additional_needed)
      } else {
          available_additional_colors <- brewer.pal(max(3, num_additional_needed), "Paired")
          additional_colors_for_new_levels <- available_additional_colors[1:num_additional_needed]
      }
      names(additional_colors_for_new_levels) <- newly_added_levels
  }
  
  # Combine the base custom palette with any new colors for unexpected levels
  PDM_SELECTED_COLOR_PALETTE <- c(base_palette, additional_colors_for_new_levels)
  # Ensure the final palette only contains colors for levels that are actually in PDM_SELECTED_LEVELS_FINAL
  PDM_SELECTED_COLOR_PALETTE <- PDM_SELECTED_COLOR_PALETTE[PDM_SELECTED_LEVELS_FINAL]
}

clustered_data$PDM_Selected <- factor(clustered_data$PDM_Selected, levels = PDM_SELECTED_LEVELS_FINAL)
cat("'PDM_Selected' column converted to factor with levels reflecting desired order and data presence.\n")


# 3. Generate Pie Charts for each Cluster ------------------------------------
cat("\n--- Generating PDM Selected pie charts for each cluster ---\n")
plots_list <- list()

for (cluster_num in 1:K_VALUE_FIXED) {
  cat("  Processing Cluster", cluster_num, "...\n")
  
  cluster_subset <- clustered_data %>%
    filter(Cluster == cluster_num)
  
  if (nrow(cluster_subset) == 0) {
    cat("    Warning: No data found for Cluster", cluster_num, ". Skipping pie chart.\n")
    next
  }
  
  # Calculate summary for the current cluster
  pdm_summary <- cluster_subset %>%
    group_by(PDM_Selected) %>%
    summarise(count = n(), .groups = 'drop') %>%
    tidyr::complete(PDM_Selected = PDM_SELECTED_LEVELS_FINAL, fill = list(count = 0)) %>%
    mutate(
      total_cluster_count = sum(count), 
      percentage = ifelse(total_cluster_count == 0, 0, (count / total_cluster_count) * 100), 
      PDM_Selected = factor(PDM_Selected, levels = PDM_SELECTED_LEVELS_FINAL), 
      label = ifelse(count > 0, paste0(count, " (", sprintf("%.1f%%", percentage), ")"), "")
    ) %>%
    arrange(PDM_Selected) # Explicitly arrange by the factor levels for ggplot order

  # Create pie chart
  pie_chart <- ggplot(pdm_summary, aes(x = "", y = percentage, fill = PDM_Selected)) +
    geom_bar(stat = "identity", width = 0.9, color = "white") +
    coord_polar("y", start = 0) +
    geom_text(data = subset(pdm_summary, count > 0),
              aes(label = label),
              position = position_stack(vjust = 0.5),
              color = "black",
              size = 3.5, 
              fontface = "bold") +
    scale_fill_manual(values = PDM_SELECTED_COLOR_PALETTE, 
                     name = "PDM Selected (by Innovativeness)",
                     breaks = PDM_SELECTED_LEVELS_FINAL,
                     labels = PDM_SELECTED_LEVELS_FINAL,
                     drop = FALSE) +
    labs(title = paste0("PDM Selected Distribution\nCluster ", cluster_num),
         subtitle = paste0("Total Responses: n=", sum(pdm_summary$count)),
         x = NULL, y = NULL) +
    theme_void() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 15, face = "bold", margin = margin(b = 10)),
      plot.subtitle = element_text(hjust = 0.5, size = 11, margin = margin(b = 20)),
      legend.title = element_text(size = 11, face = "bold"),
      legend.text = element_text(size = 9),
      legend.position = "right",
      legend.key.size = unit(0.7, "cm"),
      legend.margin = margin(l = 0, r = 10, t = 0, b = 0),
      legend.spacing.y = unit(0.25, "cm"),
      plot.margin = unit(c(1,1,1,1), "cm")
    )
  
  # Save individual pie chart
  individual_plot_filename <- file.path(FIG_DIR_094,
                                      paste0("pdm_selected_cluster_", cluster_num, "_pie_chart.pdf"))
  
ggsave(individual_plot_filename, 
         plot = pie_chart,
         width = 10, 
         height = 8,
         units = "in",
         device = "pdf")
  
  cat(paste("  Saved pie chart for Cluster", cluster_num, "to:", individual_plot_filename, "\n"))
  
  plots_list[[paste0("cluster_", cluster_num)]] <- pie_chart
}

# 4. Save Combined Plot ------------------------------------------
cat("\n--- Saving combined PDM Selected plot ---\n")

if (length(plots_list) > 0) {
  num_cols_grid <- if (K_VALUE_FIXED == 4) 2 else 3
  num_rows_grid <- ceiling(length(plots_list) / num_cols_grid)
  
  combined_plot_filename <- file.path(FIG_DIR_094, 
                                    paste0("all_clusters_pdm_selected_pie_charts_k", K_VALUE_FIXED, ".pdf"))
  
  pdf_width <- if (K_VALUE_FIXED == 4) num_cols_grid * 8 else num_cols_grid * 7 
  pdf_height <- if (K_VALUE_FIXED == 4) num_rows_grid * 7 else num_rows_grid * 6

  pdf(combined_plot_filename, width = pdf_width, height = pdf_height)
  
  for (i in 1:length(plots_list)) {
    plots_list[[i]] <- plots_list[[i]] + 
      theme(legend.position = "right",
            legend.key.size = unit(0.7, "cm"),
            legend.margin = margin(l = 0, r = 10, t = 0, b = 0),
            legend.spacing.y = unit(0.25, "cm"))
  }
  
grid.arrange(
    grobs = plots_list, 
    ncol = num_cols_grid,
    nrow = num_rows_grid,
    top = textGrob(paste0("PDM Selected Distribution by Cluster (k=", K_VALUE_FIXED, ")"), 
                   gp = gpar(fontsize = 18, fontface = "bold"))
  )
  dev.off()
  
  cat("Combined PDM Selected pie chart plot saved to:", combined_plot_filename, "\n")
} else {
  cat("No PDM Selected plots were generated to save.\n")
}

cat("\n============== SCRIPT R/094 (PDM Selected Pie Charts k4) FINISHED ==============\n") 