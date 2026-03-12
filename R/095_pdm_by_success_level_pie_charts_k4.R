# R/095_pdm_by_success_level_pie_charts_k4.R
# This script visualizes PDM_Selected distribution for k=4 clusters, 
# using sunburst charts to show hierarchical relationship between PDM types and success levels.
# Data is loaded from the output of script 090.

# Set user library path to avoid permission issues
user_lib <- Sys.getenv("R_LIBS_USER")
if (!dir.exists(user_lib)) {
  dir.create(user_lib, recursive = TRUE, showWarnings = FALSE)
}
.libPaths(c(user_lib, .libPaths()))

# 0. Load Setup and Data -----------------------------------------------------
cat("============== SCRIPT R/095 (PDM by Success Level Sunburst Charts k4) STARTING ==============\n")

# Ensure essential packages are loaded
if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
if (!requireNamespace("readr", quietly = TRUE)) install.packages("readr")
if (!requireNamespace("RColorBrewer", quietly = TRUE)) install.packages("RColorBrewer")
if (!requireNamespace("gridExtra", quietly = TRUE)) install.packages("gridExtra")
if (!requireNamespace("grid", quietly = TRUE)) install.packages("grid")
if (!requireNamespace("tidyr", quietly = TRUE)) install.packages("tidyr")

# Try to install and load sunburstR, fallback to nested pie charts if not available
sunburst_available <- FALSE
tryCatch({
  if (!requireNamespace("sunburstR", quietly = TRUE)) install.packages("sunburstR")
  library(sunburstR)
  sunburst_available <- TRUE
  cat("sunburstR package loaded successfully.\n")
}, error = function(e) {
  cat("Warning: sunburstR package not available. Will use nested pie charts instead.\n")
  sunburst_available <- FALSE
})

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

FIG_DIR_095 <- paste0("results/figures/095_pdm_by_success_k4/")
if (!dir.exists(FIG_DIR_095)) {
  dir.create(FIG_DIR_095, recursive = TRUE)
  cat(paste("Created directory:", FIG_DIR_095, "\n"))
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
UNSUCCESSFUL_PROJECT_LEVELS <- c("more unsuccessful", "very unsuccessful")

pdm_palette_name_for_fallback <- "Set2" # For any unexpected PDM levels not in the custom list

cat("Configuration complete. Output will be saved to:", FIG_DIR_095, "\n")

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

# Ensure Project_Success and PDM_Selected are present
if (!"Project_Success" %in% colnames(clustered_data)) stop("Error: 'Project_Success' column not found.")
if (!"PDM_Selected" %in% colnames(clustered_data)) stop("Error: 'PDM_Selected' column not found.")

# Create Success Category (Successful vs Unsuccessful)
clustered_data <- clustered_data %>%
  mutate(Success_Category = case_when(
    Project_Success %in% SUCCESSFUL_PROJECT_LEVELS ~ "Successful",
    Project_Success %in% UNSUCCESSFUL_PROJECT_LEVELS ~ "Unsuccessful",
    TRUE ~ "Other"
  ))

# Factorize columns
clustered_data$Project_Success <- factor(clustered_data$Project_Success, levels = PROJECT_SUCCESS_LEVELS_ALL)
clustered_data$Success_Category <- factor(clustered_data$Success_Category, levels = c("Successful", "Unsuccessful"))

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
cat("'PDM_Selected', 'Project_Success', and 'Success_Category' columns processed and factored.\n")

# 3. Generate Enhanced Nested Pie Charts ------------------------------------
cat("\n--- Generating enhanced nested PDM Selected charts by Success Level for each cluster ---\n")

# Function to create enhanced nested pie chart with better visual separation
make_enhanced_nested_pie_chart <- function(data_subset, cluster_num_text) {
  if (nrow(data_subset) == 0) {
    cat("    No data for", cluster_num_text, ". Skipping chart.\n")
    empty_plot <- ggplot() + 
      annotate("text", x=0.5, y=0.5, label=paste("No data for", cluster_num_text)) + 
      theme_void()
    return(empty_plot)
  }

  # Prepare data for enhanced nested pie chart
  nested_summary <- data_subset %>%
    group_by(Success_Category, PDM_Selected) %>%
    summarise(count = n(), .groups = 'drop') %>%
    # Complete all combinations
    tidyr::complete(Success_Category = c("Successful", "Unsuccessful"), 
                   PDM_Selected = PDM_SELECTED_LEVELS_FINAL, 
                   fill = list(count = 0)) %>%
    filter(!is.na(Success_Category)) %>%
    group_by(Success_Category) %>%
    mutate(
      category_total = sum(count),
      category_percentage = ifelse(sum(count) == 0, 0, (count / sum(count)) * 100)
    ) %>%
    ungroup() %>%
    mutate(
      total_count = sum(count),
      overall_percentage = ifelse(total_count == 0, 0, (count / total_count) * 100),
      # Create enhanced labels with success category
      PDM_Success_Combo = paste(Success_Category, PDM_Selected, sep = "\n"),
      label = ifelse(count > 0, paste0(count, "\n(", sprintf("%.1f%%", overall_percentage), ")"), ""),
      # Create pattern for visual distinction
      pattern = Success_Category
    ) %>%
    arrange(Success_Category, PDM_Selected)

  # Create enhanced color palette with patterns
  enhanced_colors <- c()
  for (pdm in PDM_SELECTED_LEVELS_FINAL) {
    base_color <- FINAL_PDM_COLOR_PALETTE[pdm]
    # Successful: lighter with border, Unsuccessful: darker with different pattern
    successful_color <- adjustcolor(base_color, alpha.f = 0.6)
    unsuccessful_color <- adjustcolor(base_color, alpha.f = 0.9)
    
    enhanced_colors[paste("Successful", pdm, sep = "\n")] <- successful_color
    enhanced_colors[paste("Unsuccessful", pdm, sep = "\n")] <- unsuccessful_color
  }

  # Create the enhanced nested pie chart
  pie_chart <- ggplot(nested_summary, aes(x = "", y = overall_percentage, fill = PDM_Success_Combo)) +
    geom_bar(stat = "identity", width = 1, color = "white", linewidth = 1) +
    coord_polar("y", start = 0) +
    geom_text(data = subset(nested_summary, count > 0),
              aes(label = label), 
              position = position_stack(vjust = 0.5),
              color = "black", size = 2.5, fontface = "bold") +
    scale_fill_manual(values = enhanced_colors, name = "Success Level - PDM Type") +
    labs(title = paste0("PDM Distribution by Success Level\n", cluster_num_text),
         subtitle = paste0("Total: n=", sum(nested_summary$count), 
                          " | Successful: n=", sum(nested_summary$count[nested_summary$Success_Category == "Successful"]),
                          " | Unsuccessful: n=", sum(nested_summary$count[nested_summary$Success_Category == "Unsuccessful"])),
         x = NULL, y = NULL) +
    theme_void() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 14, face = "bold", margin = margin(b = 5)),
      plot.subtitle = element_text(hjust = 0.5, size = 10, margin = margin(b = 10)),
      legend.title = element_text(size = 8, face = "bold"), 
      legend.text = element_text(size = 6),
      legend.position = "right", 
      legend.key.size = unit(0.3, "cm"),
      plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm")
    ) +
    guides(fill = guide_legend(ncol = 1, byrow = TRUE))
  
  return(pie_chart)
}

# Generate enhanced nested pie charts for each cluster
enhanced_plots_list <- list()

for (cluster_num in 1:K_VALUE_FIXED) {
  cluster_num_text <- paste("Cluster", cluster_num)
  cat("  Processing", cluster_num_text, "...\n")
  
  cluster_data_subset <- clustered_data %>% filter(Cluster == cluster_num)
  
  # Create enhanced nested pie chart
  plot_enhanced <- make_enhanced_nested_pie_chart(cluster_data_subset, cluster_num_text)
  enhanced_plots_list[[paste0("cluster_", cluster_num)]] <- plot_enhanced
  
  # Save individual chart
  ggsave(file.path(FIG_DIR_095, paste0("pdm_enhanced_nested_cluster_", cluster_num, ".pdf")), 
         plot_enhanced, width = 10, height = 8)
  cat(paste("    Saved enhanced nested chart for", cluster_num_text, "\n"))
}

# 4. Save Combined Enhanced Plots -------------------------------------------
cat("\n--- Saving combined enhanced nested PDM Selected plots ---\n")

num_cols_grid <- if (K_VALUE_FIXED == 4) 2 else 3
num_rows_grid <- ceiling(K_VALUE_FIXED / num_cols_grid)
pdf_width <- if (K_VALUE_FIXED == 4) num_cols_grid * 8 else num_cols_grid * 7
pdf_height <- if (K_VALUE_FIXED == 4) num_rows_grid * 7 else num_rows_grid * 6

# Combined plot for all enhanced charts
if (length(enhanced_plots_list) > 0) {
  combined_filename_enhanced <- file.path(FIG_DIR_095, paste0("all_clusters_pdm_enhanced_nested_k", K_VALUE_FIXED, ".pdf"))
  pdf(combined_filename_enhanced, width = pdf_width, height = pdf_height)
  grid.arrange(grobs = enhanced_plots_list, ncol = num_cols_grid, nrow = num_rows_grid,
               top = textGrob(paste0("PDM Distribution by Success Level - Enhanced Nested Charts (k=", K_VALUE_FIXED, ")"), 
                              gp = gpar(fontsize = 16, fontface = "bold")))
  dev.off()
  cat("Combined enhanced nested PDM charts saved to:", combined_filename_enhanced, "\n")
} else {
  cat("No enhanced plots were generated to save.\n")
}

# 5. Generate Summary Statistics Table ---------------------------------------
cat("\n--- Generating summary statistics ---\n")

summary_stats <- clustered_data %>%
  group_by(Cluster, Success_Category, PDM_Selected) %>%
  summarise(count = n(), .groups = 'drop') %>%
  tidyr::complete(Cluster = 1:K_VALUE_FIXED, 
                 Success_Category = c("Successful", "Unsuccessful"), 
                 PDM_Selected = PDM_SELECTED_LEVELS_FINAL, 
                 fill = list(count = 0)) %>%
  group_by(Cluster, Success_Category) %>%
  mutate(
    category_total = sum(count),
    within_category_percentage = ifelse(category_total == 0, 0, (count / category_total) * 100)
  ) %>%
  group_by(Cluster) %>%
  mutate(
    cluster_total = sum(count),
    within_cluster_percentage = ifelse(cluster_total == 0, 0, (count / cluster_total) * 100)
  ) %>%
  ungroup()

# Save summary table
summary_table_path <- file.path(FIG_DIR_095, "pdm_success_summary_table_enhanced.csv")
write.csv(summary_stats, summary_table_path, row.names = FALSE)
cat("Summary statistics table saved to:", summary_table_path, "\n")

# 6. Generate Sunburst Charts for Individual Clusters ----------------------
cat("\n--- Generating sunburst charts for each cluster ---\n")

# Create sunburst directory
FIG_DIR_SUNBURST <- "results/figures/095_ggplot_sunburst_k4/"
if (!dir.exists(FIG_DIR_SUNBURST)) {
  dir.create(FIG_DIR_SUNBURST, recursive = TRUE)
  cat(paste("Created directory:", FIG_DIR_SUNBURST, "\n"))
}

# Function to create sunburst chart using ggplot2
create_sunburst_chart <- function(data_subset, cluster_num) {
  if (nrow(data_subset) == 0) {
    return(NULL)
  }
  
  # Prepare hierarchical data for sunburst
  hierarchical_data <- data_subset %>%
    group_by(Success_Category, PDM_Selected) %>%
    summarise(count = n(), .groups = 'drop') %>%
    filter(!is.na(Success_Category), !is.na(PDM_Selected))
  
  if (nrow(hierarchical_data) == 0) {
    return(NULL)
  }
  
  # Calculate positions for inner ring (Success Category)
  inner_data <- hierarchical_data %>%
    group_by(Success_Category) %>%
    summarise(total = sum(count), .groups = 'drop') %>%
    mutate(
      fraction = total / sum(total),
      ymax = cumsum(fraction),
      ymin = lag(ymax, default = 0),
      # Calculate label position (middle of segment)
      label_y = (ymin + ymax) / 2,
      # Create label text
      label_text = paste0(Success_Category, "\nn=", total),
      # Position for labels in center area
      label_x = 1.2
    )
  
  # Calculate positions for outer ring (PDM within each Success Category)
  outer_data <- hierarchical_data %>%
    left_join(inner_data %>% select(Success_Category, ymin_cat = ymin, ymax_cat = ymax), 
              by = "Success_Category") %>%
    group_by(Success_Category) %>%
    mutate(
      fraction_within = count / sum(count),
      category_span = ymax_cat - ymin_cat,
      ymax = ymin_cat + cumsum(fraction_within) * category_span,
      ymin = ymin_cat + lag(cumsum(fraction_within), default = 0) * category_span,
      # Calculate label position for outer ring
      label_y = (ymin + ymax) / 2,
      # Create PDM abbreviations
      PDM_abbrev = case_when(
        PDM_Selected == "Design-Bid-Build" ~ "DBB",
        PDM_Selected == "Construction Manager @ Risk" ~ "CM@R",
        PDM_Selected == "Design-Build" ~ "DB",
        PDM_Selected == "Progressive Design-Build" ~ "PDB",
        PDM_Selected == "Integrated Project Delivery (IPD)" ~ "IPD",
        TRUE ~ as.character(PDM_Selected)
      ),
      # Create label text for outer ring (PDM abbrev with count)
      label_text = ifelse(count > 0, paste0(PDM_abbrev, " (n=", count, ")"), ""),
      # Calculate angle for label positioning
      label_angle = (ymin + ymax) / 2 * 180 / pi
    ) %>%
    ungroup()
  
  # Colors - Successful改为绿色
  success_colors <- c("Successful" = "#4DAF4A", "Unsuccessful" = "#D73027")
  
  # Create sunburst plot
  p <- ggplot() +
    # Inner ring: Success Category
    geom_rect(data = inner_data,
              aes(xmin = 2, xmax = 3, ymin = ymin, ymax = ymax, fill = Success_Category),
              color = "white", linewidth = 1) +
    # Outer ring: PDM Selected
    geom_rect(data = outer_data,
              aes(xmin = 3, xmax = 4, ymin = ymin, ymax = ymax),
              fill = FINAL_PDM_COLOR_PALETTE[outer_data$PDM_Selected],
              color = "white", linewidth = 0.5) +
    # Connection lines from inner ring to center labels (绘制在外圈之后)
    geom_segment(data = inner_data,
                 aes(x = 2, xend = 1.3, y = label_y, yend = label_y),
                 color = "gray50", linewidth = 0.3) +
    # Inner ring labels - 显示在中心空白区域 (绘制在外圈之后)
    geom_text(data = inner_data,
              aes(x = label_x, y = label_y, label = label_text),
              size = 3, fontface = "bold", color = "black", hjust = 1) +
    # Connection lines from outer ring to labels
    geom_segment(data = filter(outer_data, count > 0),
                 aes(x = 4, xend = 4.8, y = label_y, yend = label_y),
                 color = "gray50", linewidth = 0.3) +
    # Outer ring labels (positioned outside the sunburst)
    geom_text(data = filter(outer_data, count > 0),
              aes(x = 4.9, y = label_y, label = label_text),
              size = 2.8, color = "black", hjust = 0) +
    coord_polar(theta = "y", clip = "off") +
    xlim(c(0, 5.5)) +
    scale_fill_manual(values = success_colors) +
    theme_void(base_size = 10) +
    theme(
      legend.position = "none",
      plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
      plot.background = element_rect(fill = "transparent", color = NA),
      panel.background = element_rect(fill = "transparent", color = NA)
    )
  
  return(p)
}

# Helper function to save both PDF and PNG
save_plot_both_formats_095 <- function(plot_obj, file_path_without_ext, width, height) {
  if (is.null(plot_obj)) return()
  
  # Save PDF
  pdf_path <- paste0(file_path_without_ext, ".pdf")
  ggsave(pdf_path, plot = plot_obj, width = width, height = height, device = "pdf", bg = "transparent")
  
  # Save PNG with transparent background
  png_path <- paste0(file_path_without_ext, ".png")
  ggsave(png_path, plot = plot_obj, width = width, height = height, dpi = 300, device = "png", bg = "transparent")
  
  cat("    Saved sunburst chart to:", pdf_path, "and", png_path, "\n")
}

# Generate sunburst for each cluster
for (cluster_num in 1:K_VALUE_FIXED) {
  cat("  Generating sunburst for Cluster", cluster_num, "...\n")
  
  cluster_data_subset <- clustered_data %>% filter(Cluster == cluster_num)
  
  sunburst_plot <- create_sunburst_chart(cluster_data_subset, cluster_num)
  
  if (!is.null(sunburst_plot)) {
    save_plot_both_formats_095(
      sunburst_plot,
      file.path(FIG_DIR_SUNBURST, paste0("sunburst_cluster_", cluster_num)),
      4.5, 4
    )
  }
}

# Save summary table for sunburst data
sunburst_summary <- clustered_data %>%
  group_by(Cluster, Success_Category, PDM_Selected) %>%
  summarise(count = n(), .groups = 'drop') %>%
  filter(!is.na(Success_Category), !is.na(PDM_Selected)) %>%
  group_by(Cluster) %>%
  mutate(
    cluster_total = sum(count),
    percentage_within_cluster = (count / cluster_total) * 100
  ) %>%
  ungroup()

write.csv(sunburst_summary, file.path(FIG_DIR_SUNBURST, "sunburst_summary_table.csv"), row.names = FALSE)
cat("Sunburst summary table saved.\n")

cat("\n============== SCRIPT R/095 (PDM by Success Level Enhanced Nested Charts k4) FINISHED ==============\n") 