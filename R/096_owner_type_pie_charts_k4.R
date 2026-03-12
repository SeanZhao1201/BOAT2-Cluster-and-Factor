# R/096_owner_type_pie_charts_k4.R
# This script visualizes the distribution of Owner_Type (Public vs Private)
# for each of the k=4 clusters using pie charts.
# Data is loaded from the output of script 090.

# 0. Load Setup and Data -----------------------------------------------------
cat("============== SCRIPT R/096 (Owner Type Pie Charts k4) STARTING ==============\n")

if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
if (!requireNamespace("readr", quietly = TRUE)) install.packages("readr")
if (!requireNamespace("gridExtra", quietly = TRUE)) install.packages("gridExtra")
if (!requireNamespace("grid", quietly = TRUE)) install.packages("grid")
if (!requireNamespace("tidyr", quietly = TRUE)) install.packages("tidyr")

library(dplyr)
library(ggplot2)
library(readr)
library(gridExtra)
library(grid)
library(tidyr)

if (file.exists("R/000_setup.R")) {
  source("R/000_setup.R")
  cat("R/000_setup.R sourced successfully.\n")
} else {
  cat("Warning: R/000_setup.R not found. Essential packages loaded directly.\n")
}

# 1. Configuration ----------------------------------------------------------
K_VALUE_FIXED <- 4
INPUT_TABLES_DIR <- paste0("results/tables/090_kprototype_post_1_removal_k", K_VALUE_FIXED)
CLUSTER_DATA_FILENAME <- paste0("kproto_clusters_k", K_VALUE_FIXED, "_post_1_removed.csv")
CLUSTER_DATA_FILEPATH <- file.path(INPUT_TABLES_DIR, CLUSTER_DATA_FILENAME)

FIG_DIR <- paste0("results/figures/096_owner_type_pie_charts_k", K_VALUE_FIXED)
if (!dir.exists(FIG_DIR)) {
  dir.create(FIG_DIR, recursive = TRUE)
  cat(paste("Created directory:", FIG_DIR, "\n"))
}

# Owner Type levels and color palette
OWNER_TYPE_LEVELS <- c("Public/Government Sector/Quasi-public",
                       "Private Sector (for profit and nonprofit)")
OWNER_TYPE_LABELS <- c("Public/Government", "Private Sector")
OWNER_COLOR_PALETTE <- c("Public/Government Sector/Quasi-public" = "#4E79A7",
                         "Private Sector (for profit and nonprofit)" = "#E15759")

cat("Configuration complete. Output will be saved to:", FIG_DIR, "\n")

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
} else if (!"Cluster" %in% colnames(clustered_data)) {
  stop("Cluster assignment column not found in the loaded data.")
}

# Verify Owner_Type column exists
if (!"Owner_Type" %in% colnames(clustered_data)) {
  stop("Error: 'Owner_Type' column not found in the data. Please re-run scripts 010 and 090 first.")
}
clustered_data$Owner_Type <- factor(clustered_data$Owner_Type, levels = OWNER_TYPE_LEVELS)
cat("'Owner_Type' column converted to factor.\n")
cat("Overall Owner_Type distribution:\n")
print(table(clustered_data$Owner_Type, useNA = "ifany"))

# 3. Generate Pie Charts for each Cluster ------------------------------------
cat("\n--- Generating Owner Type pie charts for each cluster ---\n")
plots_list <- list()

for (cluster_num in 1:K_VALUE_FIXED) {
  cat("  Processing Cluster", cluster_num, "...\n")

  cluster_subset <- clustered_data %>%
    filter(Cluster == cluster_num)

  if (nrow(cluster_subset) == 0) {
    cat("    Warning: No data found for Cluster", cluster_num, ". Skipping.\n")
    next
  }

  owner_summary <- cluster_subset %>%
    group_by(Owner_Type) %>%
    summarise(count = n(), .groups = 'drop') %>%
    tidyr::complete(Owner_Type = OWNER_TYPE_LEVELS, fill = list(count = 0)) %>%
    mutate(
      percentage = count / sum(count) * 100,
      Owner_Type = factor(Owner_Type, levels = OWNER_TYPE_LEVELS),
      label = ifelse(count > 0, paste0(count, " (", sprintf("%.1f%%", percentage), ")"), "")
    ) %>%
    arrange(Owner_Type)

  pie_chart <- ggplot(owner_summary, aes(x = "", y = percentage, fill = Owner_Type)) +
    geom_bar(stat = "identity", width = 0.9, color = "white") +
    coord_polar("y", start = 0) +
    geom_text(data = subset(owner_summary, count > 0),
              aes(label = label),
              position = position_stack(vjust = 0.5),
              color = "black",
              size = 4,
              fontface = "bold") +
    scale_fill_manual(values = OWNER_COLOR_PALETTE,
                     name = "Owner Type",
                     breaks = OWNER_TYPE_LEVELS,
                     labels = OWNER_TYPE_LABELS,
                     drop = FALSE) +
    labs(title = paste0("Owner Type Distribution\nCluster ", cluster_num),
         subtitle = paste0("Total Projects: n=", sum(owner_summary$count)),
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

  individual_plot_filename <- file.path(FIG_DIR,
                                        paste0("cluster_", cluster_num, "_owner_type_pie.pdf"))
  ggsave(individual_plot_filename,
         plot = pie_chart,
         width = 10,
         height = 8,
         units = "in",
         device = "pdf")

  cat(paste("  Saved pie chart for Cluster", cluster_num, "to:", individual_plot_filename, "\n"))
  plots_list[[paste0("cluster_", cluster_num)]] <- pie_chart
}

# 4. Save Combined Plot -----------------------------------------------------
cat("\n--- Saving combined plot ---\n")

if (length(plots_list) > 0) {
  num_cols_grid <- 2
  num_rows_grid <- ceiling(length(plots_list) / num_cols_grid)

  combined_plot_filename <- file.path(FIG_DIR,
                                      paste0("all_clusters_owner_type_k", K_VALUE_FIXED, ".pdf"))

  pdf_width <- num_cols_grid * 8
  pdf_height <- num_rows_grid * 7

  pdf(combined_plot_filename, width = pdf_width, height = pdf_height)

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
    nrow = num_rows_grid,
    top = textGrob(paste0("Owner Type Distribution by Cluster (k=", K_VALUE_FIXED, ")"),
                   gp = gpar(fontsize = 20, fontface = "bold"))
  )
  dev.off()

  cat("Combined pie chart plot saved to:", combined_plot_filename, "\n")
} else {
  cat("No plots were generated to save.\n")
}

cat("\n============== SCRIPT R/096 (Owner Type Pie Charts k4) FINISHED ==============\n")
