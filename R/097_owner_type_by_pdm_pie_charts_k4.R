# R/097_owner_type_by_pdm_pie_charts_k4.R
# This script visualizes the cross-tabulation of Owner_Type (Public/Private) and
# PDM_Selected for each k=4 cluster using sunburst-style nested pie charts.
# Inner ring: Owner_Type, Outer ring: PDM_Selected

cat("============== SCRIPT R/097 (Owner Type × PDM Sunburst k4) STARTING ==============\n")

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
  cat("Warning: R/000_setup.R not found.\n")
}

# 1. Configuration ----------------------------------------------------------
K_VALUE_FIXED <- 4
INPUT_TABLES_DIR <- paste0("results/tables/090_kprototype_post_1_removal_k", K_VALUE_FIXED)
CLUSTER_DATA_FILENAME <- paste0("kproto_clusters_k", K_VALUE_FIXED, "_post_1_removed.csv")
CLUSTER_DATA_FILEPATH <- file.path(INPUT_TABLES_DIR, CLUSTER_DATA_FILENAME)

FIG_DIR <- "results/figures/097_owner_type_by_pdm_k4"
if (!dir.exists(FIG_DIR)) {
  dir.create(FIG_DIR, recursive = TRUE)
  cat(paste("Created directory:", FIG_DIR, "\n"))
}

# Owner Type settings
OWNER_TYPE_LEVELS <- c("Public/Government Sector/Quasi-public",
                       "Private Sector (for profit and nonprofit)")
OWNER_TYPE_SHORT <- c("Public/Government Sector/Quasi-public" = "Public",
                      "Private Sector (for profit and nonprofit)" = "Private")
OWNER_COLORS <- c("Public/Government Sector/Quasi-public" = "#4E79A7",
                   "Private Sector (for profit and nonprofit)" = "#E15759")

# PDM settings (same as 094/095)
PDM_SELECTED_LEVELS <- c(
  "Design-Bid-Build",
  "Construction Manager @ Risk",
  "Design-Build",
  "Progressive Design-Build",
  "Integrated Project Delivery (IPD)"
)
PDM_COLORS <- c(
  "Design-Bid-Build" = "#D73027",
  "Construction Manager @ Risk" = "#FC8D59",
  "Design-Build" = "#FEE08B",
  "Progressive Design-Build" = "#91BFDB",
  "Integrated Project Delivery (IPD)" = "#4575B4"
)

cat("Configuration complete.\n")

# 2. Load Data --------------------------------------------------------------
cat("\n--- Loading cluster data ---\n")
if (!file.exists(CLUSTER_DATA_FILEPATH)) stop(paste("File not found:", CLUSTER_DATA_FILEPATH))
clustered_data <- read_csv(CLUSTER_DATA_FILEPATH, show_col_types = FALSE)
cat("Loaded", nrow(clustered_data), "rows.\n")

cluster_col <- paste0("Cluster_k", K_VALUE_FIXED)
if (cluster_col %in% colnames(clustered_data)) {
  clustered_data <- clustered_data %>% rename(Cluster = !!sym(cluster_col))
}

if (!"Owner_Type" %in% colnames(clustered_data)) stop("'Owner_Type' column not found.")
if (!"PDM_Selected" %in% colnames(clustered_data)) stop("'PDM_Selected' column not found.")

clustered_data$Owner_Type <- factor(clustered_data$Owner_Type, levels = OWNER_TYPE_LEVELS)
clustered_data$PDM_Selected <- factor(clustered_data$PDM_Selected, levels = PDM_SELECTED_LEVELS)

# 3. Sunburst Chart Function ------------------------------------------------
create_sunburst <- function(data_subset, cluster_num) {
  if (nrow(data_subset) == 0) return(NULL)

  hierarchical <- data_subset %>%
    group_by(Owner_Type, PDM_Selected) %>%
    summarise(count = n(), .groups = 'drop') %>%
    filter(!is.na(Owner_Type), !is.na(PDM_Selected))

  if (nrow(hierarchical) == 0) return(NULL)

  # Inner ring: Owner_Type
  inner <- hierarchical %>%
    group_by(Owner_Type) %>%
    summarise(total = sum(count), .groups = 'drop') %>%
    mutate(
      fraction = total / sum(total),
      ymax = cumsum(fraction),
      ymin = lag(ymax, default = 0),
      label_y = (ymin + ymax) / 2,
      label_text = paste0(OWNER_TYPE_SHORT[as.character(Owner_Type)], "\nn=", total),
      label_x = 1.2
    )

  # Outer ring: PDM_Selected within each Owner_Type
  outer <- hierarchical %>%
    left_join(inner %>% select(Owner_Type, ymin_cat = ymin, ymax_cat = ymax),
              by = "Owner_Type") %>%
    group_by(Owner_Type) %>%
    mutate(
      fraction_within = count / sum(count),
      category_span = ymax_cat - ymin_cat,
      ymax = ymin_cat + cumsum(fraction_within) * category_span,
      ymin = ymin_cat + lag(cumsum(fraction_within), default = 0) * category_span,
      label_y = (ymin + ymax) / 2,
      PDM_abbrev = case_when(
        PDM_Selected == "Design-Bid-Build" ~ "DBB",
        PDM_Selected == "Construction Manager @ Risk" ~ "CM@R",
        PDM_Selected == "Design-Build" ~ "DB",
        PDM_Selected == "Progressive Design-Build" ~ "PDB",
        PDM_Selected == "Integrated Project Delivery (IPD)" ~ "IPD",
        TRUE ~ as.character(PDM_Selected)
      ),
      label_text = ifelse(count > 0, paste0(PDM_abbrev, " (n=", count, ")"), "")
    ) %>%
    ungroup()

  p <- ggplot() +
    # Inner ring
    geom_rect(data = inner,
              aes(xmin = 2, xmax = 3, ymin = ymin, ymax = ymax, fill = Owner_Type),
              color = "white", linewidth = 1) +
    # Outer ring
    geom_rect(data = outer,
              aes(xmin = 3, xmax = 4, ymin = ymin, ymax = ymax),
              fill = PDM_COLORS[as.character(outer$PDM_Selected)],
              color = "white", linewidth = 0.5) +
    # Inner labels
    geom_segment(data = inner,
                 aes(x = 2, xend = 1.3, y = label_y, yend = label_y),
                 color = "gray50", linewidth = 0.3) +
    geom_text(data = inner,
              aes(x = label_x, y = label_y, label = label_text),
              size = 3, fontface = "bold", color = "black", hjust = 1) +
    # Outer labels
    geom_segment(data = filter(outer, count > 0),
                 aes(x = 4, xend = 4.8, y = label_y, yend = label_y),
                 color = "gray50", linewidth = 0.3) +
    geom_text(data = filter(outer, count > 0),
              aes(x = 4.9, y = label_y, label = label_text),
              size = 2.8, color = "black", hjust = 0) +
    coord_polar(theta = "y", clip = "off") +
    xlim(c(0, 5.5)) +
    scale_fill_manual(values = OWNER_COLORS) +
    labs(title = paste0("Cluster ", cluster_num),
         subtitle = paste0("n=", nrow(data_subset))) +
    theme_void(base_size = 10) +
    theme(
      plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5, size = 11),
      legend.position = "none",
      plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")
    )

  return(p)
}

# 4. Generate Charts --------------------------------------------------------
cat("\n--- Generating sunburst charts ---\n")
plots_list <- list()

for (cluster_num in 1:K_VALUE_FIXED) {
  cat("  Cluster", cluster_num, "...\n")
  subset_data <- clustered_data %>% filter(Cluster == cluster_num)
  p <- create_sunburst(subset_data, cluster_num)

  if (!is.null(p)) {
    plots_list[[paste0("cluster_", cluster_num)]] <- p

    ggsave(file.path(FIG_DIR, paste0("owner_pdm_sunburst_cluster_", cluster_num, ".pdf")),
           p, width = 4.5, height = 4, device = "pdf")
    ggsave(file.path(FIG_DIR, paste0("owner_pdm_sunburst_cluster_", cluster_num, ".png")),
           p, width = 4.5, height = 4, dpi = 300, device = "png")
    cat("    Saved.\n")
  }
}

# 5. Combined Plot -----------------------------------------------------------
if (length(plots_list) > 0) {
  combined_file <- file.path(FIG_DIR, paste0("all_clusters_owner_pdm_sunburst_k", K_VALUE_FIXED, ".pdf"))
  pdf(combined_file, width = 16, height = 14)
  grid.arrange(
    grobs = plots_list, ncol = 2, nrow = 2,
    top = textGrob(
      paste0("Owner Type \u00d7 PDM Selected by Cluster (k=", K_VALUE_FIXED, ")\nInner: Public/Private | Outer: PDM Type"),
      gp = gpar(fontsize = 16, fontface = "bold"))
  )
  dev.off()
  cat("Combined plot saved to:", combined_file, "\n")
}

# 6. Summary Table -----------------------------------------------------------
summary_tbl <- clustered_data %>%
  group_by(Cluster, Owner_Type, PDM_Selected) %>%
  summarise(count = n(), .groups = 'drop') %>%
  group_by(Cluster) %>%
  mutate(cluster_total = sum(count),
         pct = round(count / cluster_total * 100, 1)) %>%
  ungroup()

write.csv(summary_tbl, file.path(FIG_DIR, "owner_pdm_summary_table.csv"), row.names = FALSE)
cat("Summary table saved.\n")

cat("\n============== SCRIPT R/097 FINISHED ==============\n")
