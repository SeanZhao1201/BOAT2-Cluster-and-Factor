# R/085_silhouette_analysis_post_1_removal.R
# This script computes average Silhouette scores for k=2..10 using Gower distance
# on the same post-1-removal dataset used by 080/090, then plots the result
# to validate the choice of k=4.

cat("============== SCRIPT R/085 (Silhouette Analysis Post 1 Removal) STARTING ==============\n")

# 1. Load Setup and Data -----------------------------------------------------
if (file.exists("R/000_setup.R")) {
  source("R/000_setup.R")
  cat("R/000_setup.R sourced successfully.\n")
} else {
  suppressPackageStartupMessages({
    library(dplyr)
    library(ggplot2)
    library(readr)
    library(clustMixType)
  })
}

library(cluster) # for daisy() and silhouette()

# Create subdirectories
fig_dir <- "results/figures/085_silhouette_post_1_removal"
tbl_dir <- "results/tables/085_silhouette_post_1_removal"
for (dir_p in c(fig_dir, tbl_dir)) {
  if (!dir.exists(dir_p)) {
    dir.create(dir_p, recursive = TRUE)
    cat(paste("Created directory:", dir_p, "\n"))
  }
}

# Load the dataset
data_file <- "data/BOAT2_Data_Success.csv"
if (!file.exists(data_file)) stop(paste("Error: Dataset not found at", data_file))
data_original <- read_csv(data_file, show_col_types = FALSE)
cat("Loaded dataset with", nrow(data_original), "rows and", ncol(data_original), "columns.\n")

# Remove X/X1 columns if present
if ("X" %in% colnames(data_original)) data_original <- data_original %>% select(-X)
if ("X1" %in% colnames(data_original)) data_original <- data_original %>% select(-X1)

# 2. Remove Outlier -----------------------------------------------------------
outlier_employee_val <- 3700
data_cleaned <- data_original %>% filter(ORG_Employees != outlier_employee_val)
cat("After removing outlier (ORG_Employees ==", outlier_employee_val, "):", nrow(data_cleaned), "rows remain.\n")

# 3. Prepare Variables --------------------------------------------------------
numerical_vars <- c("ORG_Employees", "ORG_Locations", "ORG_Departments", "ORG_Layers")
exclude_vars <- c("Owner_Type", "PDM_Selected", "PDM_Experience_DBB", "PDM_Experience_DB",
                   "PDM_Experience_PDB", "PDM_Experience_CMAR", "PDM_Experience_IPD")
exclude_vars <- exclude_vars[exclude_vars %in% colnames(data_cleaned)]

all_clustering_vars <- setdiff(colnames(data_cleaned), exclude_vars)
categorical_vars <- setdiff(all_clustering_vars, numerical_vars)
numerical_vars <- numerical_vars[numerical_vars %in% colnames(data_cleaned)]
categorical_vars <- categorical_vars[categorical_vars %in% colnames(data_cleaned)]

cat("Numerical variables:", paste(numerical_vars, collapse = ", "), "\n")
cat("Categorical variables (", length(categorical_vars), "):", paste(categorical_vars, collapse = ", "), "\n")

# Prepare kproto input
vars_for_input <- c(numerical_vars, categorical_vars)
kproto_input <- data_cleaned %>% select(all_of(vars_for_input))

# Convert categorical to factors (same logic as 080/090)
if ("Project_Success" %in% colnames(kproto_input)) {
  ps_levels <- c("very unsuccessful", "more unsuccessful", "moderately successful",
                 "more successful", "very successful")
  kproto_input$Project_Success <- factor(kproto_input$Project_Success,
                                         levels = ps_levels, ordered = TRUE)
}

other_cat <- setdiff(categorical_vars, "Project_Success")
if (length(other_cat) > 0) {
  kproto_input <- kproto_input %>%
    mutate(across(all_of(other_cat), function(col) {
      if (is.numeric(col) && !all(is.na(col))) {
        max_val <- max(col, na.rm = TRUE)
        return(ordered(round(col), levels = 1:max(5, ceiling(max_val))))
      } else {
        return(factor(col))
      }
    }))
}

cat("Input data prepared:", nrow(kproto_input), "rows x", ncol(kproto_input), "columns.\n")

# 4. Compute Gower Distance Matrix -------------------------------------------
cat("\nComputing Gower distance matrix...\n")
gower_dist <- daisy(kproto_input, metric = "gower")
cat("Gower distance matrix computed.\n")

# 5. Compute Silhouette Scores for k=2..10 -----------------------------------
max_k <- min(10, floor(nrow(kproto_input) / 2) - 1)
if (max_k < 2) max_k <- 2
k_values <- 2:max_k
sil_seed <- 12345

sil_df <- data.frame(
  k = k_values,
  avg_silhouette = numeric(length(k_values))
)

cat("\nComputing silhouette scores for k =", min(k_values), "to", max(k_values), "...\n")
for (i in seq_along(k_values)) {
  k_current <- k_values[i]
  cat("  k =", k_current, "... ")

  set.seed(sil_seed)
  kproto_model <- clustMixType::kproto(
    kproto_input,
    k = k_current,
    verbose = FALSE,
    nstart = 10
  )

  sil_result <- silhouette(kproto_model$cluster, gower_dist)
  avg_sil <- mean(sil_result[, "sil_width"])
  sil_df$avg_silhouette[i] <- avg_sil
  cat("avg silhouette =", sprintf("%.4f", avg_sil), "\n")
}

# Save results
write.csv(sil_df, file.path(tbl_dir, "silhouette_scores_post_1_removed.csv"), row.names = FALSE)
cat("\nSilhouette scores saved.\n")

# 6. Find optimal k ----------------------------------------------------------
optimal_k_sil <- sil_df$k[which.max(sil_df$avg_silhouette)]
cat("Optimal k by silhouette:", optimal_k_sil,
    "(avg sil =", sprintf("%.4f", max(sil_df$avg_silhouette)), ")\n")

# 7. Plot Silhouette Scores --------------------------------------------------
cat("\nGenerating silhouette plot...\n")

sil_plot <- ggplot(sil_df, aes(x = k, y = avg_silhouette)) +
  geom_line(color = "#0072B2", linewidth = 1) +
  geom_point(size = 3, color = "#D55E00") +
  geom_text(aes(label = sprintf("%.3f", avg_silhouette)),
            vjust = -1.2, size = 3.5, color = "black") +
  # Highlight k=4
  {if (4 %in% sil_df$k) {
    k4_data <- sil_df[sil_df$k == 4, ]
    geom_point(data = k4_data, aes(x = k, y = avg_silhouette),
               size = 5, color = "#E41A1C", shape = 21, fill = "#E41A1C", stroke = 2)
  }} +
  labs(
    x = "Number of Clusters (k)",
    y = "Average Silhouette Width"
  ) +
  scale_x_continuous(breaks = sil_df$k) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.major = element_line(colour = "grey90"),
    panel.grid.minor = element_blank(),
    axis.title = element_text(face = "bold")
  )

# Save PDF
plot_pdf <- file.path(fig_dir, "silhouette_plot_post_1_removed.pdf")
ggsave(plot_pdf, plot = sil_plot, width = 4.25, height = 2.975, dpi = 300)
cat("Silhouette plot (PDF) saved to:", plot_pdf, "\n")

# Save PNG
plot_png <- file.path(fig_dir, "silhouette_plot_post_1_removed.png")
ggsave(plot_png, plot = sil_plot, width = 4.25, height = 2.975, dpi = 300)
cat("Silhouette plot (PNG) saved to:", plot_png, "\n")

cat("\n============== SCRIPT R/085 (Silhouette Analysis Post 1 Removal) FINISHED ==============\n")
