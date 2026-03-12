# R/080_optimal_k_post_1_removal.R
# This script removes one specific outlier case (ORG_Employees == 3700)
# from BOAT2_Data_Success.csv and then performs an Elbow Plot analysis 
# to suggest an optimal k for K-prototypes. Project_Success is included.

# Set user library path to avoid permission issues
user_lib <- Sys.getenv("R_LIBS_USER")
if (!dir.exists(user_lib)) {
  dir.create(user_lib, recursive = TRUE, showWarnings = FALSE)
}
.libPaths(c(user_lib, .libPaths()))

# 1. Load Setup and Data -----------------------------------------------------
cat("============== SCRIPT R/080 (Optimal K Post 1 Removal) STARTING ==============
")

if (file.exists("R/000_setup.R")) {
  source("R/000_setup.R")
  cat("R/000_setup.R sourced successfully.\n")
} else {
  cat("Warning: R/000_setup.R not found. Loading packages directly.\n")
  suppressPackageStartupMessages({
    library(dplyr)
    library(clustMixType)
    library(ggplot2)
    library(readr)
  })
}

# Create subdirectory for results
plot_dir_080 <- "results/figures/080_optimal_k_post_1_removal"
tables_dir_080 <- "results/tables/080_optimal_k_post_1_removal"

for(dir_p in c(plot_dir_080, tables_dir_080)){
  if (!dir.exists(dir_p)) {
    dir.create(dir_p, recursive = TRUE)
    cat(paste("Created directory:", dir_p, "\n"))
  }
}

# Load the dataset with Project_Success
data_file <- "data/BOAT2_Data_Success.csv"
if (file.exists(data_file)) {
  data_original <- read_csv(data_file, show_col_types = FALSE)
  cat("Loaded dataset", data_file, "with", nrow(data_original), "rows and", ncol(data_original), "columns.\n")
} else {
  stop(paste("Error: Dataset not found at", data_file))
}

# Add an original row ID for tracking
data_original <- data_original %>%
  mutate(Original_Row_ID = 1:n())

# Remove X/X1 columns if they exist
if ("X" %in% colnames(data_original)) data_original <- data_original %>% select(-X)
if ("X1" %in% colnames(data_original)) data_original <- data_original %>% select(-X1)

# 2. Identify and Remove the Single Outlier Case ---------------------------
cat("\n--- Identifying and removing 1 outlier case ---\n")
ids_to_remove <- c()
outlier_employee_val <- 3700

case_outlier <- data_original %>%
  filter(ORG_Employees == outlier_employee_val) %>%
  slice(1) # Take the first if multiple (should be unique by ID later)

if (nrow(case_outlier) > 0) {
  ids_to_remove <- c(ids_to_remove, case_outlier$Original_Row_ID)
  cat("Identified outlier case ID for removal:", case_outlier$Original_Row_ID, "(ORG_Employees:", case_outlier$ORG_Employees, ")\n")
  data_cleaned <- data_original %>% filter(!Original_Row_ID %in% ids_to_remove)
  cat("Removed 1 case. New dataset size:", nrow(data_cleaned), "rows.\n")
} else {
  data_cleaned <- data_original
  cat("Warning: Outlier case (ORG_Employees == ", outlier_employee_val, ") not found. Proceeding with original data.\n")
}

if (nrow(data_cleaned) < 10) { 
  stop("Error: Too few data points remaining after removal for meaningful cluster analysis.")
}

# 3. Prepare data_cleaned for K-prototypes clustering (Optimal K analysis) --
cat("\n--- Preparing data for Optimal K analysis (post 1 removal) ---\n")
numerical_vars <- c("ORG_Employees", "ORG_Locations", "ORG_Departments", "ORG_Layers")
exclude_vars <- c("PDM_Selected", "PDM_Experience_DBB", "PDM_Experience_DB", 
                    "PDM_Experience_PDB", "PDM_Experience_CMAR", "PDM_Experience_IPD", 
                    "Original_Row_ID") # Exclude PDM selectors and Original_Row_ID
exclude_vars <- exclude_vars[exclude_vars %in% colnames(data_cleaned)]

all_clustering_vars <- setdiff(colnames(data_cleaned), exclude_vars)
categorical_vars <- setdiff(all_clustering_vars, numerical_vars) # Project_Success will be here

# Ensure variables exist
numerical_vars <- numerical_vars[numerical_vars %in% colnames(data_cleaned)]
categorical_vars <- categorical_vars[categorical_vars %in% colnames(data_cleaned)]

cat("Numerical variables for clustering:", paste(numerical_vars, collapse=", "), "\n")
cat("Categorical variables for clustering (", length(categorical_vars), ") including Project_Success:", paste(categorical_vars, collapse=", "), "\n")
cat("Is Project_Success in categorical_vars:", ("Project_Success" %in% categorical_vars), "\n")

kproto_input_elbow <- data_cleaned %>%
  select(all_of(c(numerical_vars, categorical_vars)))

if (length(categorical_vars) > 0) {
  # Handle Project_Success specifically
  if ("Project_Success" %in% colnames(kproto_input_elbow)) {
    defined_ps_levels <- c("very unsuccessful", "more unsuccessful", "moderately successful", "more successful", "very successful")
    
    # Store original NA count for Project_Success if column exists in data_cleaned
    original_na_count_ps <- 0
    if ("Project_Success" %in% colnames(data_cleaned)) {
        original_na_count_ps <- sum(is.na(data_cleaned$Project_Success))
    }

    kproto_input_elbow$Project_Success <- factor(
      kproto_input_elbow$Project_Success,
      levels = defined_ps_levels,
      ordered = TRUE
    )
    cat("Project_Success converted to an ordered factor with levels:", paste(defined_ps_levels, collapse=", "), "\n")
    
    new_na_count_ps <- sum(is.na(kproto_input_elbow$Project_Success))
    if (new_na_count_ps > original_na_count_ps) {
       cat("Warning: NAs may have been introduced in Project_Success during factor conversion. Original values might not perfectly match defined levels. Original NAs:", original_na_count_ps, ", New NAs:", new_na_count_ps, "\n")
    }
  }

  # Handle other categorical variables (those not Project_Success)
  other_categorical_vars_to_process <- setdiff(categorical_vars, "Project_Success")
  if (length(other_categorical_vars_to_process) > 0) {
    kproto_input_elbow <- kproto_input_elbow %>%
      mutate(across(all_of(other_categorical_vars_to_process), function(col) {
        # Use cur_column() to get the name of the current column being processed (requires dplyr 1.0.0+)
        # If not available, this part of the warning might not show the column name correctly.
        col_name <- tryCatch(cur_column(), error = function(e) deparse(substitute(col)))

        if (is.numeric(col) && !all(is.na(col))) {
          max_val <- max(col, na.rm = TRUE)
          min_val <- min(col, na.rm = TRUE)
          defined_levels <- 1:max(5, ceiling(max_val))
          if (any(col > max(defined_levels), na.rm = TRUE) || any(col < min(defined_levels), na.rm = TRUE)) {
            cat("Warning: Data for column", col_name, "is numeric but outside assumed 1-N range for ordered factor. Adjusting levels to unique sorted values.\n")
            defined_levels <- sort(unique(round(col[!is.na(col)])))
          }
          return(ordered(round(col), levels = defined_levels))
        } else if (is.character(col) || is.factor(col)) {
          return(factor(col)) # Ensure it's a base factor if not already
        } else {
          cat("Warning: Column", col_name, "is not numeric, character, or factor. Attempting to convert to factor.\n")
          return(factor(col))
        }
      }))
    cat("Other categorical variables processed.\n")
  }
}

# Check for columns that are all NA after transformations
all_na_cols <- sapply(kproto_input_elbow, function(x) all(is.na(x)))
if (any(all_na_cols)) {
    warning_msg <- paste("Warning: The following columns are all NA after transformations and will be removed before kproto:", paste(names(all_na_cols[all_na_cols]), collapse=", "))
    cat(warning_msg, "\n")
    kproto_input_elbow <- kproto_input_elbow[, !all_na_cols, drop = FALSE]
    
    # Update var lists if any were removed (though kproto uses the dataframe directly)
    numerical_vars <- numerical_vars[numerical_vars %in% colnames(kproto_input_elbow)]
    categorical_vars <- categorical_vars[categorical_vars %in% colnames(kproto_input_elbow)]
}

if (ncol(kproto_input_elbow) == 0) {
  stop("Error: No columns selected for k-prototypes input for elbow method.")
}
cat("K-prototypes input for elbow method prepared with", nrow(kproto_input_elbow), "rows and", ncol(kproto_input_elbow), "columns.\n")

# 4. Determine Optimal Number of Clusters (Elbow Method) -------------------
cat("\nDetermining optimal number of clusters (Elbow Method)...
")
max_k_val <- min(10, floor(nrow(kproto_input_elbow) / 2) -1) 
if (max_k_val < 2) max_k_val <- 2 
if (nrow(kproto_input_elbow) < 20) max_k_val <- min(max_k_val, 5) # Adjust for small N

k_values_to_test <- 2:max_k_val
wss_df <- data.frame(
  k = k_values_to_test,
  tot_withinss = numeric(length(k_values_to_test))
)

elbow_method_seed <- 12345 

for (i in 1:length(k_values_to_test)) {
  k_current <- k_values_to_test[i]
  cat("  Processing k =", k_current, "...")
  set.seed(elbow_method_seed)
  
  if (nrow(unique(kproto_input_elbow)) < k_current) {
    cat("Skipping k=", k_current, ", not enough unique rows (", nrow(unique(kproto_input_elbow)), ") for clustering.\n")
    wss_df$tot_withinss[i] <- NA
    next
  }
  
  kproto_model_run <- clustMixType::kproto(
    kproto_input_elbow, 
    k = k_current,
    verbose = FALSE,
    nstart = 10 # Use a reasonable nstart for elbow method
  )
  wss_df$tot_withinss[i] <- kproto_model_run$tot.withinss
  cat(" WSS:", kproto_model_run$tot.withinss, "\n")
}

wss_df <- wss_df[!is.na(wss_df$tot_withinss), ]

if(nrow(wss_df) < 2) {
    stop("Error: Not enough WSS results to generate an elbow plot (less than 2 k values processed).")
}

# Save WSS results
write.csv(wss_df, file.path(tables_dir_080, "elbow_method_wss_results_post_1_removed.csv"), row.names = FALSE)
cat("WSS results saved to", file.path(tables_dir_080, "elbow_method_wss_results_post_1_removed.csv"), "\n")

# Function to find elbow (from script 076)
find_elbow <- function(x_coords, y_coords) {
  if (length(x_coords) < 3) return(length(x_coords))
  p1 <- c(x_coords[1], y_coords[1])
  pn <- c(x_coords[length(x_coords)], y_coords[length(y_coords)])
  line_v <- pn - p1
  distances_to_line <- numeric(length(x_coords))
  for (j in 1:length(x_coords)) {
    pt <- c(x_coords[j], y_coords[j])
    pt_v <- pt - p1
    line_len_sq_val <- sum(line_v^2)
    if (line_len_sq_val == 0) {
        distances_to_line[j] <- sum((pt - p1)^2)
        next
    }
    proj <- sum(pt_v * line_v) / line_len_sq_val
    proj <- max(0, min(1, proj))
    closest_pt_on_line <- p1 + proj * line_v
    distances_to_line[j] <- sqrt(sum((pt - closest_pt_on_line)^2))
  }
  return(which.max(distances_to_line))
}

optimal_k_calculated <- NA
if (nrow(wss_df) > 0) {
    idx_elbow <- find_elbow(wss_df$k, wss_df$tot_withinss)
    optimal_k_calculated <- wss_df$k[idx_elbow]
    cat("\nCalculated optimal k from elbow method (distance to line method):", optimal_k_calculated, "\n")
} else {
    cat("\nCould not determine optimal k from elbow method as no WSS results were generated.\n")
}

# 5. Generate and Save the Elbow Plot --------------------------------------
# Create a scaled version of WSS for plotting (divide by 100,000)
wss_df_plot <- wss_df %>%
  mutate(tot_withinss_scaled = tot_withinss / 100000)

# Create a dataframe for label positioning
wss_df_plot <- wss_df_plot %>%
  mutate(
    label_hjust = ifelse(k == 10, 0.5, -0.3),
    label_vjust = ifelse(k == 10, -0.5, 0.5)
  )

# Create base plot for publication
elbow_plot_obj <- ggplot(wss_df_plot, aes(x = k, y = tot_withinss_scaled)) +
  geom_line(color="#0072B2", linewidth=1) +
  geom_point(size = 3, color="#D55E00") +
  geom_text(aes(label = sprintf("%.1f", tot_withinss_scaled), 
                hjust = label_hjust, vjust = label_vjust), 
            size = 3.5, color = "black") +
  labs(
    x = "Number of Clusters (k)",
    y = "Total WSS (×10⁵)"
  ) +
  scale_x_continuous(breaks = wss_df_plot$k) +
  scale_y_continuous(labels = function(x) sprintf("%.1f", x)) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.major = element_line(colour = "grey90"),
    panel.grid.minor = element_blank(),
    axis.title = element_text(face = "bold")
  )

# Highlight k=4 point if it exists in the data
if (4 %in% wss_df_plot$k) {
  k4_data <- wss_df_plot[wss_df_plot$k == 4, ]
  elbow_plot_obj <- elbow_plot_obj + 
    geom_point(data = k4_data, aes(x = k, y = tot_withinss_scaled), 
               size = 5, color = "#E41A1C", shape = 21, fill = "#E41A1C", stroke = 2)
  cat("K=4 point highlighted on the plot.\n")
}

# Save as PDF
plot_file_pdf <- file.path(plot_dir_080, "elbow_plot_post_1_removed.pdf")
ggsave(plot_file_pdf, plot = elbow_plot_obj, width = 4.25, height = 2.975, dpi = 300)
cat("Elbow plot (PDF) saved to:", plot_file_pdf, "\n")

# Save as PNG
plot_file_png <- file.path(plot_dir_080, "elbow_plot_post_1_removed.png")
ggsave(plot_file_png, plot = elbow_plot_obj, width = 4.25, height = 2.975, dpi = 300)
cat("Elbow plot (PNG) saved to:", plot_file_png, "\n")

cat("\n============== SCRIPT R/080 (Optimal K Post 1 Removal) FINISHED ==============
") 