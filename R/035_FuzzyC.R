# BOAT2 Cluster and Factor Analysis - Fuzzy C-means Clustering Analysis
# This script performs Fuzzy C-means clustering analysis and creates radar charts

# Load required libraries
if (!requireNamespace("e1071", quietly = TRUE)) install.packages("e1071")
if (!requireNamespace("fclust", quietly = TRUE)) install.packages("fclust")
if (!requireNamespace("cluster", quietly = TRUE)) install.packages("cluster")
if (!requireNamespace("fmsb", quietly = TRUE)) install.packages("fmsb")
if (!requireNamespace("RColorBrewer", quietly = TRUE)) install.packages("RColorBrewer")
if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
if (!requireNamespace("reshape2", quietly = TRUE)) install.packages("reshape2")

library(e1071)
library(fclust)
library(cluster)
library(fmsb)
library(RColorBrewer)
library(ggplot2)
library(reshape2)
library(dplyr)

# 1. Load Setup and Data -----------------------------------------------------
source("R/000_setup.R")

# Create subdirectories for results if they don't exist
dirs <- c(
  "results/figures/035_fuzzy_cmeans",
  "results/tables/035_fuzzy_cmeans"
)

for (dir in dirs) {
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE)
    cat(paste("Created directory:", dir, "\n"))
  }
}

# Load the dataset
cat("Loading dataset...\n")
data <- read.csv("data/BOAT2_Data_Enhanced.csv")
cat("Loaded dataset with", nrow(data), "rows and", ncol(data), "columns.\n")

# 2. Define variable groups ---------------------------------------------------
# Organization structure variables
org_vars <- c(
  "ORG_Employees",
  "ORG_Locations",
  "ORG_Departments",
  "ORG_Layers"
)

# Decision variables
dec_vars <- c(
  "DIST_Athority_Dispersion",
  "DIST_Athority_Delegation",
  "DIST_Process_InformalCommunication",
  "DIST_Process_InformalProcedure"
)

# Style variables
sty_vars <- c(
  "STY_DataDriven",
  "STY_Participation_Inclusion",
  "STY_Participation_Relational",
  "STY_Adaptive_Informal",
  "STY_Adaptive_Changeable",
  "STY_Authoritative_Threats",
  "STY_Authoritative_Compliance"
)

# Culture variables
cul_vars <- c(
  "CUL_Command",
  "CUL_Symbolic",
  "CUL_Formal",
  "CUL_Experimental",
  "CUL_Learning"
)

# Flexibility variables
flex_vars <- c(
  "FLEX_OpenToNewIdeas",
  "FLEX_OpenToChanges"
)

# Risk and environment variables
risk_env_vars <- c(
  "RISK_Tolerance",
  "ENV_SustainedGrowth",
  "ENV_HighriskIndustry",
  "ENV_IndustryStability"
)

# PDM variables (to exclude from clustering)
pdm_vars <- c(
  "PDM_Selected",
  "PDM_Experience_DBB",
  "PDM_Experience_DB",
  "PDM_Experience_PDB",
  "PDM_Experience_CMAR",
  "PDM_Experience_IPD"
)

# Define variable groups for radar charts
group1_vars <- c(dec_vars, sty_vars)
group2_vars <- c(cul_vars, flex_vars, risk_env_vars)

# All variables to be used for clustering (excluding PDM variables)
all_vars <- c(org_vars, group1_vars, group2_vars)

# Likert scale variables
likert_vars <- c(group1_vars, group2_vars)

# 3. Prepare data for clustering ----------------------------------------------
cat("Preparing data for clustering...\n")
cluster_data <- data[, all_vars]

# Handle missing values if any (replace with median)
for (col in names(cluster_data)) {
  if (any(is.na(cluster_data[[col]]))) {
    median_value <- median(cluster_data[[col]], na.rm = TRUE)
    cluster_data[[col]][is.na(cluster_data[[col]])] <- median_value
    cat(paste0("Filled missing values in ", col, " with median ", median_value, "\n"))
  }
}

# Scale organization variables separately since they're not Likert scales
org_data <- cluster_data[, org_vars]
org_data_scaled <- scale(org_data)
colnames(org_data_scaled) <- org_vars

# For Likert scale variables, keep as is (already in 1-5 range)
likert_data <- cluster_data[, likert_vars]

# Combine scaled data
X_processed <- cbind(org_data_scaled, likert_data)
cat(paste0("Processed data shape: ", nrow(X_processed), " rows and ", ncol(X_processed), " columns\n"))

# 4. Perform Fuzzy C-means clustering -----------------------------------------
cat("Performing Fuzzy C-means clustering with K=2...\n")
k <- 2  # Number of clusters
m <- 2  # Fuzziness parameter (usually between 1.5 and 2.5)

# Set seed for reproducibility
set.seed(123)

# Use Manhattan distance for Likert scale data
# Note: fclust package uses Euclidean by default, but we can specify the distance matrix
dist_matrix <- dist(X_processed, method = "manhattan")

# Perform Fuzzy C-means clustering
fcm_result <- fclust::FKM.gk(X_processed, k, m = m, stand = FALSE)

# Get cluster assignments and membership scores
cluster_membership <- apply(fcm_result$U, 1, which.max)
membership_scores <- apply(fcm_result$U, 1, max)

# Add results to original data
data$Cluster <- cluster_membership
data$Membership_Score <- membership_scores

# Calculate cluster sizes
cluster_sizes <- table(data$Cluster)
cat("Clustering complete!\n")
cat("Cluster sizes:", cluster_sizes, "\n")

# 5. Calculate cluster centers ------------------------------------------------
# Create a function to calculate cluster centers
calculate_cluster_centers <- function(data, all_vars, k) {
  centers <- data.frame(matrix(NA, nrow = length(all_vars), ncol = k))
  colnames(centers) <- paste0("Cluster_", 1:k)
  rownames(centers) <- all_vars
  
  # Calculate mean for each variable in each cluster
  for (i in 1:k) {
    cluster_data_i <- data[data$Cluster == i, ]
    centers[, i] <- colMeans(cluster_data_i[, all_vars], na.rm = TRUE)
  }
  
  return(centers)
}

# Calculate cluster centers and statistics
cluster_centers <- calculate_cluster_centers(data, all_vars, k)
cluster_centers_t <- t(cluster_centers)  # Transpose for easier visualization

# Calculate additional statistics for each cluster
calculate_cluster_stats <- function(data, all_vars, k) {
  stats <- list()
  
  for (i in 1:k) {
    cluster_name <- paste0("Cluster_", i)
    cluster_data_i <- data[data$Cluster == i, ]
    
    stats[[paste0(cluster_name, "_Size")]] <- nrow(cluster_data_i)
    stats[[paste0(cluster_name, "_Min")]] <- apply(cluster_data_i[, all_vars], 2, min, na.rm = TRUE)
    stats[[paste0(cluster_name, "_Max")]] <- apply(cluster_data_i[, all_vars], 2, max, na.rm = TRUE)
    stats[[paste0(cluster_name, "_Median")]] <- apply(cluster_data_i[, all_vars], 2, median, na.rm = TRUE)
  }
  
  return(stats)
}

cluster_stats <- calculate_cluster_stats(data, all_vars, k)

# 6. Save clustering results --------------------------------------------------
cat("Saving clustering results...\n")

# Save full clustering results
cluster_results_file <- "results/tables/035_fuzzy_cmeans/fuzzy_cmeans_cluster_results.csv"
write.csv(data, cluster_results_file, row.names = FALSE)
cat(paste0("Cluster results saved to ", cluster_results_file, "\n"))

# Save cluster centers
cluster_centers_file <- "results/tables/035_fuzzy_cmeans/fuzzy_cmeans_cluster_centers.csv"
write.csv(cluster_centers, cluster_centers_file)
cat(paste0("Cluster centers saved to ", cluster_centers_file, "\n"))

# 7. Create radar chart function ----------------------------------------------
cat("Generating radar charts...\n")

# Function to format variable names
format_variable_names <- function(var_names) {
  formatted_names <- var_names
  
  # Create mapping for better formatting
  name_mappings <- list(
    # Decision variables
    "DIST_Athority_Dispersion" = "DIST\nAuthority\nDispersion",
    "DIST_Athority_Delegation" = "DIST\nAuthority\nDelegation",
    "DIST_Process_InformalCommunication" = "DIST\nProcess\nInformalComm",
    "DIST_Process_InformalProcedure" = "DIST\nProcess\nInformalProc",
    
    # Style variables
    "STY_DataDriven" = "STY\nData\nDriven",
    "STY_Participation_Inclusion" = "STY\nParticipation\nInclusion",
    "STY_Participation_Relational" = "STY\nParticipation\nRelational",
    "STY_Adaptive_Informal" = "STY\nAdaptive\nInformal",
    "STY_Adaptive_Changeable" = "STY\nAdaptive\nChangeable",
    "STY_Authoritative_Threats" = "STY\nAuthoritative\nThreats",
    "STY_Authoritative_Compliance" = "STY\nAuthoritative\nCompliance",
    
    # Culture variables
    "CUL_Command" = "CUL\nCommand",
    "CUL_Symbolic" = "CUL\nSymbolic",
    "CUL_Formal" = "CUL\nFormal",
    "CUL_Experimental" = "CUL\nExperimental",
    "CUL_Learning" = "CUL\nLearning",
    
    # Flexibility variables
    "FLEX_OpenToNewIdeas" = "FLEX\nOpen To\nNew Ideas",
    "FLEX_OpenToChanges" = "FLEX\nOpen To\nChanges",
    
    # Risk and environment variables
    "RISK_Tolerance" = "RISK\nTolerance",
    "ENV_SustainedGrowth" = "ENV\nSustained\nGrowth",
    "ENV_HighriskIndustry" = "ENV\nHighrisk\nIndustry",
    "ENV_IndustryStability" = "ENV\nIndustry\nStability",
    
    # Organization structure
    "ORG_Employees" = "ORG\nEmployees",
    "ORG_Locations" = "ORG\nLocations",
    "ORG_Departments" = "ORG\nDepartments",
    "ORG_Layers" = "ORG\nLayers"
  )
  
  # Apply mappings
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

# Function to create a radar chart
create_radar_chart <- function(data, group_name, variables, title = NULL, scale_min = 1, scale_max = 5, 
                               width = 10, height = 8) {
  
  # Ensure data is a dataframe and set row names
  if(!is.data.frame(data)) {
    data <- as.data.frame(data)
  }
  
  # Check and ensure all variables exist
  missing_vars <- variables[!variables %in% colnames(data)]
  if(length(missing_vars) > 0) {
    cat("Warning: Variables don't exist:", paste(missing_vars, collapse=", "), "\n")
    variables <- variables[variables %in% colnames(data)]
    if(length(variables) == 0) {
      cat("Error: No usable variables to create radar chart\n")
      return(NULL)
    }
  }
  
  # Data preparation - ensure correct dataframe format
  radar_data <- data.frame(data[, variables, drop=FALSE])
  
  # Convert dataframe to matrix for easier manipulation
  radar_matrix <- as.matrix(radar_data)
  
  # Add max and min value rows
  max_min_matrix <- rbind(
    rep(scale_max, length(variables)),  # Max value
    rep(scale_min, length(variables)),  # Min value
    radar_matrix
  )
  
  # Convert back to dataframe and set row/column names
  radar_data_final <- as.data.frame(max_min_matrix)
  rownames(radar_data_final) <- c("max", "min", rownames(data))
  colnames(radar_data_final) <- variables
  
  # Format variable names
  formatted_names <- format_variable_names(variables)
  colnames(radar_data_final) <- formatted_names
  
  # Define cluster colors
  cluster_colors <- c("#4285F4", "#EA4335")  # Google colors for clusters 1 and 2
  
  # Create and save radar chart
  filename <- paste0("results/figures/035_fuzzy_cmeans/radar_", 
                    gsub(" ", "_", tolower(group_name)), ".pdf")
  
  # Ensure graphics device is properly opened and closed
  pdf(filename, width = width, height = height)
  
  # Set plot parameters
  par(mar = c(2, 2, 3, 2))
  
  # Draw radar chart
  tryCatch({
    radarchart(
      radar_data_final,
      pfcol = adjustcolor(cluster_colors[1:nrow(data)], alpha.f = 0.3),
      pcol = cluster_colors[1:nrow(data)],
      plty = 1,
      plwd = 2.5,
      cglcol = "gray70",
      cglty = 1,
      axislabcol = "gray30",
      calcex = 1.2,
      vlcex = 1.3,
      caxislabels = seq(scale_min, scale_max, (scale_max - scale_min) / 4),
      title = ifelse(is.null(title), 
                    paste0("Fuzzy C-means Clustering (K=2): ", group_name, " Variables"),
                    title),
      axistype = 1,
      titlecex = 1.4
    )
    
    # Add legend
    legend(
      "bottomright",
      legend = rownames(data),
      fill = adjustcolor(cluster_colors[1:nrow(data)], alpha.f = 0.3),
      col = cluster_colors[1:nrow(data)],
      lty = 1,
      lwd = 2,
      cex = 1.2,
      box.lty = 0
    )
  }, error = function(e) {
    cat("Error drawing radar chart:", e$message, "\n")
  })
  
  # Ensure device is closed
  dev.off()
  
  # Also save as PNG for easier viewing
  png_filename <- paste0("results/figures/035_fuzzy_cmeans/radar_", 
                       gsub(" ", "_", tolower(group_name)), ".png")
  
  # Open PNG device
  png(png_filename, width = width * 100, height = height * 100)
  
  # Set plot parameters
  par(mar = c(2, 2, 3, 2))
  
  # Draw radar chart to PNG
  tryCatch({
    radarchart(
      radar_data_final,
      pfcol = adjustcolor(cluster_colors[1:nrow(data)], alpha.f = 0.3),
      pcol = cluster_colors[1:nrow(data)],
      plty = 1,
      plwd = 2.5,
      cglcol = "gray70",
      cglty = 1,
      axislabcol = "gray30",
      calcex = 1.2,
      vlcex = 1.3,
      caxislabels = seq(scale_min, scale_max, (scale_max - scale_min) / 4),
      title = ifelse(is.null(title), 
                    paste0("Fuzzy C-means Clustering (K=2): ", group_name, " Variables"),
                    title),
      axistype = 1,
      titlecex = 1.4
    )
    
    # Add legend
    legend(
      "bottomright",
      legend = rownames(data),
      fill = adjustcolor(cluster_colors[1:nrow(data)], alpha.f = 0.3),
      col = cluster_colors[1:nrow(data)],
      lty = 1,
      lwd = 2,
      cex = 1.2,
      box.lty = 0
    )
  }, error = function(e) {
    cat("Error drawing PNG radar chart:", e$message, "\n")
  })
  
  # Ensure PNG device is closed
  dev.off()
  
  cat(paste0("  Created radar chart for ", group_name, ", saved to ", filename, " and ", png_filename, "\n"))
  
  return(filename)
}

# 8. Generate radar charts ----------------------------------------------------

# 1. All variables radar chart
create_radar_chart(
  cluster_centers_t, 
  "All", 
  all_vars,
  "Fuzzy C-means Clustering (K=2): All Variables"
)

# 2. Organization structure variables radar chart
# For organization variables, use data-driven scale
org_min <- min(cluster_centers_t[, org_vars])
org_max <- max(cluster_centers_t[, org_vars])
org_padding <- (org_max - org_min) * 0.1
create_radar_chart(
  cluster_centers_t, 
  "Organization Structure", 
  org_vars,
  "Fuzzy C-means Clustering (K=2): Organization Structure Variables",
  scale_min = max(0, org_min - org_padding),
  scale_max = org_max + org_padding
)

# 3. Group 1 variables radar chart (Decision & Style)
create_radar_chart(
  cluster_centers_t, 
  "Decision & Style", 
  group1_vars,
  "Fuzzy C-means Clustering (K=2): Decision & Style Variables"
)

# 4. Group 2 variables radar chart (Culture, Flexibility & Environment)
create_radar_chart(
  cluster_centers_t, 
  "Culture Flexibility Environment", 
  group2_vars,
  "Fuzzy C-means Clustering (K=2): Culture, Flexibility & Environment Variables"
)

# 9. Create PDM distribution chart -------------------------------------------
cat("Creating PDM distribution chart...\n")

create_pdm_distribution_chart <- function() {
  # Create cross-tabulation
  pdm_counts <- table(data$Cluster, data$PDM_Selected)
  
  # Convert to percentages
  pdm_pct <- prop.table(pdm_counts, margin = 1) * 100
  
  # Convert to data frame for ggplot
  pdm_df <- as.data.frame(pdm_pct)
  names(pdm_df) <- c("Cluster", "PDM", "Percentage")
  
  # Define PDM order
  pdm_levels <- c(
    "Design-Bid-Build",
    "Construction Manager @ Risk",
    "Design-Build",
    "Progressive Design-Build",
    "Integrated Project Delivery (IPD)"
  )
  
  # Filter to included PDMs and set factor levels
  pdm_df$PDM <- factor(pdm_df$PDM, levels = pdm_levels)
  
  # Define PDM colors
  pdm_colors <- c(
    "Design-Bid-Build" = "#D46A6A",             # Red (DBB)
    "Construction Manager @ Risk" = "#E3C567",  # Yellow (CMAR)
    "Design-Build" = "#9CCF9C",                 # Light green (DB)
    "Progressive Design-Build" = "#4A8F4A",     # Dark green (PDB)
    "Integrated Project Delivery (IPD)" = "#6A95CA" # Blue (IPD)
  )
  
  # Create short PDM names for labels
  pdm_short_names <- c(
    "Design-Bid-Build" = "DBB",
    "Construction Manager @ Risk" = "CMAR",
    "Design-Build" = "DB",
    "Progressive Design-Build" = "PDB",
    "Integrated Project Delivery (IPD)" = "IPD"
  )
  
  # Create the plot
  p <- ggplot(pdm_df, aes(x = Cluster, y = Percentage, fill = PDM)) +
    geom_bar(stat = "identity", position = "stack") +
    scale_fill_manual(values = pdm_colors, labels = pdm_short_names) +
    labs(
      title = "PDM Distribution by Fuzzy C-means Cluster",
      subtitle = paste0("Cluster sizes: ", paste(paste("Cluster", names(cluster_sizes), "=", cluster_sizes), collapse = ", ")),
      x = "Cluster",
      y = "Percentage",
      fill = "Project Delivery Method"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5, color = "gray40"),
      legend.position = "bottom"
    ) +
    # Add percentage labels
    geom_text(
      aes(label = sprintf("%.1f%%", Percentage)),
      position = position_stack(vjust = 0.5),
      color = "white",
      fontface = "bold"
    )
  
  # Save the plot
  pdf_file <- paste0("results/figures/035_fuzzy_cmeans/pdm_distribution.pdf")
  ggsave(pdf_file, p, width = 10, height = 8)
  
  # Also save as PNG
  png_file <- paste0("results/figures/035_fuzzy_cmeans/pdm_distribution.png")
  ggsave(png_file, p, width = 10, height = 8)
  
  cat("PDM distribution chart saved to", pdf_file, "and", png_file, "\n")
  
  return(p)
}

# Generate PDM distribution chart
pdm_dist_plot <- create_pdm_distribution_chart()

# 10. Create PDM experience analysis ------------------------------------------
cat("Creating PDM experience analysis...\n")

create_pdm_experience_chart <- function() {
  # Calculate mean PDM experience by cluster
  pdm_exp_vars <- c("PDM_Experience_DBB", "PDM_Experience_DB", 
                   "PDM_Experience_PDB", "PDM_Experience_CMAR", 
                   "PDM_Experience_IPD")
  
  pdm_exp_summary <- data %>%
    group_by(Cluster) %>%
    summarise(across(all_of(pdm_exp_vars), 
                     list(mean = ~mean(., na.rm = TRUE)),
                     .names = "{.col}_{.fn}")) %>%
    ungroup()
  
  # Save PDM experience summary
  pdm_exp_file <- "results/tables/035_fuzzy_cmeans/pdm_experience_by_cluster.csv"
  write.csv(pdm_exp_summary, pdm_exp_file, row.names = FALSE)
  
  # Reshape data for plotting
  pdm_exp_long <- pdm_exp_summary %>%
    pivot_longer(
      cols = -Cluster,
      names_to = "Experience_Type",
      values_to = "Mean_Value"
    ) %>%
    mutate(
      Experience_Type = gsub("PDM_Experience_(.+)_mean", "\\1", Experience_Type)
    )
  
  # Define PDM experience colors to match PDM distribution
  pdm_exp_colors <- c(
    "DBB" = "#D46A6A",   # Red
    "CMAR" = "#E3C567",  # Yellow
    "DB" = "#9CCF9C",    # Light green
    "PDB" = "#4A8F4A",   # Dark green
    "IPD" = "#6A95CA"    # Blue
  )
  
  # Create order for PDM experience
  pdm_exp_order <- c("DBB", "CMAR", "DB", "PDB", "IPD")
  pdm_exp_long$Experience_Type <- factor(pdm_exp_long$Experience_Type, levels = pdm_exp_order)
  
  # Create the plot
  p <- ggplot(pdm_exp_long, aes(x = Experience_Type, y = Mean_Value, fill = Experience_Type)) +
    geom_bar(stat = "identity") +
    facet_wrap(~ Cluster, labeller = labeller(Cluster = function(x) paste0("Cluster ", x))) +
    scale_fill_manual(values = pdm_exp_colors) +
    labs(
      title = "Mean PDM Experience by Fuzzy C-means Cluster",
      subtitle = "Higher values indicate more experience with a PDM type",
      x = "Project Delivery Method",
      y = "Mean Experience Score",
      fill = "PDM Type"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5, color = "gray40"),
      legend.position = "bottom",
      strip.background = element_rect(fill = "#F5F5F5"),
      strip.text = element_text(face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1)
    ) +
    # Add value labels
    geom_text(
      aes(label = sprintf("%.2f", Mean_Value)),
      vjust = -0.5,
      color = "black"
    )
  
  # Save the plot
  pdf_file <- paste0("results/figures/035_fuzzy_cmeans/pdm_experience.pdf")
  ggsave(pdf_file, p, width = 10, height = 8)
  
  # Also save as PNG
  png_file <- paste0("results/figures/035_fuzzy_cmeans/pdm_experience.png")
  ggsave(png_file, p, width = 10, height = 8)
  
  cat("PDM experience chart saved to", pdf_file, "and", png_file, "\n")
  
  return(p)
}

# Generate PDM experience chart
pdm_exp_plot <- create_pdm_experience_chart()

# Print completion message
cat("\nFuzzy C-means clustering analysis complete!\n")
cat("Results saved to: results/tables/035_fuzzy_cmeans/\n")
cat("Visualizations saved to: results/figures/035_fuzzy_cmeans/\n")
