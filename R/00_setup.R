# BOAT2 Cluster and Factor Analysis - Setup Script
# This script loads all required libraries and defines common utility functions

# Automatically set working directory to project root
# This ensures that all paths work correctly regardless of where the script is called from
tryCatch(
  {
    # Method 1: If this script is sourced from run_all.R, maintain that directory
    # Check if script was already sourced and working directory is set
    if (file.exists("R/00_setup.R") && file.exists("data") && file.exists("results")) {
      cat("Working directory already correctly set to:", getwd(), "\n")
    } else {
      # Method 2: If run in RStudio directly
      if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
        script_path <- dirname(rstudioapi::getSourceEditorContext()$path)
        setwd(dirname(script_path)) # Set to parent directory of R folder
        cat("Working directory set using RStudio API to:", getwd(), "\n")
      } else {
        # Method 3: Try using this script's location
        script_path <- getSrcDirectory(function() {})
        if (length(script_path) > 0 && script_path != "") {
          setwd(dirname(script_path))
          cat("Working directory set using script location to:", getwd(), "\n")
        } else {
          cat("Could not automatically set working directory.\n")
          cat("Current working directory is:", getwd(), "\n")
          cat("Make sure to manually set working directory to project root.\n")
        }
      }
    }
  },
  error = function(e) {
    cat("Error trying to set working directory:", e$message, "\n")
    cat("Current working directory is:", getwd(), "\n")
  }
)

# 1. Required Libraries -------------------------------------------------------

# Define function to check and install packages
check_and_install <- function(package_name) {
  if (!requireNamespace(package_name, quietly = TRUE)) {
    cat(paste("Installing package:", package_name, "\n"))
    install.packages(package_name, repos = "https://cran.rstudio.com/")
  }
}

# Define required packages by category
data_packages <- c(
  "tidyverse", # Comprehensive package for data processing and visualization
  "dplyr", # Data manipulation and transformation
  "readr", # Efficient text data reading
  "magrittr" # Pipe operators
)

visualization_packages <- c(
  "ggplot2", # Declarative graphics creation
  "gridExtra", # Combine multiple ggplot graphics
  "scales", # Graphic scaling and labels
  "viridis", # Colorblind-friendly palette
  "RColorBrewer", # Classic color palettes
  "ggrepel", # Non-overlapping text labels
  "grid", # Base graphics system
  "fmsb" # Radar charts
)

clustering_packages <- c(
  "cluster", # Clustering algorithms and analysis
  "clustMixType", # Mixed data type clustering
  "NbClust", # Determine optimal number of clusters
  "dendextend", # Dendrogram enhancement and visualization
  "mclust", # Model-based clustering and adjustedRandIndex function
  "e1071" # Fuzzy c-means clustering
)

dimensionality_packages <- c(
  "factoextra", # PCA result visualization
  "umap" # Non-linear dimensionality reduction
)

# Additional utility packages
utility_packages <- c(
  "rstudioapi" # RStudio API for working directory setting
)

# Combine all packages
all_packages <- c(
  data_packages, visualization_packages,
  clustering_packages, dimensionality_packages,
  utility_packages
)

# Check and install all packages
for (pkg in all_packages) {
  check_and_install(pkg)
}

# Load all packages
invisible(lapply(all_packages, library, character.only = TRUE))

# 2. Common Utility Functions ------------------------------------------------

# Function to create output directories if they don't exist
create_output_dirs <- function() {
  dirs <- c(
    "results",
    "results/tables",
    "results/figures"
  )

  for (dir in dirs) {
    if (!dir.exists(dir)) {
      dir.create(dir, recursive = TRUE)
      cat(paste("Created directory:", dir, "\n"))
    }
  }
}

# Create standard color palettes
get_cluster_colors <- function(n) {
  if (n <= 9) {
    return(brewer.pal(max(3, n), "Set1")[1:n])
  } else {
    return(colorRampPalette(brewer.pal(9, "Set1"))(n))
  }
}

# Function to save plots with consistent naming
save_plot <- function(plot, filename, width = 10, height = 8) {
  ggsave(
    filename = file.path("results/figures", filename),
    plot = plot,
    width = width,
    height = height,
    dpi = 300
  )
  cat(paste("Saved plot to:", file.path("results/figures", filename), "\n"))
}

# Function to save data with consistent naming
save_data <- function(data, filename) {
  write.csv(
    x = data,
    file = file.path("results/tables", filename),
    row.names = FALSE
  )
  cat(paste("Saved data to:", file.path("results/tables", filename), "\n"))
}

# 3. Global Variables and Settings -------------------------------------------

# Set default theme for all ggplot2 plots
theme_set(theme_minimal(base_size = 12))

# Set random seed for reproducibility
set.seed(123)

# Create output directories
create_output_dirs()

# Print setup completion message
cat("Setup complete! All required packages are loaded.\n")
cat("Project directories initialized.\n")
