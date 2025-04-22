# BOAT2 Cluster and Factor Analysis - Main Script
# This script runs all analysis steps in sequence using the new modular structure

# Function to run selected analysis components
run_boat2_analysis <- function(
  do_setup = TRUE,
  do_data_prep = TRUE, 
  do_exploration = TRUE, 
  do_dimensionality = TRUE,
  do_hierarchical = TRUE,
  do_kprototype = TRUE,
  do_fuzzy = TRUE,
  do_reports = TRUE
) {
  
  # Automatically set working directory to project root
  # Try different methods to ensure it works in different environments
  tryCatch({
    # Method 1: If run in RStudio
    if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
      script_path <- dirname(rstudioapi::getSourceEditorContext()$path)
      setwd(dirname(script_path))  # Set to parent directory of R folder
      cat("Working directory set using RStudio API to:", getwd(), "\n")
    }
  }, error = function(e) {
    # Method 2: If not in RStudio, try using this script's location
    script_path <- getSrcDirectory(function(){})
    if (length(script_path) > 0 && script_path != "") {
      setwd(dirname(script_path))
      cat("Working directory set using script location to:", getwd(), "\n")
    } else {
      # Method 3: Assume the script is run from the project root or set manually
      cat("Could not automatically set working directory.\n")
      cat("Current working directory is:", getwd(), "\n")
      cat("Make sure you're running this from the project root directory.\n")
    }
  })

  # Record start time
  start_time <- Sys.time()
  cat("Starting BOAT2 Cluster and Factor Analysis...\n")
  cat("Start time:", format(start_time), "\n\n")

  # 1. Setup ----------------------------------------------------------------
  if (do_setup) {
    cat("1. Loading required packages and initializing environment...\n")
    source("R/00_setup.R")

    # Load common modules
    source("R/modules/visualization.R")
    source("R/modules/validation.R")
  } else {
    cat("1. Setup skipped.\n")
  }

  # 2. Data Preparation -----------------------------------------------------
  if (do_data_prep) {
    cat("\n2. Preparing and cleaning data...\n")
    source("R/01_data_preparation.R")
  } else {
    cat("\n2. Data preparation skipped.\n")
  }

  # 3. Exploratory Data Analysis --------------------------------------------
  if (do_exploration) {
    cat("\n3. Performing exploratory data analysis...\n")
    source("R/02_exploration.R")
  } else {
    cat("\n3. Exploratory data analysis skipped.\n")
  }

  # 4. Dimensionality Reduction ---------------------------------------------
  if (do_dimensionality) {
    cat("\n4. Performing dimensionality reduction (PCA & UMAP)...\n")
    source("R/03_dimensionality.R")
  } else {
    cat("\n4. Dimensionality reduction skipped.\n")
  }

  # 5. Hierarchical Clustering Analysis -------------------------------------
  if (do_hierarchical) {
    cat("\n5. Performing hierarchical clustering...\n")
    # Load hierarchical clustering module
    source("R/modules/clustering/hierarchical.R")
    # Run hierarchical clustering from the original script for backward compatibility
    source("R/04_clustering.R")
  } else {
    cat("\n5. Hierarchical clustering skipped.\n")
  }

  # 6. K-Prototype Clustering Analysis -------------------------------------
  if (do_kprototype) {
    cat("\n6. Performing K-Prototype clustering with different k values (2, 3, 7)...\n")
    # Run K-Prototype clustering analysis
    source("R/kprototype_analysis.R")
  } else {
    cat("\n6. K-Prototype clustering skipped.\n")
  }

  # 7. Fuzzy Clustering Analysis --------------------------------------------
  if (do_fuzzy) {
    cat("\n7. Performing fuzzy c-means clustering...\n")
    source("R/05_fuzzy_clustering.R")
  } else {
    cat("\n7. Fuzzy clustering skipped.\n")
  }

  # 8. Create Combined Report -----------------------------------------------
  if (do_reports) {
    cat("\n8. Creating combined report comparing all clustering methods...\n")

    # Create PDF for combined report
    pdf("results/figures/combined_clustering_comparison.pdf", width = 12, height = 9)

    # Title page
    plot.new()
    text(0.5, 0.7, "BOAT2 Cluster and Factor Analysis", cex = 2, font = 2)
    text(0.5, 0.6, "Comparison of Different Clustering Methods", cex = 1.5)
    text(0.5, 0.5, paste("Generated on:", Sys.Date()), cex = 1.2)

    # Load results from different methods
    if (file.exists("results/tables/hierarchical_clusters.csv") &&
        file.exists("results/tables/kproto_clusters_k3.csv") &&
        file.exists("results/tables/fuzzy_clustering_results.csv")) {
      
      # Load clustering results
      hierarchical <- read.csv("results/tables/hierarchical_clusters.csv")
      kproto_k3 <- read.csv("results/tables/kproto_clusters_k3.csv")
      fuzzy <- read.csv("results/tables/fuzzy_clustering_results.csv")
      
      # Create PDM distribution plots
      hierarchical_pdm <- create_pdm_distribution_plot(
        hierarchical,
        title = "PDM Distribution by Hierarchical Cluster"
      )
      
      kproto_pdm <- create_pdm_distribution_plot(
        kproto_k3,
        title = "PDM Distribution by K-Prototype Cluster (k=3)"
      )
      
      fuzzy_pdm <- create_pdm_distribution_plot(
        fuzzy %>% mutate(Cluster = HardCluster),
        title = "PDM Distribution by Fuzzy C-Means Cluster"
      )
      
      # Print PDM distributions
      print(hierarchical_pdm)
      print(kproto_pdm)
      print(fuzzy_pdm)
      
      # Compare variable importance across methods
      # This would depend on your specific analysis needs
    }

    # Close the PDF
    dev.off()
  } else {
    cat("\n8. Combined report creation skipped.\n")
  }

  # Record end time and calculate duration
  end_time <- Sys.time()
  duration <- end_time - start_time

  # Print completion message
  cat("\n========================================================\n")
  cat("BOAT2 Cluster and Factor Analysis Complete!\n")
  cat("Analysis started at:", format(start_time), "\n")
  cat("Analysis completed at:", format(end_time), "\n")
  cat("Total duration:", format(duration), "\n")
  cat("========================================================\n\n")

  cat("All results have been saved to the 'results' directory:\n")
  cat("- Tables: results/tables/\n")
  cat("- Figures: results/figures/\n\n")

  cat("Main output files include:\n")
  cat("- Hierarchical clustering results: hierarchical_clusters.csv\n")
  cat("- K-prototypes clustering results: kproto_clusters_k2.csv, kproto_clusters_k3.csv, kproto_clusters_k7.csv\n")
  cat("- Fuzzy clustering results: fuzzy_clustering_results.csv\n")
  cat("- PCA/UMAP results: dimensionality_reduction_results.csv\n\n")

  cat("To view the PDF reports, check:\n")
  cat("- Exploratory analysis: results/figures/key_variable_pairs.pdf\n")
  cat("- Dimensionality reduction: results/figures/dimensionality_reduction_analysis.pdf\n")
  cat("- Hierarchical clustering: results/figures/hierarchical_dendrogram.pdf\n")
  cat("- K-Prototype clustering: results/figures/kproto_comparison_report.pdf\n")
  cat("- K-Prototype detailed reports: results/figures/kproto_k2_report.pdf, kproto_k3_report.pdf, kproto_k7_report.pdf\n")
  cat("- Fuzzy clustering: results/figures/fuzzy_clustering_report.pdf\n")
  cat("- Combined comparison: results/figures/combined_clustering_comparison.pdf\n") 
}

# Parse command line arguments if running from command line
if (!interactive()) {
  args <- commandArgs(trailingOnly = TRUE)
  
  # Default: run all steps
  steps_to_run <- list()
  
  # Check for specific steps
  if (length(args) > 0) {
    # Check for step names in arguments
    steps_to_run$do_setup <- "setup" %in% args
    steps_to_run$do_data_prep <- "data" %in% args
    steps_to_run$do_exploration <- "explore" %in% args
    steps_to_run$do_dimensionality <- "dim" %in% args
    steps_to_run$do_hierarchical <- "hier" %in% args
    steps_to_run$do_kprototype <- "kproto" %in% args
    steps_to_run$do_fuzzy <- "fuzzy" %in% args
    steps_to_run$do_reports <- "report" %in% args
    
    # If "all" is specified, run everything
    if ("all" %in% args) {
      steps_to_run <- list(
        do_setup = TRUE, 
        do_data_prep = TRUE, 
        do_exploration = TRUE, 
        do_dimensionality = TRUE,
        do_hierarchical = TRUE,
        do_kprototype = TRUE,
        do_fuzzy = TRUE,
        do_reports = TRUE
      )
    }
    
    # If no specific steps were found in args, run all
    if (length(steps_to_run) == 0) {
      steps_to_run <- list(
        do_setup = TRUE, 
        do_data_prep = TRUE, 
        do_exploration = TRUE, 
        do_dimensionality = TRUE,
        do_hierarchical = TRUE,
        do_kprototype = TRUE,
        do_fuzzy = TRUE,
        do_reports = TRUE
      )
    }
    
    # Call with the specified steps
    do.call(run_boat2_analysis, steps_to_run)
  } else {
    # No arguments provided, run all steps
    run_boat2_analysis()
  }
} else {
  # If running interactively (e.g., in RStudio), run all steps by default
  # To run specific steps, call the function with specific parameters, e.g.:
  # run_boat2_analysis(do_hierarchical = FALSE, do_fuzzy = FALSE)
  run_boat2_analysis()
} 