# BOAT2 Cluster and Factor Analysis - Main Script
# This script runs all analysis steps in sequence from 010 to 050

# Function to run BOAT2 analysis
run_boat2_analysis <- function(
  do_setup = TRUE,
  do_data_prep = TRUE, 
  do_exploration = TRUE, 
  do_factoring = TRUE,
  do_fuzzy = TRUE
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
    source("R/000_setup.R")
  } else {
    cat("1. Setup skipped.\n")
  }

  # 2. Data Preparation -----------------------------------------------------
  if (do_data_prep) {
    cat("\n2. Preparing and cleaning data...\n")
    source("R/010_data_preparation.R")
    
    # Also run data dictionary creation if exists
    if (file.exists("R/011_create_data_dictionary.R")) {
      cat("\nCreating data dictionary...\n")
      source("R/011_create_data_dictionary.R")
    }
  } else {
    cat("\n2. Data preparation skipped.\n")
  }

  # 3. Exploratory Data Analysis --------------------------------------------
  if (do_exploration) {
    cat("\n3. Performing exploratory data analysis...\n")
    source("R/020_exploration.R")
  } else {
    cat("\n3. Exploratory data analysis skipped.\n")
  }

  # 4. K-Prototype Clustering Analysis -------------------------------------
  cat("\n4. Performing K-Prototype clustering analysis...\n")
  
  # Determine optimal number of clusters
  if (file.exists("R/031_kprototype_optimal_number.R")) {
    cat("\n4.1 Determining optimal number of clusters for K-Prototype...\n")
    source("R/031_kprototype_optimal_number.R")
  }
  
  # Perform K-Prototype analysis
  if (file.exists("R/032_kprototype_analysis.R")) {
    cat("\n4.2 Performing K-Prototype cluster analysis...\n")
    source("R/032_kprototype_analysis.R")
  }
  
  # PDM experience clustering
  if (file.exists("R/033_cluster_pdm_experience.R")) {
    cat("\n4.3 Clustering PDM experience data...\n")
    source("R/033_cluster_pdm_experience.R")
  }
  
  # Radar charts
  if (file.exists("R/034_radar_charts.R")) {
    cat("\n4.4 Creating radar charts for visualization...\n")
    source("R/034_radar_charts.R")
  }

  # 5. Factor Analysis -----------------------------------------------------
  if (do_factoring) {
    cat("\n5. Performing factor analysis...\n")
    source("R/040_Factoring.R")
    
    # Factor visualization
    if (file.exists("R/041_Factor_Visualization.R")) {
      cat("\n5.1 Creating factor visualizations...\n")
      source("R/041_Factor_Visualization.R")
    }
  } else {
    cat("\n5. Factor analysis skipped.\n")
  }

  # 6. Fuzzy Clustering Analysis --------------------------------------------
  if (do_fuzzy) {
    cat("\n6. Performing fuzzy c-means clustering...\n")
    source("R/050_fuzzy_clustering.R")
  } else {
    cat("\n6. Fuzzy clustering skipped.\n")
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

  cat("All results have been saved to the 'results' directory.\n")
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
    steps_to_run$do_factoring <- "factor" %in% args
    steps_to_run$do_fuzzy <- "fuzzy" %in% args
    
    # If "all" is specified, run everything
    if ("all" %in% args) {
      steps_to_run <- list(
        do_setup = TRUE, 
        do_data_prep = TRUE, 
        do_exploration = TRUE, 
        do_factoring = TRUE,
        do_fuzzy = TRUE
      )
    }
    
    # If no specific steps were found in args, run all
    if (length(steps_to_run) == 0) {
      steps_to_run <- list(
        do_setup = TRUE, 
        do_data_prep = TRUE, 
        do_exploration = TRUE, 
        do_factoring = TRUE,
        do_fuzzy = TRUE
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
  # run_boat2_analysis(do_factoring = FALSE, do_fuzzy = FALSE)
  run_boat2_analysis()
} 