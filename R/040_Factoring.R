# BOAT2 Cluster and Factor Analysis - Factor Analysis
# This script performs factor analysis on the decision-making and organizational variables

# Load required libraries
if (!requireNamespace("psych", quietly = TRUE)) install.packages("psych")
if (!requireNamespace("GPArotation", quietly = TRUE)) install.packages("GPArotation")
if (!requireNamespace("corrplot", quietly = TRUE)) install.packages("corrplot")
if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
if (!requireNamespace("gridExtra", quietly = TRUE)) install.packages("gridExtra")

library(psych)
library(GPArotation)
library(corrplot)
library(ggplot2)
library(gridExtra)
library(dplyr)

# 1. Load Setup and Data -----------------------------------------------------
source("R/000_setup.R")

# Create subdirectories for results if they don't exist
dirs <- c(
  "results/figures/040_factoring",
  "results/tables/040_factoring"
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

# 2. Define variables for factor analysis ------------------------------------
# Organization structure variables (numeric)
numerical_org_vars <- c(
  "ORG_Employees",
  "ORG_Locations",
  "ORG_Departments",
  "ORG_Layers"
)

# Decision-making variables (Likert scale)
ordinal_vars <- c(
  # Decision distribution variables
  "DIST_Athority_Dispersion", 
  "DIST_Athority_Delegation",
  "DIST_Process_InformalCommunication", 
  "DIST_Process_InformalProcedure",
  
  # Decision style variables
  "STY_DataDriven",
  "STY_Participation_Inclusion", 
  "STY_Participation_Relational",
  "STY_Adaptive_Informal", 
  "STY_Adaptive_Changeable",
  "STY_Authoritative_Threats", 
  "STY_Authoritative_Compliance",
  
  # Organizational culture variables
  "CUL_Command",
  "CUL_Symbolic",
  "CUL_Formal",
  "CUL_Experimental",
  "CUL_Learning",
  
  # Decision flexibility variables
  "FLEX_OpenToNewIdeas",
  "FLEX_OpenToChanges",
  
  # Risk and environment variables
  "RISK_Tolerance",
  "ENV_SustainedGrowth",
  "ENV_HighriskIndustry",
  "ENV_IndustryStability"
)

# 3. Prepare data for analysis ----------------------------------------------
cat("Preparing data for factor analysis...\n")

# Handle missing values if any (replace with median)
fa_data <- data[, ordinal_vars]
for (col in names(fa_data)) {
  if (any(is.na(fa_data[[col]]))) {
    median_value <- median(fa_data[[col]], na.rm = TRUE)
    fa_data[[col]][is.na(fa_data[[col]])] <- median_value
    cat(paste0("Filled missing values in ", col, " with median ", median_value, "\n"))
  }
}

# 4. Exploratory Analysis ---------------------------------------------------
cat("Performing exploratory analysis...\n")

# Correlation matrix
corr_matrix <- cor(fa_data, use = "pairwise.complete.obs")

# Save correlation matrix
write.csv(corr_matrix, "results/tables/040_factoring/correlation_matrix.csv")

# Plot correlation matrix
pdf("results/figures/040_factoring/correlation_matrix.pdf", width = 12, height = 10)
corrplot(corr_matrix, method = "color", type = "upper", order = "hclust",
         tl.col = "black", tl.cex = 0.7, diag = FALSE)
title("Correlation Matrix of Decision Variables")
dev.off()

# Also save as PNG for easier viewing
png("results/figures/040_factoring/correlation_matrix.png", width = 1200, height = 1000)
corrplot(corr_matrix, method = "color", type = "upper", order = "hclust",
         tl.col = "black", tl.cex = 0.7, diag = FALSE)
title("Correlation Matrix of Decision Variables")
dev.off()

# Check sampling adequacy
cat("Checking sampling adequacy (KMO)...\n")
kmo_result <- KMO(fa_data)
cat("KMO overall MSA =", kmo_result$MSA, "\n")
write.csv(kmo_result$MSAi, "results/tables/040_factoring/kmo_values.csv")

# Bartlett's test of sphericity
cat("Performing Bartlett's test of sphericity...\n")
bartlett_test <- cortest.bartlett(corr_matrix, n = nrow(fa_data))
cat("Bartlett's test: chi-squared =", bartlett_test$chisq, 
    ", df =", bartlett_test$df, 
    ", p-value =", bartlett_test$p.value, "\n")

# 5. Determine number of factors --------------------------------------------
cat("Determining optimal number of factors...\n")

# Parallel analysis
parallel_result <- fa.parallel(fa_data, fm = "minres", fa = "fa")

# Save parallel analysis plot
pdf("results/figures/040_factoring/parallel_analysis.pdf", width = 10, height = 8)
fa.parallel(fa_data, fm = "minres", fa = "fa")
dev.off()

# Also save as PNG
png("results/figures/040_factoring/parallel_analysis.png", width = 1000, height = 800)
fa.parallel(fa_data, fm = "minres", fa = "fa")
dev.off()

# Based on parallel analysis, determine the number of factors
suggested_factors <- parallel_result$nfact
cat("Parallel analysis suggests", suggested_factors, "factors.\n")

# Also try different numbers of factors
n_factors_to_try <- c(4, 5, 6, 7)
cat("Will also try these numbers of factors:", paste(n_factors_to_try, collapse = ", "), "\n")

# 6. Perform factor analysis with different number of factors -----------------
cat("Performing factor analysis with different numbers of factors...\n")

# Function to run factor analysis and save results
run_factor_analysis <- function(n_factors) {
  cat("\nRunning factor analysis with", n_factors, "factors...\n")
  
  # Perform factor analysis with oblique rotation (promax)
  fa_result <- fa(fa_data, 
                  nfactors = n_factors, 
                  rotate = "promax", 
                  fm = "minres",
                  scores = "regression")
  
  # Save factor loadings
  loadings_file <- paste0("results/tables/040_factoring/factor_loadings_", n_factors, ".csv")
  write.csv(fa_result$loadings, loadings_file)
  
  # Save factor scores
  scores_file <- paste0("results/tables/040_factoring/factor_scores_", n_factors, ".csv")
  factor_scores <- as.data.frame(fa_result$scores)
  # Add original data for reference
  factor_scores <- cbind(data[, c("PDM_Selected")], factor_scores)
  write.csv(factor_scores, scores_file)
  
  # Create factor loading plot
  pdf_file <- paste0("results/figures/040_factoring/factor_loadings_", n_factors, ".pdf")
  pdf(pdf_file, width = 12, height = 10)
  fa.diagram(fa_result, main = paste0("Factor Analysis (", n_factors, " Factors)"),
             cut = 0.3, simple = TRUE)
  dev.off()
  
  # Also save as PNG
  png_file <- paste0("results/figures/040_factoring/factor_loadings_", n_factors, ".png")
  png(png_file, width = 1200, height = 1000)
  fa.diagram(fa_result, main = paste0("Factor Analysis (", n_factors, " Factors)"),
             cut = 0.3, simple = TRUE)
  dev.off()
  
  # Return the result
  return(fa_result)
}

# Run factor analysis with suggested number of factors and alternatives
fa_results <- list()
fa_results[[paste0("fa_", suggested_factors)]] <- run_factor_analysis(suggested_factors)

for (n in n_factors_to_try) {
  fa_results[[paste0("fa_", n)]] <- run_factor_analysis(n)
}

# 7. Analyze factor solutions and select the best one -----------------------
cat("\nAnalyzing factor solutions...\n")

# Compile metrics for each solution
solution_metrics <- data.frame(
  N_Factors = c(suggested_factors, n_factors_to_try),
  Complexity = NA,
  Proportion_Explained = NA
)

for (i in 1:nrow(solution_metrics)) {
  n <- solution_metrics$N_Factors[i]
  result <- fa_results[[paste0("fa_", n)]]
  
  # Calculate average item complexity
  solution_metrics$Complexity[i] <- mean(result$complexity)
  
  # Calculate proportion of variance explained
  solution_metrics$Proportion_Explained[i] <- sum(result$Vaccounted[1,])
}

# Save metrics
write.csv(solution_metrics, "results/tables/040_factoring/solution_metrics.csv")
cat("Factor solution metrics:\n")
print(solution_metrics)

# Calculate factor interpretability scores (subjective - higher is better)
# This would normally be done manually by examining the factor loadings
# and assessing how interpretable each solution is

# 8. Select the best solution and create detailed analysis ------------------
# Based on parallel analysis, metrics, and interpretability
# Select the best solution (for now, using the one from parallel analysis)
best_n_factors <- suggested_factors
cat("\nSelected solution with", best_n_factors, "factors as the best.\n")

# Get the best solution
best_solution <- fa_results[[paste0("fa_", best_n_factors)]]

# Create a detailed table of factor loadings with only significant loadings
significant_loadings <- function(fa_obj, cutoff = 0.3) {
  loadings <- as.data.frame(unclass(fa_obj$loadings))
  
  # Create a matrix to hold significant loadings
  sig_loadings <- matrix(NA, nrow = nrow(loadings), ncol = ncol(loadings))
  rownames(sig_loadings) <- rownames(loadings)
  colnames(sig_loadings) <- colnames(loadings)
  
  # Only include loadings above cutoff
  for (i in 1:nrow(loadings)) {
    for (j in 1:ncol(loadings)) {
      if (abs(loadings[i, j]) >= cutoff) {
        sig_loadings[i, j] <- loadings[i, j]
      }
    }
  }
  
  return(as.data.frame(sig_loadings))
}

# Get significant loadings
sig_load <- significant_loadings(best_solution, cutoff = 0.3)
write.csv(sig_load, paste0("results/tables/040_factoring/significant_loadings_", best_n_factors, ".csv"))

# Create factor names based on the variables loading on each factor
create_factor_names <- function(sig_loadings, n_factors) {
  factor_names <- character(n_factors)
  
  for (i in 1:n_factors) {
    # Get the variables with significant loadings on this factor
    col_name <- paste0("MR", i)
    if (col_name %in% colnames(sig_loadings)) {
      vars <- rownames(sig_loadings)[!is.na(sig_loadings[[col_name]])]
      
      if (length(vars) > 0) {
        # Use the top 3 loadings to create a name
        top_vars <- vars[order(abs(sig_loadings[vars, col_name]), decreasing = TRUE)[1:min(3, length(vars))]]
        factor_names[i] <- paste0("Factor ", i, ": ", paste(top_vars, collapse = " + "))
      } else {
        factor_names[i] <- paste0("Factor ", i)
      }
    } else {
      factor_names[i] <- paste0("Factor ", i)
    }
  }
  
  return(factor_names)
}

# Create factor names
factor_names <- create_factor_names(sig_load, best_n_factors)
write.csv(data.frame(Factor = 1:best_n_factors, Name = factor_names), 
          "results/tables/040_factoring/factor_names.csv")

# 9. Create factor score distributions by PDM ------------------------------
cat("Creating factor score distributions by PDM...\n")

# Function to create factor score boxplots by PDM
create_factor_boxplots <- function(factor_scores, factor_names) {
  n_factors <- ncol(factor_scores) - 1  # -1 for PDM_Selected column
  
  # Create a list to store plots
  plots <- list()
  
  # Convert PDM_Selected to factor with ordered levels
  pdm_levels <- c(
    "Design-Bid-Build",
    "Construction Manager @ Risk", 
    "Design-Build", 
    "Progressive Design-Build", 
    "Integrated Project Delivery (IPD)"
  )
  
  factor_scores$PDM_Selected <- factor(factor_scores$PDM_Selected, 
                                      levels = pdm_levels)
  
  # Create boxplot for each factor
  for (i in 1:n_factors) {
    factor_col <- paste0("MR", i)
    
    if (factor_col %in% colnames(factor_scores)) {
      p <- ggplot(factor_scores, aes_string(x = "PDM_Selected", y = factor_col, fill = "PDM_Selected")) +
        geom_boxplot() +
        labs(title = paste0("Factor ", i, " Scores by PDM"),
             subtitle = factor_names[i],
             x = "Project Delivery Method",
             y = "Factor Score",
             fill = "PDM") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1),
              plot.title = element_text(face = "bold", hjust = 0.5),
              plot.subtitle = element_text(hjust = 0.5)) +
        scale_fill_brewer(palette = "Set1")
      
      plots[[i]] <- p
    }
  }
  
  return(plots)
}

# Get factor scores with PDM
factor_scores <- as.data.frame(cbind(
  PDM_Selected = data$PDM_Selected,
  best_solution$scores
))

# Create boxplots
factor_boxplots <- create_factor_boxplots(factor_scores, factor_names)

# Save individual boxplots
for (i in 1:length(factor_boxplots)) {
  pdf_file <- paste0("results/figures/040_factoring/factor", i, "_by_pdm.pdf")
  png_file <- paste0("results/figures/040_factoring/factor", i, "_by_pdm.png")
  
  pdf(pdf_file, width = 10, height = 7)
  print(factor_boxplots[[i]])
  dev.off()
  
  png(png_file, width = 1000, height = 700)
  print(factor_boxplots[[i]])
  dev.off()
}

# 10. Print final summary -------------------------------------------------
cat("\nFactor Analysis Summary:\n")
cat("Best solution:", best_n_factors, "factors\n")
cat("Proportion of variance explained:", 
    solution_metrics$Proportion_Explained[solution_metrics$N_Factors == best_n_factors], "\n")
cat("Factor names:\n")
for (i in 1:length(factor_names)) {
  cat("  ", factor_names[i], "\n")
}

cat("\nFactor analysis complete!\n")
cat("Results saved to results/tables/040_factoring/ and results/figures/040_factoring/\n")
