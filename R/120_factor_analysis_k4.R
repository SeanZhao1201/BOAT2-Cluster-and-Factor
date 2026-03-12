# R/120_factor_analysis_k4.R
# Factor Analysis on K4 Clustering Results
# This script performs factor analysis on the k=4 clustering results to identify
# underlying factor structures and examine how they differ across clusters.

# 0. Check and Install Required Packages -------------------------------------
cat("============== SCRIPT R/120 (Factor Analysis K4) STARTING ==============\n")

if (!requireNamespace("psych", quietly = TRUE)) install.packages("psych")
if (!requireNamespace("corrplot", quietly = TRUE)) install.packages("corrplot")
if (!requireNamespace("GPArotation", quietly = TRUE)) install.packages("GPArotation")
if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
if (!requireNamespace("readr", quietly = TRUE)) install.packages("readr")
if (!requireNamespace("tidyr", quietly = TRUE)) install.packages("tidyr")
if (!requireNamespace("RColorBrewer", quietly = TRUE)) install.packages("RColorBrewer")
if (!requireNamespace("gridExtra", quietly = TRUE)) install.packages("gridExtra")

library(psych)
library(corrplot)
library(GPArotation)
library(dplyr)
library(ggplot2)
library(readr)
library(tidyr)
library(RColorBrewer)
library(gridExtra)

# Attempt to source setup file
if (file.exists("R/000_setup.R")) {
  source("R/000_setup.R")
  cat("R/000_setup.R sourced successfully.\n")
} else {
  cat("Warning: R/000_setup.R not found. Essential packages loaded directly.\n")
}

# 1. Setup and Configuration ------------------------------------------------
# Create subdirectories for results
fig_dir_120 <- "results/figures/120_factor_analysis_k4"
tbl_dir_120 <- "results/tables/120_factor_analysis_k4"

for (dir_path in c(fig_dir_120, tbl_dir_120)) {
  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE)
    cat(paste("Created directory:", dir_path, "\n"))
  }
}

cat("Factor Analysis for k=4 clustering results will be performed.\n")

# 2. Load K4 Clustering Data ------------------------------------------------
cat("\n--- Loading k=4 clustering results ---\n")

# Load the k4 clustering data
cluster_data_path <- "results/tables/090_kprototype_post_1_removal_k4/kproto_clusters_k4_post_1_removed.csv"
if (!file.exists(cluster_data_path)) {
  stop(paste("Required clustering data file not found:", cluster_data_path))
}

cluster_data <- read_csv(cluster_data_path, show_col_types = FALSE)
cat("Loaded clustering data with", nrow(cluster_data), "rows and", ncol(cluster_data), "columns.\n")

# 3. Define Variables for Factor Analysis -----------------------------------
cat("\n--- Defining variables for factor analysis ---\n")

# Variables to EXCLUDE from factor analysis
exclude_vars <- c(
  "Owner_Type",           # Owner type (public/private)
  "Project_Success",      # Outcome variable
  "PDM_Selected",         # Project delivery method
  "PDM_Experience_DBB",   # Experience variables
  "PDM_Experience_DB",
  "PDM_Experience_PDB",
  "PDM_Experience_CMAR",
  "PDM_Experience_IPD",
  "Original_Row_ID",      # ID variable
  "Cluster_k4"           # Cluster assignment
)

# All available variables
all_vars <- colnames(cluster_data)

# Variables for factor analysis (exclude the ones listed above)
factor_vars <- setdiff(all_vars, exclude_vars)

# Separate into numerical and categorical (Likert) variables
numerical_vars <- c(
  "ORG_Employees", "ORG_Locations", "ORG_Departments", "ORG_Layers"
)

# Likert scale variables (treated as continuous for factor analysis)
likert_vars <- setdiff(factor_vars, numerical_vars)

cat("Variables for factor analysis:\n")
cat("  Numerical variables:", length(numerical_vars), "\n")
cat("  Likert scale variables:", length(likert_vars), "\n")
cat("  Total variables:", length(factor_vars), "\n")

# Verify all variables exist in the data
missing_vars <- factor_vars[!factor_vars %in% colnames(cluster_data)]
if (length(missing_vars) > 0) {
  cat("Warning: The following variables are missing from the data:\n")
  cat(paste(missing_vars, collapse = ", "), "\n")
  factor_vars <- factor_vars[factor_vars %in% colnames(cluster_data)]
}

cat("Final factor analysis variables (", length(factor_vars), "):\n")
print(factor_vars)

# 4. Prepare Data for Factor Analysis ---------------------------------------
cat("\n--- Preparing data for factor analysis ---\n")

# Extract factor analysis data
fa_data <- cluster_data %>%
  select(all_of(c(factor_vars, "Cluster_k4"))) %>%
  rename(Cluster = Cluster_k4)

# Check for missing values
missing_summary <- fa_data %>%
  summarise(across(everything(), ~ sum(is.na(.))))

cat("Missing values summary:\n")
print(missing_summary)

# Remove any rows with missing values
fa_data_complete <- fa_data %>%
  drop_na()

cat("After removing missing values: ", nrow(fa_data_complete), " complete cases\n")

# Extract just the variables for factor analysis (without cluster)
fa_matrix <- fa_data_complete %>%
  select(all_of(factor_vars)) %>%
  as.data.frame()

# 5. Exploratory Data Analysis ----------------------------------------------
cat("\n--- Exploratory Data Analysis ---\n")

# Correlation matrix
cor_matrix <- cor(fa_matrix)

# Save correlation matrix
write.csv(cor_matrix, file.path(tbl_dir_120, "correlation_matrix.csv"))

# Create correlation plot
pdf(file.path(fig_dir_120, "correlation_matrix.pdf"), width = 12, height = 12)
corrplot(cor_matrix, method = "color", type = "upper", order = "hclust",
         tl.cex = 0.8, tl.col = "black", tl.srt = 45,
         title = "Correlation Matrix of Variables for Factor Analysis",
         mar = c(0,0,1,0))
dev.off()

cat("Correlation matrix plot saved.\n")

# Check data suitability for factor analysis
# Kaiser-Meyer-Olkin (KMO) test
kmo_result <- KMO(fa_matrix)
cat("Kaiser-Meyer-Olkin (KMO) Measure of Sampling Adequacy:", round(kmo_result$MSA, 3), "\n")

# Bartlett's test of sphericity
bartlett_result <- cortest.bartlett(cor_matrix, n = nrow(fa_matrix))
cat("Bartlett's Test of Sphericity p-value:", format(bartlett_result$p.value, scientific = TRUE), "\n")

# Save suitability tests
suitability_results <- data.frame(
  Test = c("KMO", "Bartlett_p_value"),
  Value = c(kmo_result$MSA, bartlett_result$p.value)
)
write.csv(suitability_results, file.path(tbl_dir_120, "factor_analysis_suitability_tests.csv"), row.names = FALSE)

# 6. Determine Optimal Number of Factors ------------------------------------
cat("\n--- Determining optimal number of factors ---\n")

# Scree plot
pdf(file.path(fig_dir_120, "scree_plot.pdf"), width = 10, height = 6)
scree_result <- fa.parallel(fa_matrix, fm = "pa", fa = "fa", main = "Scree Plot - Parallel Analysis")
dev.off()

cat("Parallel analysis suggests", scree_result$nfact, "factors\n")

# Eigenvalue analysis
eigenvalues <- eigen(cor_matrix)$values
n_factors_kaiser <- sum(eigenvalues > 1)
cat("Kaiser criterion (eigenvalue > 1) suggests", n_factors_kaiser, "factors\n")

# Create eigenvalue plot
eigen_df <- data.frame(
  Factor = 1:length(eigenvalues),
  Eigenvalue = eigenvalues
)

p_eigen <- ggplot(eigen_df, aes(x = Factor, y = Eigenvalue)) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "red", size = 2) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
  labs(title = "Eigenvalue Plot", 
       subtitle = "Kaiser Criterion: Eigenvalue > 1",
       x = "Factor Number", 
       y = "Eigenvalue") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))

ggsave(file.path(fig_dir_120, "eigenvalue_plot.pdf"), p_eigen, width = 10, height = 6)

# Save factor determination results
factor_determination <- data.frame(
  Method = c("Parallel Analysis", "Kaiser Criterion"),
  Suggested_Factors = c(scree_result$nfact, n_factors_kaiser)
)
write.csv(factor_determination, file.path(tbl_dir_120, "factor_number_determination.csv"), row.names = FALSE)

# 7. Perform Factor Analysis ------------------------------------------------
cat("\n--- Performing factor analysis ---\n")

# Use the number of factors suggested by parallel analysis
n_factors <- max(2, scree_result$nfact)  # At least 2 factors
cat("Using", n_factors, "factors for analysis\n")

# Principal Axis Factoring with Varimax rotation
fa_result <- fa(fa_matrix, nfactors = n_factors, rotate = "varimax", fm = "pa")

cat("Factor Analysis Results:\n")
print(fa_result)

# Extract factor loadings
loadings_matrix <- fa_result$loadings[]
loadings_df <- as.data.frame(loadings_matrix)
loadings_df$Variable <- rownames(loadings_df)

# Save factor loadings
write.csv(loadings_df, file.path(tbl_dir_120, "factor_loadings.csv"), row.names = FALSE)

# Create factor loadings heatmap
loadings_long <- loadings_df %>%
  pivot_longer(cols = -Variable, names_to = "Factor", values_to = "Loading")

p_loadings <- ggplot(loadings_long, aes(x = Factor, y = Variable, fill = Loading)) +
  geom_tile() +
  scale_fill_gradient2(low = "red", mid = "white", high = "blue", midpoint = 0,
                      limits = c(-1, 1)) +
  labs(title = "Factor Loadings Heatmap",
       x = "Factor", y = "Variable") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text.y = element_text(size = 8),
        plot.title = element_text(face = "bold", hjust = 0.5))

ggsave(file.path(fig_dir_120, "factor_loadings_heatmap.pdf"), p_loadings, width = 10, height = 12)

# 8. Calculate Factor Scores ------------------------------------------------
cat("\n--- Calculating factor scores ---\n")

# Calculate factor scores for each observation
factor_scores <- as.data.frame(fa_result$scores)
colnames(factor_scores) <- paste0("Factor", 1:n_factors)

# Add cluster information
factor_scores_with_cluster <- cbind(factor_scores, 
                                   Cluster = fa_data_complete$Cluster)

# Save factor scores
write.csv(factor_scores_with_cluster, file.path(tbl_dir_120, "factor_scores_by_cluster.csv"), row.names = FALSE)

# 9. Analyze Factor Differences Across Clusters -----------------------------
cat("\n--- Analyzing factor differences across clusters ---\n")

# Calculate mean factor scores by cluster
cluster_factor_means <- factor_scores_with_cluster %>%
  group_by(Cluster) %>%
  summarise(across(starts_with("Factor"), mean, .names = "Mean_{.col}"),
            n = n(), .groups = "drop")

write.csv(cluster_factor_means, file.path(tbl_dir_120, "cluster_factor_means.csv"), row.names = FALSE)

# Create boxplots for each factor by cluster
factor_boxplots <- list()

for (i in 1:n_factors) {
  factor_col <- paste0("Factor", i)
  
  p <- ggplot(factor_scores_with_cluster, aes(x = factor(Cluster), y = .data[[factor_col]], 
                                             fill = factor(Cluster))) +
    geom_boxplot(alpha = 0.7) +
    geom_jitter(width = 0.2, alpha = 0.5) +
    scale_fill_brewer(palette = "Set2") +
    labs(title = paste("Factor", i, "Scores by Cluster"),
         x = "Cluster", y = paste("Factor", i, "Score"),
         fill = "Cluster") +
    theme_minimal() +
    theme(plot.title = element_text(face = "bold", hjust = 0.5),
          legend.position = "none")
  
  factor_boxplots[[i]] <- p
  
  # Save individual plots
  ggsave(file.path(fig_dir_120, paste0("factor", i, "_boxplot_by_cluster.pdf")), 
         p, width = 8, height = 6)
}

# Create combined boxplot
if (n_factors <= 4) {
  combined_boxplot <- do.call(grid.arrange, c(factor_boxplots, ncol = 2))
  ggsave(file.path(fig_dir_120, "all_factors_boxplots_combined.pdf"), 
         combined_boxplot, width = 12, height = 8)
}

# 10. Statistical Tests for Cluster Differences -----------------------------
cat("\n--- Performing statistical tests for cluster differences ---\n")

# ANOVA tests for each factor
anova_results <- data.frame(
  Factor = character(),
  F_statistic = numeric(),
  p_value = numeric(),
  stringsAsFactors = FALSE
)

for (i in 1:n_factors) {
  factor_col <- paste0("Factor", i)
  formula_str <- paste(factor_col, "~ factor(Cluster)")
  anova_result <- aov(as.formula(formula_str), data = factor_scores_with_cluster)
  anova_summary <- summary(anova_result)
  
  anova_results <- rbind(anova_results, data.frame(
    Factor = factor_col,
    F_statistic = anova_summary[[1]][1, "F value"],
    p_value = anova_summary[[1]][1, "Pr(>F)"]
  ))
}

write.csv(anova_results, file.path(tbl_dir_120, "anova_results_factors_by_cluster.csv"), row.names = FALSE)

cat("ANOVA Results for Factor Differences by Cluster:\n")
print(anova_results)

# 11. Create Factor Interpretation Table ------------------------------------
cat("\n--- Creating factor interpretation table ---\n")

# Identify variables with high loadings for each factor (|loading| > 0.4)
factor_interpretation <- data.frame(
  Factor = character(),
  High_Loading_Variables = character(),
  Interpretation = character(),
  stringsAsFactors = FALSE
)

for (i in 1:n_factors) {
  factor_name <- paste0("PA", i)
  high_loadings <- loadings_df[abs(loadings_df[[factor_name]]) > 0.4, ]
  high_loadings <- high_loadings[order(abs(high_loadings[[factor_name]]), decreasing = TRUE), ]
  
  var_list <- paste(high_loadings$Variable, "(", round(high_loadings[[factor_name]], 2), ")", 
                   collapse = "; ")
  
  # Simple interpretation based on variable patterns
  interpretation <- ""
  if (any(grepl("STY_", high_loadings$Variable))) {
    interpretation <- paste(interpretation, "Decision Style")
  }
  if (any(grepl("CUL_", high_loadings$Variable))) {
    interpretation <- paste(interpretation, "Organizational Culture")
  }
  if (any(grepl("DIST_", high_loadings$Variable))) {
    interpretation <- paste(interpretation, "Authority Distribution")
  }
  if (any(grepl("FLEX_", high_loadings$Variable))) {
    interpretation <- paste(interpretation, "Flexibility")
  }
  if (any(grepl("ORG_", high_loadings$Variable))) {
    interpretation <- paste(interpretation, "Organizational Structure")
  }
  
  factor_interpretation <- rbind(factor_interpretation, data.frame(
    Factor = paste("Factor", i),
    High_Loading_Variables = var_list,
    Interpretation = trimws(interpretation)
  ))
}

write.csv(factor_interpretation, file.path(tbl_dir_120, "factor_interpretation.csv"), row.names = FALSE)

# 12. Create Comprehensive Summary Report -----------------------------------
cat("\n--- Creating summary report ---\n")

# Factor analysis summary
fa_summary <- data.frame(
  Metric = c("Number of Variables", "Number of Factors", "Total Variance Explained", 
             "KMO Measure", "Bartlett Test p-value"),
  Value = c(length(factor_vars), n_factors, 
            round(sum(fa_result$Vaccounted[2,]), 3),
            round(kmo_result$MSA, 3),
            format(bartlett_result$p.value, scientific = TRUE))
)

write.csv(fa_summary, file.path(tbl_dir_120, "factor_analysis_summary.csv"), row.names = FALSE)

# Cluster sample sizes
cluster_sizes <- table(factor_scores_with_cluster$Cluster)
cluster_size_df <- data.frame(
  Cluster = names(cluster_sizes),
  Sample_Size = as.numeric(cluster_sizes)
)
write.csv(cluster_size_df, file.path(tbl_dir_120, "cluster_sample_sizes.csv"), row.names = FALSE)

cat("\nFactor Analysis Results Summary:\n")
cat("- Variables analyzed:", length(factor_vars), "\n")
cat("- Number of factors extracted:", n_factors, "\n")
cat("- Total variance explained:", round(sum(fa_result$Vaccounted[2,]), 1), "%\n")
cat("- KMO measure:", round(kmo_result$MSA, 3), "\n")

cat("
Factor analysis for k=4 clustering results completed.
Results saved to respective subdirectories under results/figures and results/tables.
")
cat("============== SCRIPT R/120 (Factor Analysis K4) FINISHED ==============\n") 