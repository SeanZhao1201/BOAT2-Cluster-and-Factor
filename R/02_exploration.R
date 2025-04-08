# BOAT2 Cluster and Factor Analysis - Exploratory Data Analysis
# This script performs exploratory analysis and visualization on the prepared data

# 1. Load Setup and Data -----------------------------------------------------
source("R/00_setup.R")

# Load the prepared datasets
hierarchical_data <- read.csv("results/tables/hierarchical_data.csv")
kproto_data <- read.csv("results/tables/kproto_data.csv")
fuzzy_data <- read.csv("results/tables/fuzzy_data.csv")

# Define variable groups (from data preparation script)
# Organizational structure variables
numerical_org_vars <- c(
  "Org_Structure_Employees",
  "Org_Structure_Locations",
  "Org_Structure_Depts",
  "Org_Structure_Layers"
)

# Decision-making variables (merged)
merged_ordinal_vars <- c(
  "Distribution_Centralization",
  "Distribution_Formalization",
  "Style_Technocracy",
  "Style_Participation",
  "Style_Organicity",
  "Style_Coercion",
  "Culture_Command",
  "Culture_Symbolic",
  "Culture_Rationale",
  "Culture_Generative",
  "Culture_Transactive",
  "Flexibility_openness",
  "Flexibility_Recursiveness",
  "Risk",
  "Environment_Growth",
  "Environment_Hostile",
  "Environment_Stable"
)

# 2. Distribution Analysis ---------------------------------------------------

# 2.1 Distribution of organizational structure variables
org_structure_plots <- list()

for (var in numerical_org_vars) {
  p <- ggplot(fuzzy_data, aes_string(x = var)) +
    geom_histogram(bins = 15, fill = "steelblue", color = "white") +
    geom_density(alpha = 0.3, fill = "red") +
    labs(
      title = paste("Distribution of", gsub("_", " ", var)),
      x = gsub("Org_Structure_", "", var),
      y = "Frequency"
    ) +
    theme_minimal()

  org_structure_plots[[var]] <- p
}

# Combine plots
org_structure_combined <- gridExtra::grid.arrange(
  grobs = org_structure_plots,
  ncol = 2
)

# Save the combined plot
ggsave("results/figures/org_structure_distributions.pdf",
  org_structure_combined,
  width = 10, height = 8
)

# 2.2 Distribution of ordinal decision variables
decision_var_plots <- list()

for (var in merged_ordinal_vars) {
  p <- ggplot(fuzzy_data, aes_string(x = var)) +
    geom_histogram(bins = 10, fill = "darkgreen", color = "white") +
    labs(
      title = paste("Distribution of", gsub("_", " ", var)),
      x = var,
      y = "Frequency"
    ) +
    theme_minimal()

  decision_var_plots[[var]] <- p
}

# Save individual plots for decision variables
for (var in names(decision_var_plots)) {
  save_plot(decision_var_plots[[var]],
    paste0("decision_var_", var, ".pdf"),
    width = 8, height = 6
  )
}

# 3. PDM Analysis -----------------------------------------------------------

# 3.1 PDM frequency
# Check if PDM_Type exists in the dataset
if ("PDM_Type" %in% colnames(fuzzy_data)) {
  pdm_plot <- ggplot(fuzzy_data, aes(x = PDM_Type, fill = PDM_Type)) +
    geom_bar() +
    geom_text(stat = "count", aes(label = after_stat(count)), vjust = -0.5) +
    labs(
      title = "Project Delivery Method Distribution",
      x = "Project Delivery Method",
      y = "Count"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  # Save PDM plot
  save_plot(pdm_plot, "pdm_distribution.pdf", width = 8, height = 6)
} else {
  cat("Warning: PDM_Type column not found in fuzzy_data. Skipping PDM analysis.\n")
}

# 3.2 PDM vs Organizational Structure
if ("PDM_Type" %in% colnames(fuzzy_data)) {
  pdm_org_plots <- list()

  for (var in numerical_org_vars) {
    p <- ggplot(fuzzy_data, aes_string(x = "PDM_Type", y = var, fill = "PDM_Type")) +
      geom_boxplot() +
      labs(
        title = paste("Relationship of PDM and", gsub("_", " ", var)),
        x = "Project Delivery Method",
        y = gsub("Org_Structure_", "", var)
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none"
      )

    pdm_org_plots[[var]] <- p
  }

  # Combine PDM vs Org Structure plots
  pdm_org_combined <- gridExtra::grid.arrange(
    grobs = pdm_org_plots,
    ncol = 2
  )

  # Save combined plot
  ggsave("results/figures/pdm_org_structure_relationships.pdf",
    pdm_org_combined,
    width = 12, height = 10
  )
} else {
  cat("Warning: PDM_Type column not found in fuzzy_data. Skipping PDM vs Org Structure analysis.\n")
}

# 4. Correlation Analysis ---------------------------------------------------

# 4.1 Correlation matrix of decision-making variables
decision_cor <- cor(fuzzy_data[, merged_ordinal_vars], use = "pairwise.complete.obs")

# Create correlation plot
pdf("results/figures/decision_correlation_matrix.pdf", width = 12, height = 10)
corrplot::corrplot(
  decision_cor,
  method = "color",
  type = "upper",
  order = "hclust",
  tl.col = "black",
  tl.srt = 45,
  addCoef.col = "black",
  number.cex = 0.7,
  diag = FALSE,
  title = "Correlation Matrix of Decision-Making Variables",
  mar = c(0, 0, 2, 0)
)
dev.off()

# 4.2 Correlations with organizational structure
# Calculate correlations between decision variables and organizational structure
org_decision_cor <- cor(
  fuzzy_data[, c(numerical_org_vars, merged_ordinal_vars)],
  use = "pairwise.complete.obs"
)

# Extract just the correlations between org structure and decision variables
org_decision_subset <- org_decision_cor[
  numerical_org_vars,
  merged_ordinal_vars,
  drop = FALSE
]

# Create correlation heatmap
org_decision_heatmap <- ggplot(
  data = reshape2::melt(org_decision_subset),
  aes(x = Var2, y = Var1, fill = value)
) +
  geom_tile() +
  scale_fill_gradient2(
    low = "blue",
    mid = "white",
    high = "red",
    midpoint = 0,
    limits = c(-1, 1)
  ) +
  labs(
    title = "Correlations: Organization Structure vs Decision Variables",
    x = "Decision Variables",
    y = "Organization Structure",
    fill = "Correlation"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    axis.text.y = element_text(size = 8)
  )

# Save the heatmap
save_plot(org_decision_heatmap, "org_decision_correlations.pdf", width = 14, height = 8)

# 5. Multidimensional Analysis -----------------------------------------------

# 5.1 Simple scatterplot matrix of key variables
# Select a subset of important variables for visualization
key_vars <- c(
  "Distribution_Centralization",
  "Distribution_Formalization",
  "Style_Participation",
  "Culture_Symbolic",
  "Flexibility_openness"
)

# Create scatterplot matrix
if ("PDM_Type" %in% colnames(fuzzy_data)) {
  pairs_plot <- GGally::ggpairs(
    fuzzy_data,
    columns = key_vars,
    mapping = aes(color = PDM_Type),
    upper = list(continuous = "cor"),
    lower = list(continuous = "points")
  ) +
    theme_minimal() +
    labs(title = "Relationships Among Key Decision Variables")
} else {
  pairs_plot <- GGally::ggpairs(
    fuzzy_data,
    columns = key_vars,
    upper = list(continuous = "cor"),
    lower = list(continuous = "points")
  ) +
    theme_minimal() +
    labs(title = "Relationships Among Key Decision Variables")
}

# Save pairs plot
ggsave("results/figures/key_variable_pairs.pdf",
  pairs_plot,
  width = 12, height = 10
)

# 6. Summary Statistics by PDM Type ------------------------------------------

# Calculate summary statistics by PDM Type
if ("PDM_Type" %in% colnames(fuzzy_data)) {
  pdm_summaries <- fuzzy_data %>%
    group_by(PDM_Type) %>%
    summarise(across(
      all_of(c(numerical_org_vars, merged_ordinal_vars)),
      list(
        mean = ~ mean(., na.rm = TRUE),
        sd = ~ sd(., na.rm = TRUE),
        median = ~ median(., na.rm = TRUE)
      )
    ))

  # Save summary statistics
  save_data(pdm_summaries, "pdm_variable_summaries.csv")
} else {
  cat("Warning: PDM_Type column not found in fuzzy_data. Skipping PDM summary statistics.\n")
}

# 7. ANOVA Analysis for PDM Differences --------------------------------------

if ("PDM_Type" %in% colnames(fuzzy_data)) {
  # Function to run ANOVA and extract p-values
  run_anova <- function(variable, data) {
    formula <- as.formula(paste(variable, "~ PDM_Type"))
    model <- aov(formula, data = data)
    summary_table <- summary(model)
    p_value <- summary_table[[1]]["PDM_Type", "Pr(>F)"]
    return(p_value)
  }

  # Run ANOVA for each decision variable
  anova_results <- data.frame(
    Variable = character(),
    P_Value = numeric(),
    Significant = logical()
  )

  for (var in c(numerical_org_vars, merged_ordinal_vars)) {
    p_val <- run_anova(var, fuzzy_data)
    anova_results <- rbind(anova_results, data.frame(
      Variable = var,
      P_Value = p_val,
      Significant = p_val < 0.05
    ))
  }

  # Sort by p-value
  anova_results <- anova_results[order(anova_results$P_Value), ]

  # Save ANOVA results
  save_data(anova_results, "pdm_anova_results.csv")

  # Create visualization of ANOVA results
  anova_plot <- ggplot(anova_results, aes(x = reorder(Variable, -P_Value), y = -log10(P_Value), fill = Significant)) +
    geom_bar(stat = "identity") +
    geom_hline(yintercept = -log10(0.05), linetype = "dashed", color = "red") +
    scale_fill_manual(values = c("gray70", "darkred")) +
    labs(
      title = "ANOVA Results: Variables Differing by PDM Type",
      subtitle = "Higher bars indicate stronger evidence of difference; red line at p=0.05",
      x = "Variable",
      y = "-log10(p-value)"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  # Save ANOVA plot
  save_plot(anova_plot, "pdm_anova_plot.pdf", width = 12, height = 8)
} else {
  cat("Warning: PDM_Type column not found in fuzzy_data. Skipping ANOVA analysis.\n")
}

# Print completion message
cat("\nExploratory Data Analysis complete!\n")
cat("Visualizations saved to results/figures/\n")
cat("Summary statistics saved to results/tables/\n")
