# BOAT2 Cluster and Factor Analysis - K-Prototype Clustering Analysis
# This script performs K-prototype clustering analysis

# 1. Load Setup and Data -----------------------------------------------------
source("R/000_setup.R")

# Create subdirectories for results if they don't exist
dirs <- c(
  "results/figures/032_kprototype_analysis",
  "results/tables/032_kprototype_analysis"
)

for (dir in dirs) {
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE)
    cat(paste("Created directory:", dir, "\n"))
  }
}

# Load the enhanced dataset
data <- read.csv("data/BOAT2_Data_Enhanced.csv")
cat("Loaded dataset with", nrow(data), "rows and", ncol(data), "columns.\n")

# We'll analyze k=2, k=3, and k=4
kproto_k_values <- c(2, 3, 4)
cat("Will analyze using the following k values:", paste(kproto_k_values, collapse = ", "), "\n")

# 2. Define Variables --------------------------------------------------------

# Define numerical and categorical variables
# Organization structure variables (true numeric variables)
numerical_org_vars <- c(
  "ORG_Employees",
  "ORG_Locations",
  "ORG_Departments",
  "ORG_Layers"
)

# All variables except PDM_Selected and PDM experience variables
exclude_vars <- c("Owner_Type",
                  "PDM_Selected",
                  "PDM_Experience_DBB",
                  "PDM_Experience_DB",
                  "PDM_Experience_PDB",
                  "PDM_Experience_CMAR",
                  "PDM_Experience_IPD")
all_vars <- setdiff(colnames(data), exclude_vars)

# Likert scale variables (treated as categorical for k-prototype clustering)
categorical_vars <- setdiff(all_vars, numerical_org_vars)

# Check variables
cat("\nNumerical variables:", length(numerical_org_vars), "\n")
cat("Categorical variables:", length(categorical_vars), "\n")
cat("Total analysis variables:", length(all_vars), "\n")

# 3. Prepare data for K-prototypes clustering -------------------------------
cat("\nPreparing K-prototype clustering data...\n")

# Extract relevant columns
kproto_data <- data %>%
  select(all_of(c(numerical_org_vars, categorical_vars, "PDM_Selected", 
                  "PDM_Experience_DBB", "PDM_Experience_DB", "PDM_Experience_PDB", 
                  "PDM_Experience_CMAR", "PDM_Experience_IPD")))

# Check PDM_Selected column values
cat("\nPDM_Selected value distribution:\n")
print(table(kproto_data$PDM_Selected))

# Create mixed dataset for analysis
kproto_mixed_data <- kproto_data %>%
  select(all_of(c(numerical_org_vars, categorical_vars)))

# Convert categorical variables to ordered factors for k-prototype clustering
# For Likert scale data, we need to ensure it's treated as categorical
kproto_mixed_data <- kproto_mixed_data %>%
  mutate(across(all_of(categorical_vars), ~ ordered(round(.), levels = 1:5)))

# Print column names to check if using new variable names
cat("\nChecking clustering dataset column names:\n")
print(head(colnames(kproto_mixed_data)))

# Save preprocessed data for later use
save_data(kproto_data, "032_kprototype_analysis/kproto_data.csv")
cat("Preprocessed data saved successfully\n")

# 4. Perform K-Prototypes Clustering -----------------------------------------

# Create a list to store results
kproto_results <- list()

# Function to run k-prototypes clustering
run_kproto_analysis <- function(k_value) {
  # Set seed for reproducibility
  set.seed(123)
  
  cat("\nRunning K-Prototype clustering, k =", k_value, "...\n")
  
  # Run k-prototypes
  kproto_result <- clustMixType::kproto(
    kproto_mixed_data, 
    k = k_value,
    verbose = TRUE
  )
  
  # Create results dataframe - include all PDM variables
  cluster_results <- kproto_data %>%
    mutate(Cluster = kproto_result$cluster)
  
  # --- START: Add Cluster Renaming Logic Here ---
  # Rename clusters if needed
  if (k_value == 2) {
    cat("  Renaming clusters for k=2 (swapping 1 and 2)\\n")
    cluster_results <- cluster_results %>%
      mutate(Cluster = case_when(
        Cluster == 1 ~ 2, # Map original 1 to new 2
        Cluster == 2 ~ 1, # Map original 2 to new 1
        TRUE ~ Cluster
      ))
  } else if (k_value == 3) {
    cat("  Renaming clusters for k=3 (swapping 1 and 2, keeping 3)\\n")
    cluster_results <- cluster_results %>%
      mutate(Cluster = case_when(
        Cluster == 1 ~ 2, # Map original 1 to new 2
        Cluster == 2 ~ 1, # Map original 2 to new 1
        Cluster == 3 ~ 3, # Keep 3 as 3
        TRUE ~ Cluster
      ))
  }
  # --- END: Add Cluster Renaming Logic Here ---
  
  # Save k-prototypes clustering results (now with renamed clusters)
  save_data(
    cluster_results, 
    paste0("032_kprototype_analysis/kproto_clusters_k", k_value, ".csv")
  )
  cat("  Clustering results saved successfully\n")
  
  # Calculate cluster centroids
  centroids <- data.frame(
    Cluster = 1:k_value
  )
  
  # Add numerical variable centroids
  for (var in numerical_org_vars) {
    centroids[, var] <- kproto_result$centers[, var]
  }
  
  # Add categorical variable centroids
  for (var in categorical_vars) {
    centroids[, var] <- as.numeric(kproto_result$centers[, var])
  }
  
  # Save centroids
  save_data(
    centroids, 
    paste0("032_kprototype_analysis/kproto_centroids_k", k_value, ".csv")
  )
  cat("  Cluster centroids saved successfully\n")
  
  # Calculate cluster medians for Likert variables
  cat("  Calculating cluster medians...\n")
  
  # Create a dataframe to store medians
  medians <- data.frame(
    Cluster = 1:k_value
  )
  
  # Calculate medians for each variable
  all_analysis_vars <- c(numerical_org_vars, categorical_vars)
  for (var in all_analysis_vars) {
    # Calculate the median of the variable for each cluster
    for (cl in 1:k_value) {
      cluster_data <- cluster_results[cluster_results$Cluster == cl, var]
      if (var == all_analysis_vars[1]) {
        # Initialize row for this cluster with first variable
        medians[medians$Cluster == cl, var] <- median(cluster_data, na.rm = TRUE)
      } else {
        # Add to existing row for subsequent variables
        medians[medians$Cluster == cl, var] <- median(cluster_data, na.rm = TRUE)
      }
    }
  }
  
  # Save median data
  save_data(
    medians, 
    paste0("032_kprototype_analysis/kproto_medians_k", k_value, ".csv")
  )
  cat("  Cluster medians saved successfully\n")
  
  return(list(
    results = cluster_results,
    centroids = centroids,
    medians = medians,
    kproto_object = kproto_result
  ))
}

# Run k-prototypes for each k value
for (k in kproto_k_values) {
  kproto_results[[paste0("k", k)]] <- run_kproto_analysis(k)
}

# 5. Create PDM Distribution Analysis ----------------------------------------
cat("\nStarting PDM distribution analysis...\n")

# Function to create PDM distribution plot
create_pdm_plot <- function(cluster_results, k) {
  # Print dataset PDM_Selected value distribution
  cat("\nCluster results PDM_Selected distribution:\n")
  print(table(cluster_results$PDM_Selected))
  
  # Define PDM order (from bottom to top in stack: DBB, CMAR, DB, PDB, IPD)
  pdm_levels <- c(
    "Design-Bid-Build",              # Bottom
    "Construction Manager @ Risk", 
    "Design-Build", 
    "Progressive Design-Build", 
    "Integrated Project Delivery (IPD)"  # Top
  )
  
  # Define PDM names to abbreviations mapping
  pdm_short_names <- c(
    "Design-Bid-Build" = "DBB",
    "Construction Manager @ Risk" = "CMAR",
    "Design-Build" = "DB",
    "Progressive Design-Build" = "PDB",
    "Integrated Project Delivery (IPD)" = "IPD"
  )
  
  # Create color palette with low saturation colors as requested
  pdm_colors <- c(
    "Design-Bid-Build" = "#D46A6A",             # Red (DBB)
    "Construction Manager @ Risk" = "#E3C567",  # Yellow (CMAR)
    "Design-Build" = "#9CCF9C",                 # Light green (DB)
    "Progressive Design-Build" = "#4A8F4A",     # Dark green (PDB)
    "Integrated Project Delivery (IPD)" = "#6A95CA" # Blue (IPD)
  )
  
  # Create base data for plotting - start from scratch
  clusters <- sort(unique(cluster_results$Cluster))
  plot_data <- data.frame()
  
  # For each cluster, create a stacked rectangle data
  for (cl in clusters) {
    # PDM distribution for this cluster
    pdm_counts <- table(cluster_results$PDM_Selected[cluster_results$Cluster == cl])
    
    # Total for this cluster (for percentage calculation)
    cl_total <- sum(pdm_counts)
    
    # Starting y position
    y_start <- 0
    
    # For each PDM in our specified order (bottom to top)
    for (pdm in pdm_levels) {
      # Count for this PDM (may be 0)
      count <- ifelse(pdm %in% names(pdm_counts), pdm_counts[pdm], 0)
      
      if (count > 0) {
        # Calculate percentage
        pct <- count / cl_total * 100
        
        # Add rectangle data
        plot_data <- rbind(plot_data, data.frame(
          Cluster = cl,
          PDM = pdm,
          Count = count,
          Percentage = pct,
          ymin = y_start,
          ymax = y_start + count,
          mid_y = y_start + count/2, # Middle position for label
          stringsAsFactors = FALSE
        ))
        
        # Update y_start for next rectangle
        y_start <- y_start + count
      }
    }
  }
  
  # Create cluster totals data
  totals_data <- aggregate(Count ~ Cluster, data = plot_data, sum)
  names(totals_data) <- c("Cluster", "Total")
  
  # Ensure PDM is a factor with the desired order
  plot_data$PDM <- factor(plot_data$PDM, levels = pdm_levels)
  
  # Create the base plot
  p <- ggplot() +
    # Add rectangles manually
    geom_rect(
      data = plot_data,
      aes(
        xmin = as.numeric(Cluster) - 0.35,
        xmax = as.numeric(Cluster) + 0.35,
        ymin = ymin,
        ymax = ymax,
        fill = PDM
      )
    ) +
    # Add labels manually
    geom_text(
      data = plot_data,
      aes(
        x = as.numeric(Cluster),
        y = mid_y,
        label = paste0(Count, "\n(", round(Percentage, 1), "%)")
      ),
      color = "black",
      fontface = "bold",
      size = 3.5
    ) +
    # Add cluster total labels
    geom_text(
      data = totals_data,
      aes(
        x = as.numeric(Cluster),
        y = Total + 2,
        label = paste0("Cluster ", Cluster, "\nn=", Total)
      ),
      vjust = -0.5,
      color = "black",
      size = 4.5,
      fontface = "bold"
    ) +
    # Set colors
    scale_fill_manual(
      values = pdm_colors,
      # Use abbreviations as legend labels
      labels = pdm_short_names
    ) +
    # Configure axes
    scale_x_continuous(
      breaks = clusters,
      labels = paste("Cluster", clusters),
      limits = c(0.5, max(clusters) + 0.5)
    ) +
    labs(
      title = paste0("PDM Selected Distribution Across ", k, " Clusters"),
      subtitle = "Values show count and percentage within each cluster",
      x = NULL, # Remove x label (redundant with breaks)
      y = "Count",
      fill = "Project Delivery Method"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5, color = "gray40"),
      legend.position = "bottom",
      axis.title = element_text(face = "bold"),
      legend.title = element_text(face = "bold")
    ) +
    # Add more vertical space
    coord_cartesian(clip = "off") +
    scale_y_continuous(expand = expansion(mult = c(0, 0.2)))
  
  # Save chart with adjusted dimensions (taller)
  filename <- paste0("results/figures/032_kprototype_analysis/pdm_distribution_k", k, ".pdf")
  ggsave(filename, p, width = 8, height = 10, dpi = 300)
  
  # Also save as PNG for easier viewing
  png_filename <- paste0("results/figures/032_kprototype_analysis/pdm_distribution_k", k, ".png")
  ggsave(png_filename, p, width = 8, height = 10, dpi = 300)
  
  cat("PDM distribution plot saved to:", filename, "\n")
  cat("PDM distribution plot saved to:", png_filename, "\n")
  
  return(p)
}

# 6. Create Centroids and Medians Visualizations ------------------------------------------
cat("\nStarting cluster centroids and medians visualization...\n")

# Function to create centroids visualization
create_centroids_plot <- function(centroids, k, is_median = FALSE) {
  # Determine title type
  plot_type <- ifelse(is_median, "Medians", "Centroids")
  
  # Gather the data for easier plotting
  long_centroids <- centroids %>%
    pivot_longer(
      cols = -Cluster,
      names_to = "Variable",
      values_to = "Value"
    )
  
  # Add variable grouping for better organization
  long_centroids <- long_centroids %>%
    mutate(
      VariableGroup = case_when(
        Variable %in% numerical_org_vars ~ "Organization Structure",
        grepl("^DIST_", Variable) ~ "Decision Distribution",
        grepl("^STY_", Variable) ~ "Decision Style",
        grepl("^CUL_", Variable) ~ "Organization Culture",
        grepl("^FLEX_", Variable) ~ "Decision Flexibility",
        TRUE ~ "Other Variables"
      )
    )
  
  # Define a color palette for clusters
  cluster_colors <- c("#4285F4", "#EA4335", "#FBBC05", "#34A853", "#FF6D01", "#46BDC6", "#7B0099")
  
  # Create the plot
  p <- ggplot(long_centroids, aes(x = Variable, y = Value, color = factor(Cluster), group = Cluster)) +
    geom_line(linewidth = 1, alpha = 0.7) +
    geom_point(size = 2.5) +
    scale_color_manual(values = cluster_colors[1:k]) +
    labs(
      title = paste0("Cluster ", plot_type, " (k=", k, ")"),
      x = "Variable",
      y = "Value",
      color = "Cluster"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5, size = 14),
      axis.title = element_text(face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
      panel.grid.minor = element_blank(),
      legend.position = "bottom",
      legend.title = element_text(face = "bold")
    ) +
    # Add a horizontal line at the "neutral" value
    geom_hline(yintercept = 3, linetype = "dashed", color = "gray50")
  
  # Save the plot
  type_suffix <- ifelse(is_median, "medians", "centroids")
  filename <- paste0("results/figures/032_kprototype_analysis/", type_suffix, "_k", k, ".pdf")
  ggsave(filename, p, width = 14, height = 8, dpi = 300)
  cat("Cluster ", plot_type, " plot saved to:", filename, "\n")
  
  # Create a faceted version for better readability
  p_facet <- ggplot(long_centroids, aes(x = Variable, y = Value, color = factor(Cluster), group = Cluster)) +
    geom_line(linewidth = 1, alpha = 0.7) +
    geom_point(size = 2.5) +
    facet_wrap(~ VariableGroup, scales = "free_x") +
    scale_color_manual(values = cluster_colors[1:k]) +
    labs(
      title = paste0("Grouped Cluster ", plot_type, " (k=", k, ")"),
      x = "Variable",
      y = "Value",
      color = "Cluster"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5, size = 14),
      axis.title = element_text(face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
      panel.grid.minor = element_blank(),
      legend.position = "bottom",
      legend.title = element_text(face = "bold"),
      strip.background = element_rect(fill = "#E9ECEF"),
      strip.text = element_text(face = "bold", size = 10)
    ) +
    # Add a horizontal line at the "neutral" value
    geom_hline(yintercept = 3, linetype = "dashed", color = "gray50")
  
  # Save the faceted plot
  filename <- paste0("results/figures/032_kprototype_analysis/", type_suffix, "_faceted_k", k, ".pdf")
  ggsave(filename, p_facet, width = 16, height = 10, dpi = 300)
  cat("Grouped cluster ", plot_type, " plot saved to:", filename, "\n")
  
  return(list(main = p, faceted = p_facet))
}

# 7. Execute Analysis for Each k Value ---------------------------------------

pdm_plots <- list()
centroids_plots <- list()
medians_plots <- list()

for (k in kproto_k_values) {
  cat("\nAnalyzing cluster results for k =", k, "\n")
  
  # Get the results for this k
  cluster_results <- kproto_results[[paste0("k", k)]]$results
  centroids <- kproto_results[[paste0("k", k)]]$centroids
  medians <- kproto_results[[paste0("k", k)]]$medians
  
  # Create PDM distribution plot
  pdm_plots[[paste0("k", k)]] <- create_pdm_plot(cluster_results, k)
  
  # Create centroids plots
  centroids_plots[[paste0("k", k)]] <- create_centroids_plot(centroids, k, is_median = FALSE)
  
  # Create medians plots
  medians_plots[[paste0("k", k)]] <- create_centroids_plot(medians, k, is_median = TRUE)
}

# 8. Perform Statistical Analysis -------------------------------------------
cat("\nStarting statistical analysis...\n")

# Function to perform and visualize statistical tests
perform_statistical_analysis <- function(k) {
  # Get the clustering results for this k
  cluster_results <- kproto_results[[paste0("k", k)]]$results
  
  # Create a dataframe to store statistical test results
  stat_results <- data.frame(
    Variable = character(),
    Test = character(),
    Statistic = numeric(),
    P_Value = numeric(),
    Significant = logical(),
    stringsAsFactors = FALSE
  )
  
  # Function to run ANOVA for numerical variables
  run_anova <- function(variable) {
    formula <- as.formula(paste(variable, "~ factor(Cluster)"))
    model <- aov(formula, data = cluster_results)
    summary_table <- summary(model)
    
    # Extract p-value
    if (length(summary_table) > 0 && "Pr(>F)" %in% colnames(summary_table[[1]])) {
      p_value <- summary_table[[1]]["factor(Cluster)", "Pr(>F)"]
      f_value <- summary_table[[1]]["factor(Cluster)", "F value"]
      
      # Add to results
      stat_results <- rbind(stat_results, data.frame(
        Variable = variable,
        Test = "ANOVA",
        Statistic = f_value,
        P_Value = p_value,
        Significant = p_value < 0.05,
        stringsAsFactors = FALSE
      ))
    }
    
    return(stat_results)
  }
  
  # Function to run Kruskal-Wallis test for categorical variables
  run_kruskal <- function(variable) {
    # Convert to numeric for Kruskal-Wallis
    cluster_results[[variable]] <- as.numeric(cluster_results[[variable]])
    
    # Run Kruskal-Wallis test
    kw_result <- kruskal.test(as.formula(paste(variable, "~ factor(Cluster)")), data = cluster_results)
    
    # Extract results
    chi_squared <- kw_result$statistic
    p_value <- kw_result$p.value
    
    # Add to results
    stat_results <- rbind(stat_results, data.frame(
      Variable = variable,
      Test = "Kruskal-Wallis",
      Statistic = chi_squared,
      P_Value = p_value,
      Significant = p_value < 0.05,
      stringsAsFactors = FALSE
    ))
    
    return(stat_results)
  }
  
  # Run appropriate tests for each variable
  for (var in numerical_org_vars) {
    stat_results <- run_anova(var)
  }
  
  for (var in categorical_vars) {
    stat_results <- run_kruskal(var)
  }
  
  # Save statistical test results
  save_data(stat_results, paste0("032_kprototype_analysis/statistical_tests_k", k, ".csv"))
  
  # Create visualization of test results
  if (nrow(stat_results) > 0) {
    # Order by significance and p-value
    stat_results <- stat_results %>%
      arrange(Significant, P_Value)
    
    # Create bar plot of p-values
    p <- ggplot(stat_results, aes(x = reorder(Variable, -P_Value), y = -log10(P_Value), fill = Significant)) +
      geom_bar(stat = "identity") +
      geom_hline(yintercept = -log10(0.05), linetype = "dashed", color = "red") +
      scale_fill_manual(values = c("gray70", "#4285F4")) +
      labs(
        title = paste0("Statistical Tests for Cluster Differences (k=", k, ")"),
        subtitle = "Higher bars indicate stronger evidence of differences between clusters",
        x = "Variable",
        y = "-log10(p-value)",
        fill = "Significant at α=0.05"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(face = "bold", hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "top"
      )
    
    # Save plot
    filename <- paste0("results/figures/032_kprototype_analysis/statistical_tests_k", k, ".pdf")
    ggsave(filename, p, width = 14, height = 8, dpi = 300)
    cat("Statistical analysis plot saved to:", filename, "\n")
  } else {
    cat("No statistical test results available to visualize\n")
  }
  
  return(stat_results)
}

# Run statistical analysis for both k values
stat_results <- list()
for (k in kproto_k_values) {
  cat("\nPerforming statistical analysis for k =", k, "...\n")
  stat_results[[paste0("k", k)]] <- perform_statistical_analysis(k)
}

# 9. PDM Experience Analysis by Cluster --------------------------------------
cat("\nStarting PDM experience analysis by cluster...\n")

pdm_experience_vars <- c("PDM_Experience_DBB", "PDM_Experience_DB", 
                         "PDM_Experience_PDB", "PDM_Experience_CMAR", 
                         "PDM_Experience_IPD")

for (k in kproto_k_values) {
  cluster_results <- kproto_results[[paste0("k", k)]]$results
  
  # Calculate experience means by cluster
  experience_summary <- cluster_results %>%
    group_by(Cluster) %>%
    summarise(across(all_of(pdm_experience_vars), 
                     list(mean = ~mean(., na.rm = TRUE),
                          sd = ~sd(., na.rm = TRUE),
                          median = ~median(., na.rm = TRUE)),
                     .names = "{.col}_{.fn}"))
  
  # Save PDM experience summary
  save_data(experience_summary, 
            paste0("032_kprototype_analysis/pdm_experience_by_cluster_k", k, ".csv"))
  
  # Create visualization
  exp_plot_data <- experience_summary %>%
    select(Cluster, ends_with("_mean")) %>%
    pivot_longer(cols = -Cluster, 
                 names_to = "Experience_Type", 
                 values_to = "Mean_Value") %>%
    mutate(Experience_Type = gsub("PDM_Experience_(.+)_mean", "\\1", Experience_Type))
  
  # Define color mapping to match PDM distribution plot
  pdm_exp_colors <- c(
    "DBB" = "#D46A6A",   # Red
    "CMAR" = "#E3C567",  # Yellow
    "DB" = "#9CCF9C",    # Light green
    "PDB" = "#4A8F4A",   # Dark green
    "IPD" = "#6A95CA"    # Blue
  )
  
  exp_plot <- ggplot(exp_plot_data, aes(x = Experience_Type, y = Mean_Value, fill = Experience_Type)) +
    geom_bar(stat = "identity") +
    facet_wrap(~ Cluster, labeller = labeller(Cluster = function(x) paste0("Cluster ", x))) +
    scale_fill_manual(values = pdm_exp_colors) +
    labs(
      title = paste0("Mean PDM Experience by Cluster (k=", k, ")"),
      subtitle = "Higher values indicate more experience with a PDM type",
      x = "Project Delivery Method",
      y = "Mean Experience Score",
      fill = "PDM Type"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5),
      strip.background = element_rect(fill = "#E9ECEF"),
      strip.text = element_text(face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
  
  # Save plot
  filename <- paste0("results/figures/032_kprototype_analysis/pdm_experience_k", k, ".pdf")
  ggsave(filename, exp_plot, width = 10, height = 8, dpi = 300)
  cat("PDM experience plot saved to:", filename, "\n")
}

# Print completion message
cat("\nK-prototype clustering analysis complete!\n")
cat("Results saved to: results/tables/032_kprototype_analysis/\n")
cat("Visualizations saved to: results/figures/032_kprototype_analysis/\n") 

# Create cluster summary for all k values
cat("\nCreating cluster summary for all k values...\n")

# Create a data frame to store summary information
cluster_summary <- data.frame(
  k = numeric(),
  ClusterID = numeric(),
  Size = numeric(),
  SizePercentage = numeric(),
  DominantPDM = character(),
  DominantPDM_Percentage = numeric(),
  stringsAsFactors = FALSE
)

# For each k, add cluster information
for (k in kproto_k_values) {
  # Get cluster results
  cluster_results <- kproto_results[[paste0("k", k)]]$results
  
  # Calculate cluster sizes
  cluster_sizes <- table(cluster_results$Cluster)
  size_percentage <- round(prop.table(cluster_sizes) * 100, 1)
  
  # For each cluster in this k
  for (cl in 1:k) {
    # Calculate dominant PDM
    pdm_counts <- table(cluster_results$PDM_Selected[cluster_results$Cluster == cl])
    dominant_pdm <- names(pdm_counts)[which.max(pdm_counts)]
    dominant_pdm_pct <- round(max(pdm_counts) / sum(pdm_counts) * 100, 1)
    
    # Add to summary
    cluster_summary <- rbind(cluster_summary, data.frame(
      k = k,
      ClusterID = cl,
      Size = as.numeric(cluster_sizes[cl]),
      SizePercentage = size_percentage[cl],
      DominantPDM = dominant_pdm,
      DominantPDM_Percentage = dominant_pdm_pct,
      stringsAsFactors = FALSE
    ))
  }
}

# Save summary
save_data(cluster_summary, "032_kprototype_analysis/cluster_summary.csv")
cat("Cluster summary saved to: results/tables/032_kprototype_analysis/cluster_summary.csv\n") 