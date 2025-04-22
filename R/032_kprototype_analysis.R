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

# We'll analyze k=2 and k=3 only
kproto_k_values <- c(2, 3)
cat("Will analyze using the following k values:", paste(kproto_k_values, collapse = ", "), "\n")

# 2. Define Variables --------------------------------------------------------

# Define numerical and categorical variables
# Organization structure variables (true numeric variables)
numerical_org_vars <- c(
  "ORG_Size_Employees",
  "ORG_Complexity_Locations",
  "ORG_Complexity_Departments",
  "ORG_Hierarchy_Layers"
)

# All variables except PDM_Selected and PDM experience variables
exclude_vars <- c("PDM_Selected", 
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
  
  # Save k-prototypes clustering results
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
  
  # 创建一个数据框来存储中位数
  medians <- data.frame(
    Cluster = 1:k_value
  )
  
  # 为每个变量计算中位数
  all_analysis_vars <- c(numerical_org_vars, categorical_vars)
  for (var in all_analysis_vars) {
    # 对每个聚类计算变量的中位数
    for (cl in 1:k_value) {
      cluster_data <- cluster_results[cluster_results$Cluster == cl, var]
      if (var == all_analysis_vars[1]) {
        # 第一个变量时初始化该聚类的行
        medians[medians$Cluster == cl, var] <- median(cluster_data, na.rm = TRUE)
      } else {
        # 后续变量添加到已有的行
        medians[medians$Cluster == cl, var] <- median(cluster_data, na.rm = TRUE)
      }
    }
  }
  
  # 保存中位数数据
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
  
  # Define PDM名称与简写的映射
  pdm_short_names <- c(
    "Design-Bid-Build" = "DBB",
    "Construction Manager @ Risk" = "CMAR",
    "Design-Build" = "DB",
    "Progressive Design-Build" = "PDB",
    "Integrated Project Delivery (IPD)" = "IPD"
  )
  
  # Create color palette with low saturation colors as requested
  pdm_colors <- c(
    "Design-Bid-Build" = "#D46A6A",             # 红色 (DBB)
    "Construction Manager @ Risk" = "#E3C567",  # 黄色 (CMAR)
    "Design-Build" = "#9CCF9C",                 # 浅绿色 (DB)
    "Progressive Design-Build" = "#4A8F4A",     # 深绿色 (PDB)
    "Integrated Project Delivery (IPD)" = "#6A95CA" # 蓝色 (IPD)
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
      # 使用简写名称作为图例标签
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
  # 确定标题类型
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
        grepl("^DEC_", Variable) ~ "Decision Distribution",
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
      
      return(stat_results)
    }
    
    return(NULL)
  }
  
  # Function to run Chi-square test for categorical variables (PDM_Selected)
  run_chi_square <- function() {
    chi_test <- chisq.test(table(cluster_results$PDM_Selected, cluster_results$Cluster))
    
    # Add to results
    stat_results <- rbind(stat_results, data.frame(
      Variable = "PDM_Selected",
      Test = "Chi-square",
      Statistic = chi_test$statistic,
      P_Value = chi_test$p.value,
      Significant = chi_test$p.value < 0.05,
      stringsAsFactors = FALSE
    ))
    
    return(stat_results)
  }
  
  # Run ANOVA for numerical organization variables
  for (var in numerical_org_vars) {
    cat("   Analyzing organizational structure variable:", var, "\n")
    temp_results <- run_anova(var)
    if (!is.null(temp_results)) {
      stat_results <- temp_results
    }
  }
  
  # Run ANOVA for categorical decision variables
  for (var in categorical_vars) {
    cat("   Analyzing decision variable:", var, "\n")
    temp_results <- run_anova(var)
    if (!is.null(temp_results)) {
      stat_results <- temp_results
    }
  }
  
  # Run Chi-square test for PDM_Selected
  cat("   Analyzing PDM_Selected\n")
  temp_results <- run_chi_square()
  if (!is.null(temp_results)) {
    stat_results <- temp_results
  }
  
  # Save statistical results
  save_data(
    stat_results, 
    paste0("032_kprototype_analysis/statistical_tests_k", k, ".csv")
  )
  
  # Create visualization of statistical results
  if (nrow(stat_results) > 0) {
    # Sort by p-value
    stat_results <- stat_results[order(stat_results$P_Value), ]
    
    # Create plot of significant variables
    p <- ggplot(stat_results, aes(x = reorder(Variable, -P_Value), y = -log10(P_Value), fill = Significant)) +
      geom_bar(stat = "identity", alpha = 0.9) +
      geom_hline(yintercept = -log10(0.05), linetype = "dashed", color = "#D55E00", size = 1) +
      scale_fill_manual(values = c("#BBBBBB", "#2D708E"), labels = c("No", "Yes")) +
      labs(
        title = paste0("Cluster Difference Significance Analysis (k=", k, ")"),
        subtitle = "Bar height represents significance of differences; red line is p=0.05 significance level",
        x = "Variable",
        y = "-log10(p-value)",
        fill = "Significant Difference"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5, color = "gray30"),
        axis.title = element_text(face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
        legend.position = "top"
      )
    
    # Save the plot
    filename <- paste0("results/figures/032_kprototype_analysis/statistical_tests_k", k, ".pdf")
    ggsave(filename, p, width = 12, height = 8, dpi = 300)
    cat("Statistical analysis results plot saved to:", filename, "\n")
    
    return(list(results = stat_results, plot = p))
  }
  
  return(NULL)
}

# Run statistical analysis for each k value
stat_analysis <- list()
for (k in kproto_k_values) {
  cat("\nPerforming statistical analysis for k =", k, "\n")
  stat_analysis[[paste0("k", k)]] <- perform_statistical_analysis(k)
}

# 9. Create Summary Report -------------------------------------------------
cat("\nGenerating analysis summary report...\n")

# Create a summary of the clustering results
summary_data <- data.frame(
  K_Value = integer(),
  Cluster = integer(),
  Size = integer(),
  PDM_Distribution = character(),
  Key_Features = character(),
  stringsAsFactors = FALSE
)

# Function to generate cluster summaries
generate_cluster_summary <- function(k) {
  # Get the results for this k
  cluster_results <- kproto_results[[paste0("k", k)]]$results
  centroids <- kproto_results[[paste0("k", k)]]$centroids
  
  # Summarize each cluster
  for (cluster_num in 1:k) {
    # Get cluster size
    cluster_size <- sum(cluster_results$Cluster == cluster_num)
    
    # Get PDM distribution
    pdm_dist <- table(cluster_results$PDM_Selected[cluster_results$Cluster == cluster_num])
    pdm_pct <- round(prop.table(pdm_dist) * 100, 1)
    top_pdms <- names(sort(pdm_dist, decreasing = TRUE))[1:min(2, length(pdm_dist))]
    pdm_summary <- paste(
      sapply(top_pdms, function(pdm) {
        paste0(pdm, " (", pdm_pct[pdm], "%)")
      }),
      collapse = ", "
    )
    
    # Identify key features (variables with extreme values in centroids)
    cluster_centroid <- centroids[centroids$Cluster == cluster_num, ]
    
    # Find variables where this cluster has the highest or lowest value
    key_features <- c()
    for (var in setdiff(colnames(centroids), "Cluster")) {
      var_values <- centroids[, var]
      max_val <- max(var_values)
      min_val <- min(var_values)
      
      if (cluster_centroid[, var] == max_val && max_val > 3.5) {
        key_features <- c(key_features, paste0("High ", var))
      } else if (cluster_centroid[, var] == min_val && min_val < 2.5) {
        key_features <- c(key_features, paste0("Low ", var))
      }
    }
    
    # Limit to top 5 features
    if (length(key_features) > 5) {
      key_features <- key_features[1:5]
    }
    
    # Add to summary data
    summary_data <- rbind(summary_data, data.frame(
      K_Value = k,
      Cluster = cluster_num,
      Size = cluster_size,
      PDM_Distribution = pdm_summary,
      Key_Features = paste(key_features, collapse = ", "),
      stringsAsFactors = FALSE
    ))
  }
  
  return(summary_data)
}

# Generate summaries for each k value
for (k in kproto_k_values) {
  summary_data <- generate_cluster_summary(k)
}

# Save summary data
save_data(summary_data, "032_kprototype_analysis/cluster_summary.csv")
cat("Cluster summary report saved successfully\n")

# Print final message
cat("\nK-prototype clustering analysis completed!\n")
cat("Visualization results saved to: results/figures/032_kprototype_analysis/\n")
cat("Data results saved to: results/tables/032_kprototype_analysis/\n") 
cat("\nAdditional data generated for radar charts:\n")
cat("  - Cluster medians data for k=", paste(kproto_k_values, collapse=", "), "\n") 