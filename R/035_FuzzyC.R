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
  "ORG_Size_Employees",
  "ORG_Complexity_Locations",
  "ORG_Complexity_Departments",
  "ORG_Hierarchy_Layers"
)

# Decision variables
dec_vars <- c(
  "DEC_Authority_Dispersion",
  "DEC_Authority_Delegation",
  "DEC_Process_InformalCommunication",
  "DEC_Process_InformalProcedures"
)

# Style variables
sty_vars <- c(
  "STY_Analytical_DataDriven",
  "STY_Participative_Inclusion",
  "STY_Participative_Relational",
  "STY_Organic_InformalStructure",
  "STY_Organic_Adaptability",
  "STY_Directive_Threats",
  "STY_Directive_Compliance"
)

# Culture variables
cul_vars <- c(
  "CUL_Authority_Hierarchical",
  "CUL_Integration_Vision",
  "CUL_Integration_Systematic",
  "CUL_Innovation_Experimental",
  "CUL_Collaboration_Stakeholder"
)

# Flexibility variables
flex_vars <- c(
  "FLEX_Cognitive_Receptivity",
  "FLEX_Behavioral_Adaptability"
)

# Risk and environment variables
risk_env_vars <- c(
  "RISK_Appetite_Investment",
  "ENV_Context_Growth",
  "ENV_Context_Volatility",
  "ENV_Context_Stability"
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
    "DEC_Authority_Dispersion" = "DEC\nAuthority\nDispersion",
    "DEC_Authority_Delegation" = "DEC\nAuthority\nDelegation",
    "DEC_Process_InformalCommunication" = "DEC\nProcess\nInformalComm",
    "DEC_Process_InformalProcedures" = "DEC\nProcess\nInformalProc",
    
    # Style variables
    "STY_Analytical_DataDriven" = "STY\nAnalytical\nDataDriven",
    "STY_Participative_Inclusion" = "STY\nParticipative\nInclusion",
    "STY_Participative_Relational" = "STY\nParticipative\nRelational",
    "STY_Organic_InformalStructure" = "STY\nOrganic\nInformalStructure",
    "STY_Organic_Adaptability" = "STY\nOrganic\nAdaptability",
    "STY_Directive_Threats" = "STY\nDirective\nThreats",
    "STY_Directive_Compliance" = "STY\nDirective\nCompliance",
    
    # Culture variables
    "CUL_Authority_Hierarchical" = "CUL\nAuthority\nHierarchical",
    "CUL_Integration_Vision" = "CUL\nIntegration\nVision",
    "CUL_Integration_Systematic" = "CUL\nIntegration\nSystematic",
    "CUL_Innovation_Experimental" = "CUL\nInnovation\nExperimental",
    "CUL_Collaboration_Stakeholder" = "CUL\nCollaboration\nStakeholder",
    
    # Flexibility variables
    "FLEX_Cognitive_Receptivity" = "FLEX\nCognitive\nReceptivity",
    "FLEX_Behavioral_Adaptability" = "FLEX\nBehavioral\nAdaptability",
    
    # Risk and environment variables
    "RISK_Appetite_Investment" = "RISK\nAppetite\nInvestment",
    "ENV_Context_Growth" = "ENV\nContext\nGrowth",
    "ENV_Context_Volatility" = "ENV\nContext\nVolatility",
    "ENV_Context_Stability" = "ENV\nContext\nStability",
    
    # Organization structure
    "ORG_Size_Employees" = "ORG\nSize\nEmployees",
    "ORG_Complexity_Locations" = "ORG\nComplexity\nLocations",
    "ORG_Complexity_Departments" = "ORG\nComplexity\nDepartments",
    "ORG_Hierarchy_Layers" = "ORG\nHierarchy\nLayers"
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
  
  # 确保数据是数据框，并且将行名设置好
  if(!is.data.frame(data)) {
    data <- as.data.frame(data)
  }
  
  # 检查并确保所有变量存在
  missing_vars <- variables[!variables %in% colnames(data)]
  if(length(missing_vars) > 0) {
    cat("警告: 变量不存在:", paste(missing_vars, collapse=", "), "\n")
    variables <- variables[variables %in% colnames(data)]
    if(length(variables) == 0) {
      cat("错误: 没有可用的变量来创建雷达图\n")
      return(NULL)
    }
  }
  
  # 数据准备 - 确保是正确的数据框格式
  radar_data <- data.frame(data[, variables, drop=FALSE])
  
  # 将数据框转换为矩阵以便于操作
  radar_matrix <- as.matrix(radar_data)
  
  # 添加最大值和最小值行
  max_min_matrix <- rbind(
    rep(scale_max, length(variables)),  # 最大值
    rep(scale_min, length(variables)),  # 最小值
    radar_matrix
  )
  
  # 转换回数据框并设置行名列名
  radar_data_final <- as.data.frame(max_min_matrix)
  rownames(radar_data_final) <- c("max", "min", rownames(data))
  colnames(radar_data_final) <- variables
  
  # 格式化变量名
  formatted_names <- format_variable_names(variables)
  colnames(radar_data_final) <- formatted_names
  
  # 定义集群颜色
  cluster_colors <- c("#4285F4", "#EA4335")  # 集群1和2的Google颜色
  
  # 创建并保存雷达图
  filename <- paste0("results/figures/035_fuzzy_cmeans/radar_", 
                    gsub(" ", "_", tolower(group_name)), ".pdf")
  
  # 确保图形设备被正确打开和关闭
  pdf(filename, width = width, height = height)
  
  # 设置绘图参数
  par(mar = c(2, 2, 3, 2))
  
  # 绘制雷达图
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
    
    # 添加图例
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
    cat("绘制雷达图时出错:", e$message, "\n")
  })
  
  # 确保设备关闭
  dev.off()
  
  # 也保存为PNG以便于查看
  png_filename <- paste0("results/figures/035_fuzzy_cmeans/radar_", 
                       gsub(" ", "_", tolower(group_name)), ".png")
  
  # 打开PNG设备
  png(png_filename, width = width * 100, height = height * 100)
  
  # 设置绘图参数
  par(mar = c(2, 2, 3, 2))
  
  # 绘制雷达图到PNG
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
    
    # 添加图例
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
    cat("绘制PNG雷达图时出错:", e$message, "\n")
  })
  
  # 确保PNG设备关闭
  dev.off()
  
  cat(paste0("  创建了", group_name, "的雷达图，保存到", filename, "和", png_filename, "\n"))
  
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
  
  # Define short PDM names for legend
  pdm_short_names <- c(
    "Design-Bid-Build" = "DBB",
    "Construction Manager @ Risk" = "CMAR",
    "Design-Build" = "DB",
    "Progressive Design-Build" = "PDB",
    "Integrated Project Delivery (IPD)" = "IPD"
  )
  
  # Create PDF file
  pdf_file <- "results/figures/035_fuzzy_cmeans/pdm_distribution.pdf"
  pdf(pdf_file, width = 12, height = 8)
  
  # Create stacked bar chart
  plot <- ggplot(pdm_df, aes(x = Cluster, y = Percentage, fill = PDM)) +
    geom_bar(stat = "identity", position = "stack") +
    scale_fill_manual(values = pdm_colors, labels = pdm_short_names) +
    labs(
      title = "Project Delivery Method Distribution by Cluster",
      x = "Cluster",
      y = "Percentage (%)",
      fill = "Project Delivery Method"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
      axis.title = element_text(face = "bold"),
      legend.position = "right",
      legend.title = element_text(face = "bold")
    )
  
  # Print the plot to PDF
  print(plot)
  dev.off()
  
  # Create PNG version
  png_file <- "results/figures/035_fuzzy_cmeans/pdm_distribution.png"
  png(png_file, width = 1200, height = 800)
  print(plot)
  dev.off()
  
  cat(paste0("  PDM distribution chart saved to ", pdf_file, " and ", png_file, "\n"))
  
  return(plot)
}

# Generate PDM distribution chart
pdm_distribution <- create_pdm_distribution_chart()

# 10. Print cluster summary ---------------------------------------------------
cat("\nCluster Summary:\n")
for (i in 1:k) {
  cat(paste0("Cluster ", i, ": ", cluster_sizes[i], " observations\n"))
  
  # Display top PDMs for this cluster
  cluster_pdms <- sort(table(data$PDM_Selected[data$Cluster == i]), decreasing = TRUE)
  cluster_pdm_pct <- round(prop.table(cluster_pdms) * 100, 1)
  
  cat("  Top PDMs:\n")
  for (j in 1:min(length(cluster_pdms), 3)) {
    cat(paste0("    ", names(cluster_pdms)[j], ": ", 
              cluster_pdms[j], " (", cluster_pdm_pct[j], "%)\n"))
  }
  
  # Show key features (variables with extreme values)
  cat("  Key features:\n")
  
  # For organization variables (high/low compared to other cluster)
  for (var in org_vars) {
    val <- cluster_centers[var, i]
    other_val <- cluster_centers[var, (i %% k) + 1]  # Other cluster
    
    if (val > other_val * 1.5) {
      cat(paste0("    High ", var, ": ", round(val, 2), "\n"))
    } else if (val < other_val * 0.7) {
      cat(paste0("    Low ", var, ": ", round(val, 2), "\n"))
    }
  }
  
  # For Likert scale variables (high/low on the scale)
  for (var in likert_vars) {
    val <- cluster_centers[var, i]
    
    if (val >= 4.0) {
      cat(paste0("    High ", var, ": ", round(val, 2), "\n"))
    } else if (val <= 2.0) {
      cat(paste0("    Low ", var, ": ", round(val, 2), "\n"))
    }
  }
  
  cat("\n")
}

cat("Fuzzy C-means clustering and visualization complete!\n")
cat("Analysis results saved to results/tables/035_fuzzy_cmeans and results/figures/035_fuzzy_cmeans directories\n")
