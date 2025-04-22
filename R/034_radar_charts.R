# BOAT2 Cluster and Factor Analysis - Radar Chart Visualization
# This script creates radar chart visualizations for the k-prototype clustering results

# 0. Check and Install Required Packages -------------------------------------
if (!requireNamespace("fmsb", quietly = TRUE)) {
  cat("Installing fmsb package...\n")
  install.packages("fmsb")
}
library(fmsb)
library(RColorBrewer) # For color palettes

# 添加测试函数在主函数之前
test_csv_reading <- function() {
  cat("\n---------- 开始测试CSV文件读取 ----------\n")
  
  # 尝试读取中位数文件
  file_path <- "results/tables/032_kprototype_analysis/kproto_medians_k2.csv"
  
  if (file.exists(file_path)) {
    cat("文件存在:", file_path, "\n")
    
    # 直接读取文件的前几行
    file_lines <- readLines(file_path, n = 5)
    cat("文件内容预览:\n")
    for (line in file_lines) {
      cat("  ", line, "\n") 
    }
    
    # 尝试不同的CSV读取参数
    cat("\n尝试方法1 - 标准read.csv:\n")
    df1 <- tryCatch({
      result <- read.csv(file_path, stringsAsFactors = FALSE)
      print(str(result))
      print(colnames(result))
      result
    }, error = function(e) {
      cat("错误:", e$message, "\n")
      NULL
    })
    
    cat("\n尝试方法2 - 带引号处理的read.csv:\n")
    df2 <- tryCatch({
      result <- read.csv(file_path, stringsAsFactors = FALSE, quote = "\"")
      print(str(result))
      print(colnames(result))
      result
    }, error = function(e) {
      cat("错误:", e$message, "\n")
      NULL
    })
    
    cat("\n尝试方法3 - 使用read.table:\n")
    df3 <- tryCatch({
      result <- read.table(file_path, header = TRUE, sep = ",", quote = "\"", stringsAsFactors = FALSE)
      print(str(result))
      print(colnames(result))
      result
    }, error = function(e) {
      cat("错误:", e$message, "\n")
      NULL
    })
    
    cat("\n尝试方法4 - 使用readr包:\n")
    # 检查是否已加载readr
    if(requireNamespace("readr", quietly = TRUE)) {
      df4 <- tryCatch({
        result <- readr::read_csv(file_path)
        print(str(result))
        print(colnames(result))
        result
      }, error = function(e) {
        cat("错误:", e$message, "\n")
        NULL
      })
    } else {
      cat("readr包未加载，跳过方法4\n")
    }
    
    # 返回成功的数据框
    successful_df <- NULL
    if (!is.null(df1) && is.data.frame(df1)) successful_df <- df1
    else if (!is.null(df2) && is.data.frame(df2)) successful_df <- df2
    else if (!is.null(df3) && is.data.frame(df3)) successful_df <- df3
    else if (exists("df4") && !is.null(df4)) successful_df <- as.data.frame(df4)
    
    if (!is.null(successful_df)) {
      cat("\n成功读取的数据框示例 (前2行):\n")
      print(head(successful_df, 2))
      return(successful_df)
    } else {
      cat("\n所有读取方法均失败\n")
      return(NULL)
    }
  } else {
    cat("文件不存在:", file_path, "\n")
    return(NULL)
  }
}

# 1. Load Setup and Data -----------------------------------------------------
source("R/000_setup.R")

# Create subdirectories for results if they don't exist
dirs <- c(
  "results/figures/034_radar_charts",
  "results/tables/034_radar_charts"
)

for (dir in dirs) {
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE)
    cat(paste("Created directory:", dir, "\n"))
  }
}

# Define k values to analyze (same as in the k-prototype analysis)
kproto_k_values <- c(2, 3)
cat("Will generate radar charts for k values:", paste(kproto_k_values, collapse = ", "), "\n")

# 执行CSV测试读取
test_result <- test_csv_reading()
if (!is.null(test_result)) {
  cat("CSV测试读取成功，继续执行脚本\n")
} else {
  cat("CSV测试读取失败，脚本可能无法正常工作\n")
}

# 2. Load Clustering Results -------------------------------------------------
cat("\nLoading k-prototype clustering results...\n")

# Function to load centroids data for a specific k value
load_centroids <- function(k) {
  file_path <- paste0("results/tables/032_kprototype_analysis/kproto_centroids_k", k, ".csv")
  if (file.exists(file_path)) {
    # 添加更多的调试信息
    cat("  Reading centroids file:", file_path, "\n")
    
    # 直接读取并打印文件的前几行以进行调试
    file_lines <- readLines(file_path, n = 5)
    cat("  File content preview:\n")
    for (line in file_lines) {
      cat("    ", line, "\n") 
    }
    
    # 确保read.csv使用标准参数读取
    centroids <- read.csv(file_path, stringsAsFactors = FALSE, check.names = FALSE)
    
    # 打印读取后的数据结构
    cat("  Loaded data structure:\n")
    print(str(centroids))
    cat("  Column names:\n")
    print(colnames(centroids))
    
    # 确保Cluster列是数值型
    centroids$Cluster <- as.numeric(centroids$Cluster)
    cat("  Loaded centroids for k =", k, "\n")
    return(centroids)
  } else {
    cat("  Error: Could not find centroids file for k =", k, "\n")
    return(NULL)
  }
}

# Function to load medians data for a specific k value
load_medians <- function(k) {
  file_path <- paste0("results/tables/032_kprototype_analysis/kproto_medians_k", k, ".csv")
  if (file.exists(file_path)) {
    # 添加更多的调试信息
    cat("  Reading medians file:", file_path, "\n")
    
    # 直接读取并打印文件的前几行以进行调试
    file_lines <- readLines(file_path, n = 5)
    cat("  File content preview:\n")
    for (line in file_lines) {
      cat("    ", line, "\n") 
    }
    
    # 确保read.csv使用标准参数读取
    medians <- read.csv(file_path, stringsAsFactors = FALSE, check.names = FALSE)
    
    # 打印读取后的数据结构
    cat("  Loaded data structure:\n")
    print(str(medians))
    cat("  Column names:\n")
    print(colnames(medians))
    
    # 确保Cluster列是数值型
    medians$Cluster <- as.numeric(medians$Cluster)
    cat("  Loaded medians for k =", k, "\n")
    return(medians)
  } else {
    cat("  Error: Could not find medians file for k =", k, "\n")
    return(NULL)
  }
}

# Function to load full cluster data (for box plots)
load_cluster_data <- function(k) {
  file_path <- paste0("results/tables/032_kprototype_analysis/kproto_clusters_k", k, ".csv")
  if (file.exists(file_path)) {
    clusters <- read.csv(file_path)
    cat("  Loaded cluster data for k =", k, "\n")
    return(clusters)
  } else {
    cat("  Error: Could not find cluster data file for k =", k, "\n")
    return(NULL)
  }
}

# 新增：为Box Plot创建专用函数
create_box_plots <- function(k, cluster_data) {
  cat("\nCreating box plots for k =", k, "...\n")
  
  # 确认cluster_data是否有效
  if (is.null(cluster_data) || !is.data.frame(cluster_data)) {
    cat("  Error: Invalid or NULL cluster data\n")
    return(NULL)
  }
  
  # 创建Box Plot专用文件夹
  box_dir <- paste0("results/figures/034_radar_charts/k", k, "_box_plot")
  if (!dir.exists(box_dir)) {
    dir.create(box_dir, recursive = TRUE)
    cat("  Created box plot directory:", box_dir, "\n")
  }
  
  # 只获取ORG_开头的组织结构变量
  org_vars <- grep("^ORG_", names(cluster_data), value = TRUE)
  cat("  Only generating box plots for organization structure variables:", paste(org_vars, collapse=", "), "\n")
  
  if (length(org_vars) == 0) {
    cat("  Error: No organization structure variables (ORG_) found in the dataset\n")
    return(NULL)
  }
  
  # 处理离散的组织规模变量
  # 为了更好地可视化，将大型组织规模压缩到合理范围
  if ("ORG_Size_Employees" %in% org_vars) {
    # 创建组织规模的对数转换版本，便于box plot显示
    cat("  Creating log-transformed version of ORG_Size_Employees for better visualization\n")
    cluster_data$ORG_Size_Employees_Log <- log1p(cluster_data$ORG_Size_Employees)
    # 将此变量添加到org_vars
    org_vars <- c(org_vars, "ORG_Size_Employees_Log")
  }
  
  # 对每个变量创建单独的box plot
  for (var in org_vars) {
    # 跳过与原始ORG_Size_Employees重复的对数变量
    if (var == "ORG_Size_Employees" && "ORG_Size_Employees_Log" %in% org_vars) {
      next  # 跳过，只使用对数变换版本
    }
    
    var_display <- gsub("_Log$", " (Log Scale)", var)  # 为对数变换变量显示更好的标签
    var_display <- gsub("_", " ", var_display)  # 将下划线替换为空格，使标签更友好
    
    # 准备数据
    plot_data <- cluster_data[, c("Cluster", var)]
    
    # 处理离群值
    upper_limit <- NULL
    if (var != "ORG_Size_Employees_Log" && var %in% c("ORG_Size_Employees", "ORG_Complexity_Locations", "ORG_Complexity_Departments")) {
      # 对大型组织变量应用上限
      q3 <- quantile(plot_data[[var]], 0.75, na.rm = TRUE)
      iqr <- IQR(plot_data[[var]], na.rm = TRUE)
      upper_limit <- q3 + 1.5 * iqr
      
      cat("    Variable", var, "has outliers. Using capped values for visualization.\n")
      cat("    Original range:", min(plot_data[[var]], na.rm = TRUE), "to", max(plot_data[[var]], na.rm = TRUE), "\n")
      cat("    Upper limit for visualization:", upper_limit, "\n")
      
      # 为可视化创建截断版本
      plot_data$capped_value <- pmin(plot_data[[var]], upper_limit)
      var_to_plot <- "capped_value"
      var_display <- paste0(var_display, " (Capped)")
    } else {
      var_to_plot <- var
    }
    
    # 创建Box Plot
    pdf_file <- paste0(box_dir, "/", gsub(" ", "_", tolower(var)), "_boxplot.pdf")
    pdf(pdf_file, width = 10, height = 8)
    
    # 设置边距
    par(mar = c(5, 6, 4, 2) + 0.1)
    
    # 绘制box plot
    boxplot(
      reformulate("Cluster", var_to_plot), 
      data = plot_data,
      main = paste0(var_display, " by Cluster"),
      xlab = "Cluster",
      ylab = var_display,
      col = brewer.pal(n = min(max(k, 3), 9), name = "Set1"),
      cex.axis = 1.2,
      cex.lab = 1.3,
      cex.main = 1.4,
      outline = TRUE,
      axes = (var != "ORG_Size_Employees_Log") # 对数变量暂时不显示默认轴
    )
    
    # 对对数轴添加更有意义的标签
    if (var == "ORG_Size_Employees_Log") {
      # 获取当前的轴范围
      log_range <- range(plot_data[[var]], na.rm = TRUE)
      # 创建适当的刻度
      log_breaks <- seq(from = floor(log_range[1]), to = ceiling(log_range[2]), by = 1)
      # 原始值（指数变换回去）
      orig_values <- round(expm1(log_breaks))
      # 自定义标签
      axis(2, at = log_breaks, labels = paste0(round(log_breaks, 1), "\n(", orig_values, ")"), las = 1, cex.axis = 1.1)
      # 添加网格线
      abline(h = log_breaks, col = "lightgray", lty = 3)
    }
    
    # 如果有上限值，添加上限标注
    if (!is.null(upper_limit)) {
      mtext(paste("Values capped at", round(upper_limit, 1)), side = 3, line = 0.5, cex = 0.9, col = "red")
    }
    
    # 添加中位数值标签
    med_vals <- tapply(plot_data[[var]], plot_data$Cluster, median, na.rm = TRUE)
    if (var == "ORG_Size_Employees_Log") {
      # 对对数值，同时显示转换前后的值
      med_pos <- med_vals + 0.15 * diff(range(plot_data[[var_to_plot]], na.rm = TRUE))
      text_labels <- sprintf("%.2f\n(%.0f)", med_vals, expm1(med_vals))
      text(1:k, med_pos, text_labels, cex = 1.1)
    } else {
      # 对普通值，只显示原值
      text(1:k, med_vals + 0.15 * diff(range(plot_data[[var_to_plot]], na.rm = TRUE)), 
           sprintf("%.2f", med_vals), cex = 1.1)
    }
    
    # 关闭设备
    dev.off()
    cat("    Box plot saved to:", pdf_file, "\n")
  }
  
  # 创建组合box plot
  combined_pdf <- paste0(box_dir, "/organization_structure_combined_boxplot.pdf")
  
  # 计算需要的行数
  n_vars <- length(org_vars)
  if ("ORG_Size_Employees" %in% org_vars && "ORG_Size_Employees_Log" %in% org_vars) {
    n_vars <- n_vars - 1  # 减去一个，因为我们不会使用原始的ORG_Size_Employees
  }
  
  n_rows <- ceiling(n_vars / 2)  # 每行2个图
  
  # 创建组合图
  pdf(combined_pdf, width = 16, height = 6 * n_rows)
  par(mfrow = c(n_rows, 2), mar = c(5, 5, 4, 2) + 0.1)
  
  for (var in org_vars) {
    # 跳过与原始ORG_Size_Employees重复的对数变量
    if (var == "ORG_Size_Employees" && "ORG_Size_Employees_Log" %in% org_vars) {
      next  # 跳过，只使用对数变换版本
    }
    
    var_display <- gsub("_Log$", " (Log Scale)", var)
    var_display <- gsub("_", " ", var_display)  # 将下划线替换为空格，使标签更友好
    
    # 准备数据
    plot_data <- cluster_data[, c("Cluster", var)]
    
    # 处理离群值
    upper_limit <- NULL
    if (var != "ORG_Size_Employees_Log" && var %in% c("ORG_Size_Employees", "ORG_Complexity_Locations", "ORG_Complexity_Departments")) {
      # 对大型组织变量应用上限
      q3 <- quantile(plot_data[[var]], 0.75, na.rm = TRUE)
      iqr <- IQR(plot_data[[var]], na.rm = TRUE)
      upper_limit <- q3 + 1.5 * iqr
      
      # 为可视化创建截断版本
      plot_data$capped_value <- pmin(plot_data[[var]], upper_limit)
      var_to_plot <- "capped_value"
      var_display <- paste0(var_display, " (Capped)")
    } else {
      var_to_plot <- var
    }
    
    # 绘制box plot
    boxplot(
      reformulate("Cluster", var_to_plot), 
      data = plot_data,
      main = var_display,
      xlab = "Cluster",
      ylab = var_display,
      col = brewer.pal(n = min(max(k, 3), 9), name = "Set1"),
      cex.axis = 1.2,
      cex.lab = 1.3,
      cex.main = 1.4,
      outline = TRUE,
      axes = (var != "ORG_Size_Employees_Log") # 对数变量暂时不显示默认轴
    )
    
    # 对对数轴添加更有意义的标签
    if (var == "ORG_Size_Employees_Log") {
      # 获取当前的轴范围
      log_range <- range(plot_data[[var]], na.rm = TRUE)
      # 创建适当的刻度
      log_breaks <- seq(from = floor(log_range[1]), to = ceiling(log_range[2]), by = 1)
      # 原始值（指数变换回去）
      orig_values <- round(expm1(log_breaks))
      # 自定义标签
      axis(2, at = log_breaks, labels = paste0(round(log_breaks, 1), "\n(", orig_values, ")"), las = 1, cex.axis = 1.1)
      # 添加网格线
      abline(h = log_breaks, col = "lightgray", lty = 3)
    }
    
    # 如果有上限值，添加上限标注
    if (!is.null(upper_limit)) {
      mtext(paste("Values capped at", round(upper_limit, 1)), side = 3, line = 0.5, cex = 0.9, col = "red")
    }
    
    # 添加中位数值标签
    med_vals <- tapply(plot_data[[var]], plot_data$Cluster, median, na.rm = TRUE)
    if (var == "ORG_Size_Employees_Log") {
      # 对对数值，同时显示转换前后的值
      med_pos <- med_vals + 0.1 * diff(range(plot_data[[var_to_plot]], na.rm = TRUE))
      text_labels <- sprintf("%.1f\n(%.0f)", med_vals, expm1(med_vals))
      text(1:k, med_pos, text_labels, cex = 1.1)
    } else {
      # 对普通值，只显示原值
      text(1:k, med_vals + 0.1 * diff(range(plot_data[[var_to_plot]], na.rm = TRUE)), 
           sprintf("%.2f", med_vals), cex = 1.1)
    }
  }
  
  dev.off()
  cat("  Combined organization structure box plot saved to:", combined_pdf, "\n")
  
  cat("  Box plots creation completed for k =", k, "\n")
}

# Load data for each k value
centroids_list <- list()
medians_list <- list()  # 新增中位数列表
cluster_data_list <- list()

for (k in kproto_k_values) {
  centroids_list[[paste0("k", k)]] <- load_centroids(k)
  medians_list[[paste0("k", k)]] <- load_medians(k)  # 加载中位数数据
  cluster_data_list[[paste0("k", k)]] <- load_cluster_data(k)
}

# 3. Define Variable Groups for Radar Charts ---------------------------------
cat("\nOrganizing variables for radar charts...\n")

# Define numeric organizational structure variables (separate visualization)
org_structure_vars <- c(
  "ORG_Size_Employees",
  "ORG_Complexity_Locations",
  "ORG_Complexity_Departments",
  "ORG_Hierarchy_Layers"
)

# Define Group 1: Decision and Style variables (Likert scale)
group1_vars <- c(
  # Decision distribution variables
  "DEC_Authority_Dispersion",
  "DEC_Authority_Delegation",
  "DEC_Process_InformalCommunication",
  "DEC_Process_InformalProcedures",
  
  # Decision style variables
  "STY_Analytical_DataDriven",
  "STY_Participative_Inclusion",
  "STY_Participative_Relational",
  "STY_Organic_InformalStructure",
  "STY_Organic_Adaptability", 
  "STY_Directive_Threats",
  "STY_Directive_Compliance"
)

# Define Group 2: Culture, Flexibility, Risk and Environment variables (Likert scale)
group2_vars <- c(
  # Organization culture variables
  "CUL_Authority_Hierarchical",
  "CUL_Integration_Vision",
  "CUL_Integration_Systematic",
  "CUL_Innovation_Experimental",
  "CUL_Collaboration_Stakeholder",
  
  # Decision flexibility variables
  "FLEX_Cognitive_Receptivity",
  "FLEX_Behavioral_Adaptability",
  
  # Risk and environment variables
  "RISK_Appetite_Investment",
  "ENV_Context_Growth",
  "ENV_Context_Volatility",
  "ENV_Context_Stability"
)

# Create a list of variable groups (new organization)
variable_groups <- list(
  "Organization Structure (Numeric)" = org_structure_vars,
  "Group 1 (Decision & Style)" = group1_vars,
  "Group 2 (Culture, Flexibility & Environment)" = group2_vars
)

# Also create a list of all Likert scale variables for combined charts
likert_vars <- c(group1_vars, group2_vars)

# 4. Create Improved Radar Chart Function with Variable Name Formatting -----------------

# Function to format variable names with line breaks
format_variable_names <- function(var_names) {
  # Replace underscore with newline character
  formatted_names <- var_names
  
  # Create mapping for specific variable pattern replacements
  name_mappings <- list(
    "STY_Analytical_DataDriven" = "STY\nAnalytical\nDataDriven",
    "STY_Participative_Inclusion" = "STY\nParticipative\nInclusion",
    "STY_Participative_Relational" = "STY\nParticipative\nRelational",
    "STY_Organic_InformalStructure" = "STY\nOrganic\nInformalStructure",
    "STY_Organic_Adaptability" = "STY\nOrganic\nAdaptability",
    "STY_Directive_Threats" = "STY\nDirective\nThreats",
    "STY_Directive_Compliance" = "STY\nDirective\nCompliance",
    
    "DEC_Authority_Dispersion" = "DEC\nAuthority\nDispersion",
    "DEC_Authority_Delegation" = "DEC\nAuthority\nDelegation",
    "DEC_Process_InformalCommunication" = "DEC\nProcess\nInformalComm",
    "DEC_Process_InformalProcedures" = "DEC\nProcess\nInformalProc",
    
    "CUL_Authority_Hierarchical" = "CUL\nAuthority\nHierarchical",
    "CUL_Integration_Vision" = "CUL\nIntegration\nVision",
    "CUL_Integration_Systematic" = "CUL\nIntegration\nSystematic",
    "CUL_Innovation_Experimental" = "CUL\nInnovation\nExperimental",
    "CUL_Collaboration_Stakeholder" = "CUL\nCollaboration\nStakeholder",
    
    "FLEX_Cognitive_Receptivity" = "FLEX\nCognitive\nReceptivity",
    "FLEX_Behavioral_Adaptability" = "FLEX\nBehavioral\nAdaptability",
    
    "RISK_Appetite_Investment" = "RISK\nAppetite\nInvestment",
    
    "ENV_Context_Growth" = "ENV\nContext\nGrowth",
    "ENV_Context_Volatility" = "ENV\nContext\nVolatility",
    "ENV_Context_Stability" = "ENV\nContext\nStability",
    
    "ORG_Size_Employees" = "ORG\nSize\nEmployees",
    "ORG_Complexity_Locations" = "ORG\nComplexity\nLocations",
    "ORG_Complexity_Departments" = "ORG\nComplexity\nDepartments",
    "ORG_Hierarchy_Layers" = "ORG\nHierarchy\nLayers"
  )
  
  # Apply mappings for original variable names
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

# Function to create a radar chart for a given dataset and variables
create_radar_chart <- function(data, variables, 
                               title = "Radar Chart", 
                               scale_min = 1, 
                               scale_max = 5,
                               width = 14,  # Increased width for better variable name display
                               height = 10) {
  
  # Simplify: Keep only variables that exist in the data
  vars_to_use <- variables[variables %in% colnames(data)]
  original_vars <- vars_to_use  # Keep original names for reference
  
  if (length(vars_to_use) == 0) {
    stop("None of the specified variables exist in the dataset")
  }
  
  # Create data frame for radar chart
  n_clusters <- nrow(data)
  radar_data <- data.frame(matrix(NA, nrow = n_clusters, ncol = length(vars_to_use)))
  colnames(radar_data) <- vars_to_use
  rownames(radar_data) <- paste("Cluster", data$Cluster)
  
  # Fill in the data
  for (i in 1:nrow(data)) {
    for (j in 1:length(vars_to_use)) {
      var <- vars_to_use[j]
      radar_data[i, j] <- data[i, var]
    }
  }
  
  # Add max and min rows
  radar_data <- rbind(
    rep(scale_max, ncol(radar_data)),
    rep(scale_min, ncol(radar_data)),
    radar_data
  )
  rownames(radar_data)[1:2] <- c("max", "min")
  
  # Create formatted labels with line breaks
  display_labels <- format_variable_names(colnames(radar_data))
  colnames(radar_data) <- display_labels
  
  # Define colors for clusters
  if (n_clusters <= 2) {
    cluster_colors <- c("#4285F4", "#EA4335")
  } else {
    cluster_colors <- brewer.pal(n = min(max(n_clusters, 3), 9), name = "Set1")
  }
  
  # Create plotting function
  plot_fn <- function() {
    # Set up plot margins - increased for wider display
    par(mar = c(2, 2, 3, 2))
    
    # Plot the radar chart
    radarchart(
      radar_data,
      pfcol = adjustcolor(cluster_colors[1:n_clusters], alpha.f = 0.3),
      pcol = cluster_colors[1:n_clusters],
      plty = 1,
      plwd = 2.5,
      cglcol = "gray70",
      cglty = 1,
      axislabcol = "gray30",
      calcex = 1.2,  # 增大坐标轴标签字体大小，从0.8到1.2
      vlcex = 1.3,  # 增大变量标签字体大小，从0.9到1.3
      caxislabels = seq(scale_min, scale_max, (scale_max - scale_min) / 4),
      title = title,
      axistype = 1,  # Use axis labels
      titlecex = 1.4  # 增大标题字体大小
    )
    
    # NOTE: Removed value labels as requested
    
    # Add legend
    if (n_clusters > 1) {
      legend(
        "bottomright",
        legend = paste("Cluster", data$Cluster),
        fill = adjustcolor(cluster_colors[1:n_clusters], alpha.f = 0.3),
        col = cluster_colors[1:n_clusters],
        lty = 1,
        lwd = 2,
        cex = 1.2,  # 增大图例字体大小，从0.8到1.2
        box.lty = 0
      )
    }
  }
  
  # Return both the plotting function and the dimensions
  return(list(
    plot_fn = plot_fn,
    width = width,
    height = height
  ))
}

# 5. Generate Radar Charts and Box Plots for Each k Value --------------------
cat("\nGenerating charts...\n")

# Function to generate radar charts and box plots for a specific k value
generate_charts <- function(k, centroids, medians, cluster_data, use_medians = FALSE) {
  cat("\nCreating charts for k =", k, "...\n")
  
  # 确定使用哪个数据源及相应的目录后缀
  data_type <- ifelse(use_medians, "medians", "centroids")
  # Directly assign the data source based on the parameter
  data_source <- if (use_medians) medians else centroids
  dir_suffix <- ifelse(use_medians, "_median", "")
  
  # Check if data_source is valid
  if (is.null(data_source) || !is.data.frame(data_source)) {
    cat("  Error: Invalid or NULL data source for", data_type, "\n")
    return(NULL)
  }
  
  # 打印数据源的列名以进行调试
  cat("  Data source column names:\n")
  print(colnames(data_source))
  cat("  Data source structure:\n")
  print(str(data_source))
  cat("  Data source head:\n")
  print(head(data_source))
  
  # Create directory for k-specific charts
  k_dir <- paste0("results/figures/034_radar_charts/k", k, dir_suffix)
  if (!dir.exists(k_dir)) {
    dir.create(k_dir, recursive = TRUE)
  }
  
  # 1. Create overall radar chart with all Likert scale variables
  cat("  Creating charts for all Likert scale variables...\n")
  
  # Check if all variables exist in the dataset
  missing_vars <- likert_vars[!likert_vars %in% colnames(data_source)]
  if (length(missing_vars) > 0) {
    cat("  Warning: Missing variables:", paste(missing_vars, collapse = ", "), "\n")
    likert_vars_filtered <- likert_vars[likert_vars %in% colnames(data_source)]
    
    if (length(likert_vars_filtered) == 0) {
      cat("  Error: No Likert variables found in dataset. Skipping Likert radar charts.\n")
    } else {
      cat("  Using", length(likert_vars_filtered), "available Likert variables for chart.\n")
      
      # Create radar chart for all Likert variables
      likert_radar <- create_radar_chart(
        data_source,
        likert_vars_filtered,
        title = paste0("Likert Scale Variables (k=", k, ")", ifelse(use_medians, " - Medians", " - Means"))
      )
      
      # Save the Likert variables radar chart
      pdf(paste0(k_dir, "/likert_variables_radar.pdf"), width = likert_radar$width, height = likert_radar$height)
      likert_radar$plot_fn()
      dev.off()
      cat("  Likert scale variables radar chart saved to:", paste0(k_dir, "/likert_variables_radar.pdf"), "\n")
    }
  } else {
    # Create radar chart for all Likert variables
    likert_radar <- create_radar_chart(
      data_source,
      likert_vars,
      title = paste0("Likert Scale Variables (k=", k, ")", ifelse(use_medians, " - Medians", " - Means"))
    )
    
    # Save the Likert variables radar chart
    pdf(paste0(k_dir, "/likert_variables_radar.pdf"), width = likert_radar$width, height = likert_radar$height)
    likert_radar$plot_fn()
    dev.off()
    cat("  Likert scale variables radar chart saved to:", paste0(k_dir, "/likert_variables_radar.pdf"), "\n")
  }
  
  # 2. Create radar chart for Group 1 variables (Decision & Style)
  cat("  Creating charts for Group 1 (Decision & Style) variables...\n")
  group1_vars_filtered <- group1_vars[group1_vars %in% colnames(data_source)]
  if (length(group1_vars_filtered) > 0) {
    group1_radar <- create_radar_chart(
      data_source,
      group1_vars_filtered,
      title = paste0("Group 1: Decision & Style (k=", k, ")", ifelse(use_medians, " - Medians", " - Means"))
    )
    
    # Save the Group 1 variables radar chart
    pdf(paste0(k_dir, "/group1_radar.pdf"), width = group1_radar$width, height = group1_radar$height)
    group1_radar$plot_fn()
    dev.off()
    cat("  Group 1 variables radar chart saved to:", paste0(k_dir, "/group1_radar.pdf"), "\n")
  } else {
    cat("  Warning: No Group 1 variables found in dataset. Skipping Group 1 radar charts.\n")
  }
  
  # 3. Create radar chart for Group 2 variables (Culture, Flexibility & Environment)
  cat("  Creating charts for Group 2 (Culture, Flexibility & Environment) variables...\n")
  group2_vars_filtered <- group2_vars[group2_vars %in% colnames(data_source)]
  if (length(group2_vars_filtered) > 0) {
    group2_radar <- create_radar_chart(
      data_source,
      group2_vars_filtered,
      title = paste0("Group 2: Culture, Flexibility & Environment (k=", k, ")", ifelse(use_medians, " - Medians", " - Means"))
    )
    
    # Save the Group 2 variables radar chart
    pdf(paste0(k_dir, "/group2_radar.pdf"), width = group2_radar$width, height = group2_radar$height)
    group2_radar$plot_fn()
    dev.off()
    cat("  Group 2 variables radar chart saved to:", paste0(k_dir, "/group2_radar.pdf"), "\n")
  } else {
    cat("  Warning: No Group 2 variables found in dataset. Skipping Group 2 radar charts.\n")
  }
  
  # 4. Create individual radar charts for each cluster (using Likert variables only)
  for (cluster_num in 1:k) {
    cat("  Processing cluster", cluster_num, "...\n")
    
    # Filter data for this cluster - 使用更安全的子集方式
    if ("Cluster" %in% names(data_source)) {
      cluster_data_filtered <- subset(data_source, Cluster == cluster_num)
      cat("    Found", nrow(cluster_data_filtered), "rows for cluster", cluster_num, "\n")
    } else {
      cat("    Error: No 'Cluster' column found in data source\n")
      print(names(data_source))
      next
    }
    
    # 如果有匹配的Likert变量，则创建雷达图
    likert_vars_available <- likert_vars[likert_vars %in% colnames(data_source)]
    if (length(likert_vars_available) > 0) {
      # Create radar chart for this cluster (Likert variables only)
      cluster_radar <- create_radar_chart(
        cluster_data_filtered,
        likert_vars_available,
        title = paste0("Cluster ", cluster_num, " Profile (k=", k, ")",
                     ifelse(use_medians, " - Medians", " - Means"))
      )
      
      # Save the cluster radar chart
      pdf_file <- paste0(k_dir, "/cluster", cluster_num, "_radar.pdf")
      pdf(pdf_file, width = cluster_radar$width, height = cluster_radar$height)
      cluster_radar$plot_fn()
      dev.off()
      cat("  Cluster", cluster_num, "radar chart saved to:", pdf_file, "\n")
      
      # Create Group 1 radar chart for this cluster
      if (length(group1_vars_filtered) > 0) {
        cluster_group1_radar <- create_radar_chart(
          cluster_data_filtered,
          group1_vars_filtered,
          title = paste0("Cluster ", cluster_num, " - Group 1 (k=", k, ")",
                       ifelse(use_medians, " - Medians", " - Means"))
        )
        
        # Save the cluster Group 1 radar chart
        pdf_file <- paste0(k_dir, "/cluster", cluster_num, "_group1_radar.pdf")
        pdf(pdf_file, width = cluster_group1_radar$width, height = cluster_group1_radar$height)
        cluster_group1_radar$plot_fn()
        dev.off()
        cat("  Cluster", cluster_num, "Group 1 radar chart saved to:", pdf_file, "\n")
      }
      
      # Create Group 2 radar chart for this cluster
      if (length(group2_vars_filtered) > 0) {
        cluster_group2_radar <- create_radar_chart(
          cluster_data_filtered,
          group2_vars_filtered,
          title = paste0("Cluster ", cluster_num, " - Group 2 (k=", k, ")",
                       ifelse(use_medians, " - Medians", " - Means"))
        )
        
        # Save the cluster Group 2 radar chart
        pdf_file <- paste0(k_dir, "/cluster", cluster_num, "_group2_radar.pdf")
        pdf(pdf_file, width = cluster_group2_radar$width, height = cluster_group2_radar$height)
        cluster_group2_radar$plot_fn()
        dev.off()
        cat("  Cluster", cluster_num, "Group 2 radar chart saved to:", pdf_file, "\n")
      }
    } else {
      cat("  Warning: No Likert variables found for Cluster", cluster_num, ". Skipping radar chart.\n")
    }
  }
  
  # Return radars (if they exist)
  return(list(
    likert_radar = if(exists("likert_radar")) likert_radar else NULL,
    group1_radar = if(exists("group1_radar")) group1_radar else NULL,
    group2_radar = if(exists("group2_radar")) group2_radar else NULL
  ))
}

# Generate charts for each k value
chart_results <- list()
chart_results_median <- list()  # 存储中位数雷达图结果

for (k in kproto_k_values) {
  centroids_data <- centroids_list[[paste0("k", k)]]
  medians_data <- medians_list[[paste0("k", k)]]
  cluster_data <- cluster_data_list[[paste0("k", k)]]
  
  if (!is.null(centroids_data) && !is.null(cluster_data)) {
    # 生成雷达图
    cat("\n生成基于均值(质心)的雷达图 k =", k, "...\n")
    chart_results[[paste0("k", k)]] <- generate_charts(
      k = k,
      centroids = centroids_data,
      medians = medians_data,
      cluster_data = cluster_data,
      use_medians = FALSE
    )
    
    # 生成中位数雷达图
    if (!is.null(medians_data)) {
      cat("\n生成基于中位数的雷达图 k =", k, "...\n")
      chart_results_median[[paste0("k", k)]] <- generate_charts(
        k = k,
        centroids = centroids_data,
        medians = medians_data,
        cluster_data = cluster_data,
        use_medians = TRUE
      )
    }
    
    # 生成box plot
    cat("\n生成Box Plot可视化 k =", k, "...\n")
    create_box_plots(
      k = k,
      cluster_data = cluster_data
    )
  } else {
    cat("\n跳过 k =", k, "因为数据缺失\n")
  }
}

# 6. Create Comparative Visualizations ---------------------------------------
cat("\nCreating comparative visualizations...\n")

# 生成三个比较可视化：一个基于所有Likert变量，一个基于Group 1变量，一个基于Group 2变量
# 每组比较可视化有两个版本：一个基于均值，一个基于中位数
for (data_type in c("means", "medians")) {
  use_medians <- data_type == "medians"
  results_list <- if(use_medians) chart_results_median else chart_results
  
  # 跳过没有数据的类型
  if (length(results_list) == 0) {
    cat("  没有", data_type, "类型的数据可用于比较可视化\n")
    next
  }
  
  # 为所有Likert变量创建比较可视化
  create_comparative_visualization <- function(chart_type, title_suffix, width = 18, height = 14) {
    pdf_file <- paste0("results/figures/034_radar_charts/comparative_", chart_type, "_radar_", data_type, ".pdf")
    
    # Check if any of the results have the necessary radar chart
    has_charts <- FALSE
    for (k in kproto_k_values) {
      key <- paste0("k", k)
      if (key %in% names(results_list) && !is.null(results_list[[key]]) && !is.null(results_list[[key]][[chart_type]])) {
        has_charts <- TRUE
        break
      }
    }
    
    if (!has_charts) {
      cat("  No", chart_type, "charts available for", data_type, "visualization\n")
      return(invisible(NULL))
    }
    
    pdf(pdf_file, width = width, height = height)
    
    # Set up multi-panel layout based on number of charts
    n_panels <- length(kproto_k_values)
    if (n_panels <= 2) {
      par(mfrow = c(1, 2))  # 1x2 layout for 1-2 charts
    } else if (n_panels <= 4) {
      par(mfrow = c(2, 2))  # 2x2 layout for 3-4 charts
    } else {
      par(mfrow = c(2, 3))  # 2x3 layout for 5-6 charts
    }
    
    # Plot each k value radar chart
    for (k in kproto_k_values) {
      key <- paste0("k", k)
      if (key %in% names(results_list) && !is.null(results_list[[key]]) && !is.null(results_list[[key]][[chart_type]])) {
        results_list[[key]][[chart_type]]$plot_fn()
        title(main = paste0("k=", k, title_suffix), line = -1)
      }
    }
    
    # Reset layout
    par(mfrow = c(1, 1))
    
    # Close the PDF
    dev.off()
    cat("  Comparative", chart_type, "radar chart (", data_type, ") saved to:", pdf_file, "\n")
  }
  
  # 创建三种比较可视化
  create_comparative_visualization("likert_radar", " - All Variables")
  create_comparative_visualization("group1_radar", " - Decision & Style")
  create_comparative_visualization("group2_radar", " - Culture, Flex & Env")
}

# Print final message
cat("\nChart visualization completed!\n")
cat("Visualization results saved to: results/figures/034_radar_charts/\n")

# Print radar chart customization options
cat("\n雷达图可调整的参数包括：\n")
cat("1. 宽高比：通过调整 width 和 height 参数\n")
cat("2. 变量名格式：修改 format_variable_names 函数中的名称映射\n")
cat("3. 颜色方案：修改 cluster_colors 变量\n")
cat("4. 透明度：调整 adjustcolor 函数中的 alpha.f 参数\n")
cat("5. 线条粗细：调整 plwd 参数\n")
cat("6. 文字大小：调整 vlcex（变量标签）和 calcex（坐标轴标签）参数\n")
cat("7. 图表标题：通过 title 参数设置\n")
cat("8. 网格线样式：通过 cglcol 和 cglty 参数\n")
cat("9. 坐标刻度：通过 caxislabels 参数\n")
cat("10. 图例位置和样式：通过 legend 函数的参数\n")
cat("\n可以使用的数据类型：\n")
cat("- 均值 (质心)：results/figures/034_radar_charts/k2, k3/\n")
cat("- 中位数：results/figures/034_radar_charts/k2_median, k3_median/\n") 