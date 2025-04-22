# BOAT2 Cluster and Factor Analysis - Exploratory Data Analysis
# This script performs exploratory analysis and visualization on the prepared data

# 1. Load Setup and Data -----------------------------------------------------
source("R/000_setup.R")

# Create dedicated subdirectories for this script's outputs
exploration_figures_dir <- "results/figures/02_exploration"
exploration_tables_dir <- "results/tables/02_exploration"

# Create directories if they don't exist
dir.create(exploration_figures_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(exploration_tables_dir, recursive = TRUE, showWarnings = FALSE)

# Custom save functions for this script's outputs
save_exploration_plot <- function(plot, filename, width = 10, height = 8) {
  full_path <- file.path(exploration_figures_dir, filename)
  ggsave(
    filename = full_path,
    plot = plot,
    width = width,
    height = height,
    dpi = 300
  )
  cat(paste("Saved plot to:", full_path, "\n"))
}

save_exploration_data <- function(data, filename) {
  full_path <- file.path(exploration_tables_dir, filename)
  write.csv(
    x = data,
    file = full_path,
    row.names = FALSE
  )
  cat(paste("Saved data to:", full_path, "\n"))
}

# Load the enhanced dataset
boat2_data <- read.csv("data/BOAT2_Data_Enhanced.csv")

# Define variable groups (from data preparation script)
# Organizational structure variables
numerical_org_vars <- c(
  "ORG_Size_Employees",
  "ORG_Complexity_Locations",
  "ORG_Complexity_Departments",
  "ORG_Hierarchy_Layers"
)

# Decision-making variables (updated to match new descriptive column names)
merged_ordinal_vars <- c(
  # 决策分布变量
  "DEC_Authority_Dispersion", 
  "DEC_Authority_Delegation",
  "DEC_Process_InformalCommunication", 
  "DEC_Process_InformalProcedures",
  
  # 决策风格变量
  "STY_Analytical_DataDriven",
  "STY_Participative_Inclusion", 
  "STY_Participative_Relational",
  "STY_Organic_InformalStructure", 
  "STY_Organic_Adaptability",
  "STY_Directive_Threats", 
  "STY_Directive_Compliance",
  
  # 组织文化变量
  "CUL_Authority_Hierarchical",
  "CUL_Integration_Vision",
  "CUL_Integration_Systematic",
  "CUL_Innovation_Experimental",
  "CUL_Collaboration_Stakeholder",
  
  # 决策灵活性变量
  "FLEX_Cognitive_Receptivity",
  "FLEX_Behavioral_Adaptability",
  
  # 风险与环境变量
  "RISK_Appetite_Investment",
  "ENV_Context_Growth",
  "ENV_Context_Volatility",
  "ENV_Context_Stability"
)

# PDM variable (updated to match actual column name)
pdm_var <- "PDM_Selected"

# Define attractive color schemes
# Modern color palette for histograms
hist_fill_color <- "#4682B4"  # Steel blue
hist_color <- "#FFFFFF"       # White
density_fill <- "#FF7F50"     # Coral, more vibrant than red

# Decision variable histogram colors
decision_fill_color <- "#3CB371"  # Medium sea green
decision_color <- "#FFFFFF"       # White

# Custom theme with improved aesthetics
theme_custom <- theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 12),
    axis.title = element_text(face = "bold", size = 10),
    legend.title = element_text(face = "bold"),
    panel.grid.major = element_line(color = "#DDDDDD"),
    panel.grid.minor = element_line(color = "#F5F5F5"),
    panel.background = element_rect(fill = "#FCFCFC"),
    plot.background = element_rect(fill = "#FFFFFF")
  )

# 2. Distribution Analysis ---------------------------------------------------

# 2.1 Distribution of organizational structure variables
org_structure_plots <- list()

for (var in numerical_org_vars) {
  p <- ggplot(boat2_data, aes(x = .data[[var]])) +
    geom_histogram(bins = 15, fill = hist_fill_color, color = hist_color, alpha = 0.8) +
    geom_density(alpha = 0.4, fill = density_fill, color = "#E8491D") +
    labs(
      title = paste("Distribution of", gsub("_", " ", var)),
      x = gsub("ORG_", "", var),
      y = "Frequency"
    ) +
    theme_custom
    
  org_structure_plots[[var]] <- p
}

# Combine plots
org_structure_combined <- gridExtra::grid.arrange(
  grobs = org_structure_plots,
  ncol = 2
)

# Save the combined plot
save_exploration_plot(org_structure_combined, "org_structure_distributions.pdf", width = 10, height = 8)

# 2.2 Distribution of ordinal decision variables
decision_var_plots <- list()

for (var in merged_ordinal_vars) {
  # 提取前缀作为变量类别（如DEC, STY, CUL等）
  prefix <- substr(var, 1, 3)
  
  # 提取实际的变量含义（如Authority_Dispersion）
  var_meaning <- gsub("^[A-Z]+_", "", var)
  
  p <- ggplot(boat2_data, aes(x = .data[[var]])) +
    geom_histogram(bins = 10, fill = decision_fill_color, color = decision_color, alpha = 0.8) +
    labs(
      title = paste(prefix, "-", gsub("_", " ", var_meaning)),
      x = gsub("_", " ", var_meaning),
      y = "Frequency"
    ) +
    theme_custom
    
  decision_var_plots[[var]] <- p
}

# Save individual plots for decision variables
for (var in names(decision_var_plots)) {
  save_exploration_plot(decision_var_plots[[var]],
    paste0("decision_var_", var, ".pdf"),
    width = 8, height = 6
  )
}

# 3. PDM Analysis -----------------------------------------------------------

# 3.1 PDM frequency
# Check if PDM variable exists in the dataset
if (pdm_var %in% colnames(boat2_data)) {
  cat("开始PDM分布分析...\n")
  tryCatch({
    # Create a vibrant color palette for PDM types
    pdm_colors <- c("#1E88E5", "#D81B60", "#8E24AA", "#FFC107", "#43A047")
    
    # 打印PDM变量的值，检查是否有问题
    cat("PDM变量取值: ", unique(boat2_data[[pdm_var]]), "\n")
    
    pdm_plot <- ggplot(boat2_data, aes(x = .data[[pdm_var]], fill = .data[[pdm_var]])) +
      geom_bar(alpha = 0.9) +
      geom_text(stat = "count", aes(label = after_stat(count)), vjust = -0.5, color = "#333333") +
      scale_fill_manual(values = pdm_colors) +
      labs(
        title = "Project Delivery Method Distribution",
        x = "Project Delivery Method",
        y = "Count"
      ) +
      theme_custom +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none"
      )

    # Save PDM plot
    save_exploration_plot(pdm_plot, "pdm_distribution.pdf", width = 8, height = 6)
    cat("PDM分布图保存成功\n")
  }, error = function(e) {
    cat("PDM分布分析出错:", conditionMessage(e), "\n")
  })
} else {
  cat("Warning: PDM variable column not found in boat2_data. Skipping PDM analysis.\n")
}

# 3.2 PDM vs Organizational Structure
if (pdm_var %in% colnames(boat2_data)) {
  cat("开始PDM与组织结构关系分析...\n")
  tryCatch({
    pdm_org_plots <- list()

    for (var in numerical_org_vars) {
      p <- ggplot(boat2_data, aes(x = .data[[pdm_var]], y = .data[[var]], fill = .data[[pdm_var]])) +
        geom_boxplot(alpha = 0.8) +
        scale_fill_manual(values = pdm_colors) +
        labs(
          title = paste("Relationship of PDM and", gsub("_", " ", var)),
          x = "Project Delivery Method",
          y = gsub("ORG_", "", var)
        ) +
        theme_custom +
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
    save_exploration_plot(pdm_org_combined, "pdm_org_structure_relationships.pdf", width = 12, height = 10)
    cat("PDM与组织结构关系图保存成功\n")
  }, error = function(e) {
    cat("PDM与组织结构关系分析出错:", conditionMessage(e), "\n")
  })
} else {
  cat("Warning: PDM variable column not found in boat2_data. Skipping PDM vs Org Structure analysis.\n")
}

# 4. Correlation Analysis ---------------------------------------------------

# 4.1 Correlation matrix of decision-making variables
decision_cor <- cor(boat2_data[, merged_ordinal_vars], use = "pairwise.complete.obs")

# Create a corrplot ggplot version with improved colors
corrplot_gg <- function(correlation_matrix, title = "Correlation Matrix") {
  # Convert correlation matrix to long format for ggplot
  corr_data <- reshape2::melt(correlation_matrix)
  
  # Create the correlation plot using ggplot2
  ggplot(corr_data, aes(x = Var1, y = Var2, fill = value)) +
    geom_tile() +
    scale_fill_gradient2(
      low = "#053061",  # Dark blue 
      mid = "#FFFFFF",  # White
      high = "#67001F", # Dark red
      midpoint = 0, 
      limits = c(-1, 1),
      name = "Correlation"
    ) +
    geom_text(aes(label = round(value, 2)), 
              size = 2.5, 
              color = ifelse(abs(corr_data$value) > 0.7, "white", "black")) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 6),
      axis.text.y = element_text(size = 6),
      axis.title = element_blank(),
      panel.grid = element_blank(),
      plot.title = element_text(face = "bold", hjust = 0.5, size = 12)
    ) +
    labs(title = title)
}

# Create and save correlation plot
decision_corr_plot <- corrplot_gg(decision_cor, "Correlation Matrix of Decision-Making Variables")
save_exploration_plot(decision_corr_plot, "decision_correlation_matrix.pdf", width = 12, height = 10)

# 4.2 Correlations with organizational structure
# Calculate correlations between decision variables and organizational structure
org_decision_cor <- cor(
  boat2_data[, c(numerical_org_vars, merged_ordinal_vars)],
  use = "pairwise.complete.obs"
)

# Extract just the correlations between org structure and decision variables
org_decision_subset <- org_decision_cor[
  numerical_org_vars,
  merged_ordinal_vars,
  drop = FALSE
]

# Create correlation heatmap with improved colors
org_decision_heatmap <- ggplot(
  data = reshape2::melt(org_decision_subset),
  aes(x = Var2, y = Var1, fill = value)
) +
  geom_tile() +
  scale_fill_gradient2(
    low = "#053061",      # Dark blue
    mid = "#FFFFFF",      # White
    high = "#67001F",     # Dark red
    midpoint = 0,
    limits = c(-1, 1),
    name = "Correlation"
  ) +
  geom_text(aes(label = round(value, 2)), 
            size = 2.8, 
            color = ifelse(abs(reshape2::melt(org_decision_subset)$value) > 0.6, "white", "black")) +
  labs(
    title = "Correlations: Organization Structure vs Decision Variables",
    x = "Decision Variables",
    y = "Organization Structure"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
    axis.text.y = element_text(size = 8),
    panel.grid = element_blank(),
    axis.title = element_text(face = "bold", size = 10)
  )

# Save the heatmap
save_exploration_plot(org_decision_heatmap, "org_decision_correlations.pdf", width = 14, height = 8)

# 5. Multidimensional Analysis -----------------------------------------------

# 5.1 Simple scatterplot matrix of key variables
cat("\n生成关键变量散点图矩阵...\n")
# Select a subset of important variables for visualization (减少变量数量以提高性能)
key_vars <- c(
  "DEC_Authority_Dispersion",
  "STY_Participative_Inclusion",
  "CUL_Integration_Vision",
  "FLEX_Cognitive_Receptivity"
)

# 更健壮的错误处理
tryCatch({
  # Create scatterplot matrix with improved aesthetics
  if (pdm_var %in% colnames(boat2_data)) {
    cat("  使用PDM分组创建散点图矩阵...\n")
    pairs_plot <- GGally::ggpairs(
      boat2_data,
      columns = key_vars,
      mapping = aes(color = .data[[pdm_var]]),
      upper = list(continuous = "cor"),
      lower = list(continuous = "points"),
      diag = list(continuous = "densityDiag"),
      legend = 1
    ) +
      theme_minimal() +
      theme(
        strip.background = element_rect(fill = "#F5F5F5"),
        strip.text = element_text(face = "bold", color = "#333333"),
        axis.text = element_text(size = 8),
        legend.position = "bottom"
      ) +
      labs(title = "Relationships Among Key Decision Variables")
    
    # Use the PDM colors
    if (length(unique(boat2_data[[pdm_var]])) <= length(pdm_colors)) {
      pairs_plot <- pairs_plot + scale_color_manual(values = pdm_colors) + scale_fill_manual(values = pdm_colors)
    }
    
  } else {
    cat("  创建不带PDM分组的散点图矩阵...\n")
    pairs_plot <- GGally::ggpairs(
      boat2_data,
      columns = key_vars,
      upper = list(continuous = "cor"),
      lower = list(continuous = "points"),
      diag = list(continuous = "densityDiag")
    ) +
      theme_minimal() +
      theme(
        strip.background = element_rect(fill = "#F5F5F5"),
        strip.text = element_text(face = "bold", color = "#333333"),
        axis.text = element_text(size = 8)
      ) +
      labs(title = "Relationships Among Key Decision Variables")
  }

  # Save pairs plot
  save_exploration_plot(pairs_plot, "key_variable_pairs.pdf", width = 12, height = 10)
  cat("散点图矩阵保存成功\n")
}, error = function(e) {
  cat("创建散点图矩阵时出错:", conditionMessage(e), "\n")
})

# 6. Summary Statistics by PDM Type ------------------------------------------
cat("\n生成PDM类型统计摘要...\n")

# Calculate summary statistics by PDM Type
tryCatch({
  if (pdm_var %in% colnames(boat2_data)) {
    pdm_summaries <- boat2_data %>%
      group_by(.data[[pdm_var]]) %>%
      summarise(across(
        all_of(c(numerical_org_vars, key_vars)), # 使用减少后的变量集提高性能
        list(
          mean = ~ mean(., na.rm = TRUE),
          sd = ~ sd(., na.rm = TRUE),
          median = ~ median(., na.rm = TRUE)
        ),
        .names = "{.col}_{.fn}"
      ))

    # Save summary statistics
    save_exploration_data(pdm_summaries, "pdm_variable_summaries.csv")
    cat("PDM统计摘要保存成功\n")
  } else {
    cat("警告: 未在boat2_data中找到PDM变量列。跳过PDM统计摘要。\n")
  }
}, error = function(e) {
  cat("生成PDM统计摘要出错:", conditionMessage(e), "\n")
})

# 7. ANOVA Analysis for PDM Differences --------------------------------------
cat("\n开始ANOVA分析...\n")

if (pdm_var %in% colnames(boat2_data)) {
  tryCatch({
    # Function to run ANOVA and extract p-values - 更健壮的实现
    run_anova <- function(variable, data) {
      tryCatch({
        formula <- as.formula(paste(variable, "~", pdm_var))
        model <- aov(formula, data = data)
        summary_table <- summary(model)
        
        # 从ANOVA表中提取p值 - 更安全的方法
        if (length(summary_table) > 0 && "Pr(>F)" %in% colnames(summary_table[[1]])) {
          # 找到PDM变量所在的行
          pdm_row <- which(rownames(summary_table[[1]]) == pdm_var)
          if (length(pdm_row) > 0) {
            p_value <- summary_table[[1]][pdm_row, "Pr(>F)"]
            return(p_value)
          }
        }
        cat("  无法从ANOVA结果中提取", variable, "的p值\n")
        return(NA)
      }, error = function(e) {
        cat("  运行", variable, "的ANOVA分析时出错:", conditionMessage(e), "\n")
        return(NA)
      })
    }

    # 使用一小部分变量进行测试
    test_vars <- c(numerical_org_vars, key_vars)
    cat("使用", length(test_vars), "个变量进行ANOVA分析\n")
    
    # 创建结果数据框
    anova_results <- data.frame(
      Variable = character(),
      P_Value = numeric(),
      Significant = logical(),
      stringsAsFactors = FALSE
    )

    # 逐个处理变量
    for (var in test_vars) {
      cat("  处理变量:", var, "\n")
      p_val <- run_anova(var, boat2_data)
      if (!is.na(p_val)) {
        anova_results <- rbind(anova_results, data.frame(
          Variable = var,
          P_Value = p_val,
          Significant = p_val < 0.05,
          stringsAsFactors = FALSE
        ))
      }
    }

    # 只有当有结果时才继续处理
    if (nrow(anova_results) > 0) {
      # Sort by p-value
      anova_results <- anova_results[order(anova_results$P_Value), ]

      # Save ANOVA results
      save_exploration_data(anova_results, "pdm_anova_results.csv")
      cat("ANOVA结果表格保存成功\n")

      # Create visualization of ANOVA results with improved colors
      anova_plot <- ggplot(anova_results, aes(x = reorder(Variable, -P_Value), y = -log10(P_Value), fill = Significant)) +
        geom_bar(stat = "identity", alpha = 0.9) +
        geom_hline(yintercept = -log10(0.05), linetype = "dashed", color = "#D55E00", size = 1) +
        scale_fill_manual(values = c("#BBBBBB", "#2D708E")) +
        labs(
          title = "ANOVA Results: Variables Differing by PDM Type",
          subtitle = "Higher bars indicate stronger evidence of difference; red line at p=0.05",
          x = "Variable",
          y = "-log10(p-value)"
        ) +
        theme_custom +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
          plot.subtitle = element_text(size = 9),
          legend.position = "top"
        )

      # Save ANOVA plot
      save_exploration_plot(anova_plot, "pdm_anova_plot.pdf", width = 12, height = 8)
      cat("ANOVA分析图保存成功\n")
    } else {
      cat("警告: ANOVA分析没有产生有效结果\n")
    }
  }, error = function(e) {
    cat("ANOVA分析出错:", conditionMessage(e), "\n")
  })
} else {
  cat("警告: 未在boat2_data中找到PDM变量列。跳过ANOVA分析。\n")
}

# Print completion message
cat("\n探索性数据分析完成!\n")
cat(paste("可视化图表已保存到:", exploration_figures_dir, "\n"))
cat(paste("统计摘要已保存到:", exploration_tables_dir, "\n"))
