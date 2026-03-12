# R/092_cluster_pdm_pie_charts_k4.R
# 此脚本创建饼图来可视化每个簇(k=4)中PDM经验的分布
# 数据从脚本090的输出加载

# 1. 加载Setup和数据 -----------------------------------------------------
cat("============== SCRIPT R/092 (PDM Experience Pie Charts k4) STARTING ==============\n")

# 确保加载必要的包
if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
if (!requireNamespace("readr", quietly = TRUE)) install.packages("readr")
if (!requireNamespace("RColorBrewer", quietly = TRUE)) install.packages("RColorBrewer")
if (!requireNamespace("gridExtra", quietly = TRUE)) install.packages("gridExtra")
if (!requireNamespace("tidyr", quietly = TRUE)) install.packages("tidyr")
if (!requireNamespace("patchwork", quietly = TRUE)) install.packages("patchwork")

library(dplyr)
library(ggplot2)
library(readr)
library(RColorBrewer)
library(gridExtra)
library(tidyr)
library(patchwork)  # 用于组合图

# 辅助函数：从ggplot提取图例
get_legend <- function(a_ggplot) {
  tmp <- ggplot_gtable(ggplot_build(a_ggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  if(length(leg) > 0) {
    legend <- tmp$grobs[[leg]]
    return(legend)
  } else {
    return(NULL)
  }
}

# 尝试加载setup文件
if (file.exists("R/000_setup.R")) {
  source("R/000_setup.R")
  cat("R/000_setup.R sourced successfully.\n")
} else {
  cat("Warning: R/000_setup.R not found. Essential packages loaded directly.\n")
}

# 2. 配置 ----------------------------------------------------------
K_VALUE_FIXED <- 4 # <--- CHANGED
INPUT_TABLES_DIR_090 <- paste0("results/tables/090_kprototype_post_1_removal_k", K_VALUE_FIXED) # <--- CHANGED from 081
CLUSTER_DATA_FILENAME <- paste0("kproto_clusters_k", K_VALUE_FIXED, "_post_1_removed.csv")
CLUSTER_DATA_FILEPATH <- file.path(INPUT_TABLES_DIR_090, CLUSTER_DATA_FILENAME) # <--- Uses new input dir

# 为此脚本的结果创建子目录(092)
FIG_DIR_092 <- paste0("results/figures/092_pdm_experience_pie_charts_k", K_VALUE_FIXED) # <--- CHANGED from 084
if (!dir.exists(FIG_DIR_092)) {
  dir.create(FIG_DIR_092, recursive = TRUE)
  cat(paste("Created directory:", FIG_DIR_092, "\n"))
}
TBL_DIR_092 <- paste0("results/tables/092_pdm_experience_pie_charts_k", K_VALUE_FIXED) # <--- ADDED for tables
if (!dir.exists(TBL_DIR_092)) {
  dir.create(TBL_DIR_092, recursive = TRUE)
  cat(paste("Created directory:", TBL_DIR_092, "\n"))
}


# 3. 定义PDM经验变量 -----------------------------------------
pdm_experience_vars <- c(
  "PDM_Experience_DBB",
  "PDM_Experience_DB",
  "PDM_Experience_PDB", 
  "PDM_Experience_CMAR",
  "PDM_Experience_IPD"
)

# 创建更直观的标签用于可视化(删除括号)
pdm_experience_labels <- c(
  "PDM_Experience_DBB" = "Design-Bid-Build",
  "PDM_Experience_DB" = "Design-Build",
  "PDM_Experience_PDB" = "Progressive Design-Build", 
  "PDM_Experience_CMAR" = "Construction Manager @ Risk",
  "PDM_Experience_IPD" = "Integrated Project Delivery"
)

# 4. 定义经验级别类别(更正为1-4级) -------------
experience_categories <- c(
  "Never used & not familiar" = 1,
  "Never used but familiar" = 2,
  "Used once or twice" = 3,
  "Used many times" = 4
)

# 辅助函数：格式化多行标题
format_title <- function(pdm_name, cluster_id) {
  # 在适当位置插入换行符
  if (pdm_name == "Progressive Design-Build") {
    title <- paste0("Progressive\nDesign-Build\nCluster ", cluster_id)
  } else if (pdm_name == "Construction Manager @ Risk") {
    title <- paste0("Construction Manager\n@ Risk\nCluster ", cluster_id)
  } else if (pdm_name == "Integrated Project Delivery") {
    title <- paste0("Integrated\nProject Delivery\nCluster ", cluster_id)
  } else if (pdm_name == "Design-Bid-Build") {
    title <- paste0("Design-Bid-Build\nCluster ", cluster_id) 
  } else if (pdm_name == "Design-Build") {
    title <- paste0("Design-Build\nCluster ", cluster_id)
  } else {
    title <- paste0(pdm_name, "\nCluster ", cluster_id)
  }
  
  return(title)
}

cat("Configuration complete. Output will be saved to:", FIG_DIR_092, "and", TBL_DIR_092, "\n") # <--- UPDATED message

# 5. 加载数据 --------------------------------------------------------------
cat("\n--- Loading cluster data from:", CLUSTER_DATA_FILEPATH, " ---\n")
if (!file.exists(CLUSTER_DATA_FILEPATH)) {
  stop(paste("Error: Cluster data file not found at", CLUSTER_DATA_FILEPATH))
}
clustered_data <- read_csv(CLUSTER_DATA_FILEPATH, show_col_types = FALSE)
cat("Loaded clustered data with", nrow(clustered_data), "rows and", ncol(clustered_data), "columns.\n")

# 标准化集群列名
cluster_col_original <- paste0("Cluster_k", K_VALUE_FIXED)
if (cluster_col_original %in% colnames(clustered_data)) {
  clustered_data <- clustered_data %>%
    rename(Cluster = !!sym(cluster_col_original))
  cat("Renamed cluster column '", cluster_col_original, "' to 'Cluster'.\n")
} else if (!"Cluster" %in% colnames(clustered_data)){
  stop("Cluster assignment column not found in the loaded data.")
}

# 6. 创建每个PDM类型的饼图 ------------------------
cat("\n--- Generating PDM experience pie charts for each PDM type ---\n")

create_pdm_pie_charts <- function(pdm_var) {
  # 创建一个更易读的PDM名称用于标题
  pdm_name <- pdm_experience_labels[pdm_var]
  if (is.na(pdm_name)) {
    pdm_name <- gsub("PDM_Experience_", "", pdm_var)
  }
  
  cat("  Creating pie charts for", pdm_name, "\n")
  
  # 定义专业颜色板用于经验级别(4级)
  experience_colors <- c(
    "Never used & not familiar" = "#E41A1C",    # 红色
    "Never used but familiar" = "#FF7F00",      # 橙色
    "Used once or twice" = "#4DAF4A",           # 绿色
    "Used many times" = "#377EB8"               # 蓝色
  )
  
  plots_list <- list()
  
  # 按数字顺序排序集群ID
  sorted_clusters <- sort(unique(clustered_data$Cluster))
  
  for (cluster_id in sorted_clusters) {
    # 过滤此集群的数据
    cluster_subset <- clustered_data %>%
      filter(Cluster == cluster_id)
    
    if (nrow(cluster_subset) == 0) {
      cat("    Warning: No data found for Cluster", cluster_id, ". Skipping pie chart.\n")
      next
    }
    
    # 准备饼图数据
    pie_data <- cluster_subset %>%
      group_by(!!sym(pdm_var)) %>%
      summarise(count = n(), .groups = 'drop') %>%
      mutate(
        percentage = count / sum(count) * 100,
        label = paste0(count, " (", sprintf("%.1f%%", percentage), ")"),
        Experience = factor(!!sym(pdm_var), 
                          levels = unname(experience_categories),
                          labels = names(experience_categories))
      ) %>%
      arrange(Experience)
    
    # 确保所有经验级别都有代表(即使计数为0)
    all_experiences <- data.frame(
      Experience = factor(names(experience_categories), 
                        levels = names(experience_categories))
    )
    
    pie_data <- all_experiences %>%
      left_join(pie_data, by = "Experience") %>%
      mutate(
        Experience_Level = match(as.character(Experience), names(experience_categories)),
        count = ifelse(is.na(count), 0, count),
        percentage = ifelse(is.na(percentage), 0, percentage),
        label = ifelse(is.na(label), paste0(count, " (", sprintf("%.1f%%", percentage), ")"), label)
      )
    
    # 此集群的总计数
    total_count <- sum(pie_data$count)
    
    # 格式化标题，适当换行
    formatted_title <- format_title(pdm_name, cluster_id)
    
    # 创建饼图
    pie_chart <- ggplot(pie_data, aes(x = "", y = percentage, fill = Experience)) +
      geom_bar(stat = "identity", width = 0.6, color = "white") +
      coord_polar("y", start = 0) +
      geom_text(aes(label = ifelse(count > 0, label, "")), 
                position = position_stack(vjust = 0.5),
                size = 3.5, fontface = "bold") +
      labs(
        title = formatted_title,
        subtitle = paste0("n = ", total_count),
        fill = "Experience Level"
      ) +
      scale_fill_manual(values = experience_colors, drop = FALSE) +
      theme_void() +
      theme(
        plot.title = element_text(face = "bold", size = 14, hjust = 0.5, lineheight = 0.9),
        plot.subtitle = element_text(size = 12, hjust = 0.5),
        legend.position = "right",
        legend.title = element_text(face = "bold"),
        legend.key.size = unit(0.8, "cm"),
        legend.margin = margin(l = 0, r = 10, t = 0, b = 0),
        legend.spacing.y = unit(0.3, "cm")
      )
    
    plots_list[[paste0("cluster_", cluster_id)]] <- pie_chart
    
    # 保存单个饼图
    individual_plot_filename <- file.path(FIG_DIR_092, # <--- CHANGED to FIG_DIR_092
                                       paste0(gsub("PDM_Experience_", "", pdm_var), 
                                              "_cluster_", cluster_id, "_pie_chart.pdf"))
    
    ggsave(individual_plot_filename, 
           plot = pie_chart,
           width = 8, 
           height = 6,
           units = "in",
           device = "pdf")
    
    cat(paste("    Saved pie chart for", pdm_name, "Cluster", cluster_id, "to:", individual_plot_filename, "\n"))
  }
  
  # 使用patchwork组合所有图形并在底部共享图例
  if (length(plots_list) > 0) {
    # 设置图例到右侧并使其紧凑
    for (i in 1:length(plots_list)) {
      plots_list[[i]] <- plots_list[[i]] + 
        theme(legend.position = "right",
              legend.key.size = unit(0.8, "cm"),
              legend.margin = margin(l = 0, r = 10, t = 0, b = 0),
              legend.spacing.y = unit(0.3, "cm"))
    }
    
    # For K=4, use 2 columns for the combined plot
    plot_cols <- if (K_VALUE_FIXED == 4) 2 else 3 # <--- DYNAMIC based on K_VALUE_FIXED
    plot_rows <- ceiling(K_VALUE_FIXED / plot_cols)
    
    combined_plot <- wrap_plots(plots_list, ncol = plot_cols, nrow = plot_rows) + # <--- Use dynamic cols/rows
      plot_layout(guides = "collect") &
      theme(legend.position = "right",
            legend.box = "vertical",
            legend.title = element_text(face = "bold"))
    
    # 添加主标题
    final_plot <- combined_plot + 
      plot_annotation(
        title = paste0(pdm_name, " Experience by Cluster (k=", K_VALUE_FIXED, ")"),
        theme = theme(plot.title = element_text(face = "bold", size = 16, hjust = 0.5))
      )
    
    # 保存组合图
    combined_filename <- file.path(FIG_DIR_092, # <--- CHANGED to FIG_DIR_092
                                 paste0(gsub("PDM_Experience_", "", pdm_var), 
                                        "_all_clusters_k", K_VALUE_FIXED, ".pdf"))
    
    ggsave(
      combined_filename,
      final_plot,
      width = ifelse(K_VALUE_FIXED == 4, 10, 12), # Adjust width for 2 cols vs 3
      height = ifelse(K_VALUE_FIXED == 4, 6 * plot_rows, 9), # Adjust height
      dpi = 300
    )
    
    cat("  Combined plot for", pdm_name, "saved to:", combined_filename, "\n")
    
    # 也保存为PNG以便于查看
    png_filename <- file.path(FIG_DIR_092, # <--- CHANGED to FIG_DIR_092
                            paste0(gsub("PDM_Experience_", "", pdm_var), 
                                   "_all_clusters_k", K_VALUE_FIXED, ".png"))
    
    ggsave(
      png_filename,
      final_plot,
      width = ifelse(K_VALUE_FIXED == 4, 10, 12), # Adjust width
      height = ifelse(K_VALUE_FIXED == 4, 6 * plot_rows, 9), # Adjust height
      dpi = 300
    )
  } else {
    cat("  No plots were generated to save for", pdm_name, "\n")
  }
  
  return(plots_list)
}

# 为每个PDM经验变量创建饼图
all_pdm_plots <- list()
for (pdm_var in pdm_experience_vars) {
  all_pdm_plots[[pdm_var]] <- create_pdm_pie_charts(pdm_var)
}

# 7. 创建经验分布的汇总表 ---------------------------------------
cat("\n--- Creating summary tables of PDM experience distributions ---\n")

experience_summary <- data.frame()

for (pdm_var in pdm_experience_vars) {
  pdm_summary <- clustered_data %>%
    group_by(Cluster, !!sym(pdm_var)) %>%
    summarise(Count = n(), .groups = "drop") %>%
    mutate(
      PDM_Type = pdm_var,
      Experience_Level = !!sym(pdm_var)
    ) %>%
    select(Cluster, PDM_Type, Experience_Level, Count)
  
  experience_summary <- bind_rows(experience_summary, pdm_summary)
}

# 保存汇总表
# summary_table_path variable removed, using TBL_DIR_092 directly

summary_filename <- file.path(TBL_DIR_092, paste0("pdm_experience_distribution_k", K_VALUE_FIXED, ".csv")) # <--- CHANGED to TBL_DIR_092
write_csv(experience_summary, summary_filename)
cat("Summary table saved to:", summary_filename, "\n")

cat("\n============== SCRIPT R/092 (PDM Experience Pie Charts k4) FINISHED ==============\n") 