# BOAT2 Cluster and Factor Analysis - Enhanced Factor Analysis Visualizations
# This script creates improved visualizations for the factor analysis results

# Load required libraries
library(tidyverse)
library(ggplot2)
library(reshape2)
library(viridis)
library(corrplot)
library(fmsb)
library(gridExtra)
library(psych)

# 1. Load Setup and Data -----------------------------------------------------
source("R/000_setup.R")

# Create subdirectories for results if they don't exist
vis_dir <- "results/figures/041_factor_visualization"
if (!dir.exists(vis_dir)) {
  dir.create(vis_dir, recursive = TRUE)
  cat(paste("Created directory:", vis_dir, "\n"))
}

# Also create tables directory if it doesn't exist
tables_dir <- "results/tables/041_factor_visualization"
if (!dir.exists(tables_dir)) {
  dir.create(tables_dir, recursive = TRUE)
  cat(paste("Created directory:", tables_dir, "\n"))
}

# Load the factor analysis results
cat("Loading factor analysis results...\n")

# Load best factor loadings (using 4 factors as determined in 040_Factoring.R)
factor_loadings <- read.csv("results/tables/040_factoring/factor_loadings_4.csv", row.names = 1)
significant_loadings <- read.csv("results/tables/040_factoring/significant_loadings_4.csv", row.names = 1)
factor_scores <- read.csv("results/tables/040_factoring/factor_scores_4.csv")

# Load the original dataset for additional analyses
data <- read.csv("data/BOAT2_Data_Enhanced.csv")

# Define factor names based on our interpretation
factor_names <- c(
  "Flexibility & Adaptability",
  "Directive & Compliance", 
  "Informal Process & Communication",
  "Distributed Decision-Making & Analysis"
)

# 2. Create an enhanced correlation heatmap of factors -----------------------
cat("Creating enhanced correlation heatmap...\n")

# Create a cleaner version of the loading matrix
clean_loadings <- as.matrix(factor_loadings)

# Ensure the factors are in the correct order for our factor names
# Get column names and check order
factor_cols <- colnames(clean_loadings)

# Define custom colors for the heatmap
heatmap_colors <- colorRampPalette(c("#4575B4", "white", "#D73027"))(100)

# Create a heatmap of factor loadings with numbers
pdf(paste0(vis_dir, "/factor_loading_heatmap.pdf"), width = 10, height = 12)
corrplot(clean_loadings, method = "color", 
         col = heatmap_colors,
         tl.col = "black", tl.srt = 45, 
         tl.cex = 0.8,
         title = "Factor Loadings Heatmap",
         mar = c(0, 0, 1, 0),
         cl.ratio = 0.2,
         cl.align = "r",
         addCoef.col = "black",    # 添加数字标签，黑色
         number.cex = 0.7,         # 调整数字大小
         number.digits = 2)        # 显示2位小数
dev.off()

# Also save as PNG
png(paste0(vis_dir, "/factor_loading_heatmap.png"), width = 1000, height = 1200, res = 120)
corrplot(clean_loadings, method = "color", 
         col = heatmap_colors,
         tl.col = "black", tl.srt = 45, 
         tl.cex = 0.8,
         title = "Factor Loadings Heatmap",
         mar = c(0, 0, 1, 0),
         cl.ratio = 0.2,
         cl.align = "r",
         addCoef.col = "black",    # 添加数字标签，黑色
         number.cex = 0.7,         # 调整数字大小
         number.digits = 2)        # 显示2位小数
dev.off()

# 3. Create radar charts for factor profiles by PDM --------------------------
cat("Creating radar charts for factor profiles by PDM...\n")

# Prepare data for radar charts - get mean factor scores by PDM
# Ensure PDM_Selected exists in factor_scores
if (!"PDM_Selected" %in% colnames(factor_scores)) {
  cat("WARNING: PDM_Selected column not found in factor_scores. Using PDM column from original data...\n")
  
  # Merge factor scores with original data to get PDM information
  # Assuming the rows in factor_scores match the rows in the original data
  if(nrow(factor_scores) == nrow(data)) {
    factor_scores$PDM_Selected <- data$PDM_Selected
  } else {
    stop("Cannot match factor scores with original data. Please check your data.")
  }
}

radar_data <- factor_scores %>%
  select(-X) %>%
  group_by(PDM_Selected) %>%
  summarise(across(starts_with("MR"), mean, na.rm = TRUE)) %>%
  as.data.frame()

# Set PDM as row names for radar chart
rownames(radar_data) <- radar_data$PDM_Selected
radar_data <- radar_data %>% select(-PDM_Selected)

# Rename columns to factor names
colnames(radar_data) <- factor_names

# Add max and min for radar chart
radar_data_bounds <- rbind(
  apply(radar_data, 2, max) + 0.5,  # Max values with a little buffer
  apply(radar_data, 2, min) - 0.5,  # Min values with a little buffer
  radar_data
)

# Define colors for PDMs
pdm_colors <- c(
  "Design-Bid-Build" = "#D46A6A",             # Red (DBB)
  "Construction Manager @ Risk" = "#E3C567",  # Yellow (CMAR)
  "Design-Build" = "#9CCF9C",                 # Light green (DB)
  "Progressive Design-Build" = "#4A8F4A",     # Dark green (PDB)
  "Integrated Project Delivery (IPD)" = "#6A95CA" # Blue (IPD)
)

# Create radar chart
pdf(paste0(vis_dir, "/pdm_factor_radar.pdf"), width = 10, height = 10)
par(mar = c(1, 1, 3, 1))
radarchart(
  radar_data_bounds, 
  pcol = pdm_colors[rownames(radar_data)],
  pfcol = adjustcolor(pdm_colors[rownames(radar_data)], alpha.f = 0.3),
  plwd = 2,
  plty = 1,
  cglcol = "grey",
  cglty = 1,
  axislabcol = "grey30",
  caxislabels = seq(-2, 2, 1),
  title = "Factor Profiles by Project Delivery Method",
  vlcex = 0.9
)
legend(
  "topright",
  legend = rownames(radar_data),
  col = pdm_colors[rownames(radar_data)],
  lty = 1,
  lwd = 2,
  pch = 16,
  pt.cex = 1.5,
  bty = "n"
)
dev.off()

# Also save as PNG
png(paste0(vis_dir, "/pdm_factor_radar.png"), width = 1000, height = 1000, res = 120)
par(mar = c(1, 1, 3, 1))
radarchart(
  radar_data_bounds, 
  pcol = pdm_colors[rownames(radar_data)],
  pfcol = adjustcolor(pdm_colors[rownames(radar_data)], alpha.f = 0.3),
  plwd = 2,
  plty = 1,
  cglcol = "grey",
  cglty = 1,
  axislabcol = "grey30",
  caxislabels = seq(-2, 2, 1),
  title = "Factor Profiles by Project Delivery Method",
  vlcex = 0.9
)
legend(
  "topright",
  legend = rownames(radar_data),
  col = pdm_colors[rownames(radar_data)],
  lty = 1,
  lwd = 2,
  pch = 16,
  pt.cex = 1.5,
  bty = "n"
)
dev.off()

# 4. Create enhanced boxplots for factor scores by PDM ----------------------
cat("Creating enhanced boxplots for factor scores by PDM...\n")

# Reshape data for ggplot
factor_long <- factor_scores %>%
  select(-X) %>%
  pivot_longer(
    cols = starts_with("MR"),
    names_to = "Factor",
    values_to = "Score"
  )

# Create a mapping between MR columns and factor names
factor_mapping <- setNames(factor_names, paste0("MR", 1:4))

# Replace factor codes with descriptive names
factor_long$Factor <- factor_mapping[factor_long$Factor]

# Convert PDM_Selected to factor with ordered levels
pdm_levels <- c(
  "Design-Bid-Build",
  "Construction Manager @ Risk", 
  "Design-Build", 
  "Progressive Design-Build", 
  "Integrated Project Delivery (IPD)"
)

factor_long$PDM_Selected <- factor(factor_long$PDM_Selected, levels = pdm_levels)
factor_long$Factor <- factor(factor_long$Factor, levels = factor_names)

# Create combined boxplot with violin plot overlay
p <- ggplot(factor_long, aes(x = PDM_Selected, y = Score, fill = PDM_Selected)) +
  geom_violin(alpha = 0.3, width = 0.8) +
  geom_boxplot(width = 0.4, alpha = 0.7, outlier.size = 0.5) +
  facet_wrap(~ Factor, ncol = 2, scales = "free_y") +
  labs(
    title = "Factor Score Distributions by Project Delivery Method",
    x = "Project Delivery Method",
    y = "Factor Score",
    fill = "PDM"
  ) +
  scale_fill_manual(values = pdm_colors) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold", size = 11),
    strip.background = element_rect(fill = "gray95", color = NA),
    plot.title = element_text(face = "bold", hjust = 0.5, size = 14),
    legend.position = "bottom",
    legend.title = element_text(face = "bold")
  )

# Save plot
ggsave(paste0(vis_dir, "/factor_pdm_boxplots.pdf"), p, width = 12, height = 10)
ggsave(paste0(vis_dir, "/factor_pdm_boxplots.png"), p, width = 12, height = 10, dpi = 120)

# 5. Create a factor loading diagram with variable groups -------------------
cat("Creating enhanced factor loading diagram...\n")

# Prepare data for the diagram
load_diagram_data <- as.data.frame(clean_loadings)

# Define variable groups (using prefixes)
var_groups <- list(
  "Decision Distribution" = c("DEC_Authority_Dispersion", "DEC_Authority_Delegation", 
                            "DEC_Process_InformalCommunication", "DEC_Process_InformalProcedures"),
  "Decision Style" = c("STY_Analytical_DataDriven", "STY_Participative_Inclusion", 
                      "STY_Participative_Relational", "STY_Organic_InformalStructure", 
                      "STY_Organic_Adaptability", "STY_Directive_Threats", "STY_Directive_Compliance"),
  "Culture" = c("CUL_Authority_Hierarchical", "CUL_Integration_Vision", 
               "CUL_Integration_Systematic", "CUL_Innovation_Experimental", 
               "CUL_Collaboration_Stakeholder"),
  "Flexibility" = c("FLEX_Cognitive_Receptivity", "FLEX_Behavioral_Adaptability"),
  "Risk & Environment" = c("RISK_Appetite_Investment", "ENV_Context_Growth", 
                          "ENV_Context_Volatility", "ENV_Context_Stability")
)

# Add a 'group' column to each row based on variable name
load_diagram_data$Variable <- rownames(load_diagram_data)
load_diagram_data$Group <- NA

for (group_name in names(var_groups)) {
  vars_in_group <- var_groups[[group_name]]
  load_diagram_data$Group[load_diagram_data$Variable %in% vars_in_group] <- group_name
}

# Melt data for ggplot
load_long <- load_diagram_data %>%
  pivot_longer(
    cols = starts_with("MR"),
    names_to = "Factor",
    values_to = "Loading"
  )

# Create a mapping between MR columns and factor names
factor_mapping <- setNames(factor_names, colnames(clean_loadings))
load_long$Factor <- factor_mapping[load_long$Factor]

# Create enhanced loading plot
p2 <- ggplot(load_long, aes(x = Variable, y = Loading, fill = Loading)) +
  geom_bar(stat = "identity") +
  facet_grid(Group ~ Factor, scales = "free_y", space = "free_y") +
  scale_fill_gradient2(low = "#4575B4", mid = "white", high = "#D73027", midpoint = 0) +
  labs(
    title = "Factor Loadings by Variable Group",
    x = NULL,
    y = "Loading Value",
    fill = "Loading"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, size = 6),
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold"),
    strip.text.y = element_text(angle = 0),
    plot.title = element_text(face = "bold", hjust = 0.5, size = 14),
    legend.position = "right"
  ) +
  coord_flip()

# Save the plot
ggsave(paste0(vis_dir, "/factor_loadings_by_group.pdf"), p2, width = 14, height = 10)
ggsave(paste0(vis_dir, "/factor_loadings_by_group.png"), p2, width = 14, height = 10, dpi = 120)

# 6. Create a scatter plot matrix of factor scores colored by PDM -----------
cat("Creating factor score scatter plot matrix...\n")

# Prepare data for scatterplot matrix
scatter_data <- factor_scores %>%
  select(-X) %>%
  rename_with(~ factor_names, starts_with("MR"))

# Add PDM as a factor column
scatter_data$PDM_Selected <- factor(scatter_data$PDM_Selected, levels = pdm_levels)

# Create pairs plot
pdf(paste0(vis_dir, "/factor_scatterplot_matrix.pdf"), width = 10, height = 10)
pairs.panels(
  scatter_data[, factor_names],
  bg = pdm_colors[scatter_data$PDM_Selected],
  pch = 21,
  cex = 1.2,
  main = "Scatterplot Matrix of Factor Scores",
  smoother = FALSE,
  rug = FALSE
)

# Add a legend manually
par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
plot(0, 0, type = "n", axes = FALSE, xlab = "", ylab = "")
legend("bottom", 
       legend = pdm_levels,
       pt.bg = pdm_colors[pdm_levels],
       pch = 21,
       bty = "n",
       cex = 0.8,
       ncol = 5)
dev.off()

# Also save as PNG
png(paste0(vis_dir, "/factor_scatterplot_matrix.png"), width = 1000, height = 1000, res = 120)
pairs.panels(
  scatter_data[, factor_names],
  bg = pdm_colors[scatter_data$PDM_Selected],
  pch = 21,
  cex = 1.2,
  main = "Scatterplot Matrix of Factor Scores",
  smoother = FALSE,
  rug = FALSE
)

# Add a legend manually
par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
plot(0, 0, type = "n", axes = FALSE, xlab = "", ylab = "")
legend("bottom", 
       legend = pdm_levels,
       pt.bg = pdm_colors[pdm_levels],
       pch = 21,
       bty = "n",
       cex = 0.8,
       ncol = 5)
dev.off()

# 7. Create a table of mean factor scores by PDM ---------------------------
cat("Creating table of mean factor scores by PDM...\n")

# Compute mean factor scores by PDM
pdm_factor_means <- factor_scores %>%
  select(-X) %>%
  group_by(PDM_Selected) %>%
  summarise(across(starts_with("MR"), ~ mean(.x, na.rm = TRUE))) %>%
  ungroup()

# Rename the MR columns to factor names
pdm_factor_means <- pdm_factor_means %>%
  rename_with(~ factor_names, starts_with("MR"))

# Save the table
write.csv(pdm_factor_means, paste0(tables_dir, "/pdm_factor_means.csv"), row.names = FALSE)

# Create a heatmap of the means
pdm_means_matrix <- as.matrix(pdm_factor_means[, -1])
rownames(pdm_means_matrix) <- pdm_factor_means$PDM_Selected
colnames(pdm_means_matrix) <- factor_names

pdf(paste0(vis_dir, "/pdm_factor_means_heatmap.pdf"), width = 10, height = 7)
corrplot(pdm_means_matrix, is.corr = FALSE, 
         method = "color", 
         col = colorRampPalette(c("#4575B4", "white", "#D73027"))(100),
         tl.col = "black", tl.srt = 45, 
         cl.align.text = "l",
         title = "Mean Factor Scores by Project Delivery Method",
         addCoef.col = "black",    # 添加数字标签，黑色
         number.cex = 0.9,         # 调整数字大小
         number.digits = 2)        # 显示2位小数
dev.off()

# Also save as PNG
png(paste0(vis_dir, "/pdm_factor_means_heatmap.png"), width = 1000, height = 700, res = 120)
corrplot(pdm_means_matrix, is.corr = FALSE, 
         method = "color", 
         col = colorRampPalette(c("#4575B4", "white", "#D73027"))(100),
         tl.col = "black", tl.srt = 45, 
         cl.align.text = "l",
         title = "Mean Factor Scores by Project Delivery Method",
         addCoef.col = "black",    # 添加数字标签，黑色
         number.cex = 0.9,         # 调整数字大小
         number.digits = 2)        # 显示2位小数
dev.off()

# 8. Create a heat map of top loading variables for each factor -------------
cat("Creating heat map of top loading variables for each factor...\n")

# 为每个因子找出最重要的变量（绝对值最高的负荷）
top_n_variables <- 5  # 每个因子显示前5个最重要的变量

# 提取每个因子最高负荷的变量
top_vars_by_factor <- lapply(1:ncol(clean_loadings), function(i) {
  # 获取该因子的所有变量负荷
  factor_loads <- clean_loadings[, i]
  # 按照绝对值大小排序并取前N个
  top_indices <- order(abs(factor_loads), decreasing = TRUE)[1:top_n_variables]
  return(rownames(clean_loadings)[top_indices])
})

# 构建一个新的矩阵，只包含每个因子的顶部变量
top_vars_unique <- unique(unlist(top_vars_by_factor))
top_loadings_matrix <- matrix(NA, nrow = length(top_vars_unique), ncol = ncol(clean_loadings))
rownames(top_loadings_matrix) <- top_vars_unique
colnames(top_loadings_matrix) <- colnames(clean_loadings)

# 填充矩阵
for (i in 1:length(top_vars_unique)) {
  var_name <- top_vars_unique[i]
  for (j in 1:ncol(clean_loadings)) {
    top_loadings_matrix[i, j] <- clean_loadings[var_name, j]
  }
}

# 重命名列名为因子名称
colnames(top_loadings_matrix) <- factor_names

# 创建显示最重要变量的热图 - 增加宽度以确保所有文本完全显示
pdf(paste0(vis_dir, "/top_variables_heatmap.pdf"), width = 16, height = 10)
corrplot(top_loadings_matrix, is.corr = FALSE,
         method = "color",
         col = colorRampPalette(c("#4575B4", "white", "#D73027"))(100),
         tl.col = "black", 
         tl.srt = 0,  # 水平显示变量名
         tl.cex = 0.8, # 调整文本大小
         cl.align.text = "l",
         title = "Top Loading Variables for Each Factor",
         mar = c(0, 0, 2, 0),
         addCoef.col = "black",    # 添加数字标签
         number.cex = 0.9,         # 调整数字大小
         number.digits = 2)        # 显示2位小数
dev.off()

# 也保存为PNG - 增加宽度和分辨率
png(paste0(vis_dir, "/top_variables_heatmap.png"), width = 1800, height = 1000, res = 150)
corrplot(top_loadings_matrix, is.corr = FALSE,
         method = "color",
         col = colorRampPalette(c("#4575B4", "white", "#D73027"))(100),
         tl.col = "black", 
         tl.srt = 0,  # 水平显示变量名
         tl.cex = 0.8, # 调整文本大小
         cl.align.text = "l",
         title = "Top Loading Variables for Each Factor",
         mar = c(0, 0, 2, 0),
         addCoef.col = "black",    # 添加数字标签
         number.cex = 0.9,         # 调整数字大小
         number.digits = 2)        # 显示2位小数
dev.off()

# 创建一个表格，显示每个因子的顶部变量及其负荷值
top_vars_table <- data.frame(Factor = character(),
                           Variable = character(),
                           Loading = numeric(),
                           stringsAsFactors = FALSE)

for (i in 1:length(factor_names)) {
  factor_name <- factor_names[i]
  col_name <- colnames(clean_loadings)[i]
  
  # 获取该因子的所有变量负荷
  factor_loads <- clean_loadings[, i]
  # 按照绝对值大小排序并取前N个
  top_indices <- order(abs(factor_loads), decreasing = TRUE)[1:top_n_variables]
  top_vars <- rownames(clean_loadings)[top_indices]
  top_loads <- factor_loads[top_indices]
  
  # 添加到表格
  for (j in 1:length(top_vars)) {
    top_vars_table <- rbind(top_vars_table, 
                          data.frame(Factor = factor_name,
                                   Variable = top_vars[j],
                                   Loading = top_loads[j],
                                   stringsAsFactors = FALSE))
  }
}

# 保存表格
write.csv(top_vars_table, paste0(tables_dir, "/top_variables_by_factor.csv"), row.names = FALSE)

cat("\nEnhanced factor visualizations complete!\n")
cat("Results saved to", vis_dir, "\n") 