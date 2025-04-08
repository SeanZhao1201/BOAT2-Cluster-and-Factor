# BOAT2 Cluster and Factor Analysis - K-Prototype Clustering Analysis
# This script performs K-prototype clustering analysis

# 1. Load Setup and Data -----------------------------------------------------
source("R/00_setup.R")

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

# Try to load optimal k from previous analysis
optimal_k_file <- "results/tables/031_kprototype_optimal/optimal_k.csv"
if (file.exists(optimal_k_file)) {
  optimal_k_data <- read.csv(optimal_k_file)
  # Choose results from Silhouette or Elbow method as needed
  optimal_k_silhouette <- optimal_k_data$OptimalK[optimal_k_data$Method == "Silhouette"]
  optimal_k_elbow <- optimal_k_data$OptimalK[optimal_k_data$Method == "Elbow"]
  
  cat("Loaded optimal k (Silhouette) =", optimal_k_silhouette, "\n")
  cat("Loaded optimal k (Elbow) =", optimal_k_elbow, "\n")
  
  # Use the maximum k value as optimal_k
  optimal_k <- max(optimal_k_silhouette, optimal_k_elbow)
  cat("Using max value as optimal k =", optimal_k, "\n")
} else {
  # Default values if previous analysis not available
  optimal_k <- 3
  cat("Previous analysis not found. Using default k =", optimal_k, "\n")
}

# We'll analyze both k=2 and k=3 (or optimal_k if it's different)
kproto_k_values <- unique(c(2, 3, optimal_k))
cat("Will perform analysis with k values:", paste(kproto_k_values, collapse = ", "), "\n")

# 2. Define Variables --------------------------------------------------------

# Define numerical and categorical variables
numerical_org_vars <- c(
  "Org_Structure_Employees",
  "Org_Structure_Locations",
  "Org_Structure_Depts",
  "Org_Structure_Layers"
)

# All variables except PDM_Selected and PDM experience variables
exclude_vars <- c("PDM_Selected", 
                  "PDM_Experience_DBB", 
                  "PDM_Experience_DB", 
                  "PDM_Experience_PDB", 
                  "PDM_Experience_CMAR", 
                  "PDM_Experience_IPD")
all_vars <- setdiff(colnames(data), exclude_vars)

# Categorical variables (all except numerical ones)
categorical_vars <- setdiff(all_vars, numerical_org_vars)

# Check variables
cat("\nNumerical variables:", length(numerical_org_vars), "\n")
cat("Categorical variables:", length(categorical_vars), "\n")
cat("All analysis variables:", length(all_vars), "\n")

# 3. Prepare data for K-prototypes clustering -------------------------------
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

# Convert categorical variables to ordered factors
kproto_mixed_data <- kproto_mixed_data %>%
  mutate(across(all_of(categorical_vars), ~ ordered(round(.), levels = 1:5)))

# Save preprocessed data for later use
save_data(kproto_data, "032_kprototype_analysis/kproto_data.csv")

# 4. Perform K-Prototypes Clustering -----------------------------------------

# Create a list to store results
kproto_results <- list()

# Function to run k-prototypes clustering
run_kproto_analysis <- function(k_value) {
  # Set seed for reproducibility
  set.seed(123)
  
  cat("\nRunning K-Prototype clustering with k =", k_value, "...\n")
  
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
  
  return(list(
    results = cluster_results,
    centroids = centroids,
    kproto_object = kproto_result
  ))
}

# Run k-prototypes for each k value
for (k in kproto_k_values) {
  kproto_results[[paste0("k", k)]] <- run_kproto_analysis(k)
}

# 5. Create PDM Distribution Analysis ----------------------------------------

# Function to create PDM distribution plot
create_pdm_plot <- function(cluster_results, k) {
  # Print dataset PDM_Selected value distribution
  cat("\nCluster results PDM_Selected distribution:\n")
  print(table(cluster_results$PDM_Selected))
  
  # Create contingency table
  pdm_table <- table(cluster_results$PDM_Selected, cluster_results$Cluster)
  
  # 打印contingency table查看内容
  cat("\nContingency table:\n")
  print(pdm_table)
  
  # Convert to data frame for ggplot
  pdm_df <- as.data.frame(pdm_table)
  # 保留原始列名
  colnames(pdm_df) <- c("PDM_Selected", "Cluster", "Count")
  pdm_df$Cluster <- paste("Cluster", pdm_df$Cluster)
  
  # Calculate percentages
  pdm_df <- pdm_df %>%
    group_by(Cluster) %>%
    mutate(Percentage = Count / sum(Count) * 100)
  
  # Define custom order based on innovation level (most to least innovative)
  pdm_order <- c("Integrated Project Delivery (IPD)", 
                 "Progressive Design-Build", 
                 "Design-Build", 
                 "Construction Manager @ Risk", 
                 "Design-Bid-Build")
  
  # Shorter labels for display
  pdm_labels <- c(
    "Integrated Project Delivery (IPD)" = "IPD (Most Innovative)",
    "Progressive Design-Build" = "Progressive Design-Build",
    "Design-Build" = "Design-Build",
    "Construction Manager @ Risk" = "Construction Manager @ Risk",
    "Design-Bid-Build" = "Design-Bid-Build (Most Traditional)"
  )
  
  # Convert PDM_Selected to factor with custom order
  pdm_df$PDM_Selected <- factor(pdm_df$PDM_Selected, levels = pdm_order)
  
  # Define low saturation color palette reflecting innovation level
  # Blue (most innovative) to Purple (most traditional)
  custom_colors <- c(
    "Integrated Project Delivery (IPD)" = "#a6cee3",    # Light blue - most innovative
    "Progressive Design-Build" = "#b2df8a",             # Light green
    "Design-Build" = "#fbf069",                        # Light yellow
    "Construction Manager @ Risk" = "#fdbf6f",          # Light orange 
    "Design-Bid-Build" = "#cab2d6"                      # Light purple - most traditional
  )
  
  # Create plot
  p <- ggplot(pdm_df, aes(x = Cluster, y = Count, fill = PDM_Selected)) +
    geom_bar(stat = "identity", position = "fill") +
    geom_text(
      aes(
        label = sprintf("%.1f%%", Percentage),
        y = Count / sum(Count[Cluster == Cluster])
      ),
      position = position_fill(vjust = 0.5),
      size = 3,
      color = "black"
    ) +
    labs(
      title = paste("PDM Distribution by K-Prototypes Cluster (k =", k, ")"),
      x = "Cluster",
      y = "Proportion",
      fill = "Selected PDM"  # 更改图例标题
    ) +
    theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(face = "bold"),
      legend.position = "right",
      legend.title = element_text(face = "bold"),
      # 更新图例样式
      legend.background = element_rect(fill = "white", color = NA),
      # 添加边框
      panel.border = element_rect(color = "gray90", fill = NA),
      # 确保背景是纯白色
      panel.background = element_rect(fill = "white", color = NA)
    ) +
    scale_y_continuous(labels = scales::percent) +
    # 使用自定义颜色
    scale_fill_manual(
      values = custom_colors,
      labels = pdm_labels
    )
  
  return(list(
    plot = p,
    data = pdm_df
  ))
}

# Create PDM distribution plots and save data
pdm_plots <- list()
for (k in kproto_k_values) {
  result_key <- paste0("k", k)
  pdm_result <- create_pdm_plot(kproto_results[[result_key]]$results, k)
  pdm_plots[[result_key]] <- pdm_result
  
  # 美化PDM分布图
  improved_plot <- pdm_result$plot +
    theme(
      axis.title = element_text(face = "bold", size = 14),
      axis.text = element_text(size = 12),
      legend.text = element_text(size = 11),
      plot.title = element_text(size = 16, face = "bold"),
      panel.grid.major.y = element_line(color = "gray95"),
      panel.grid.minor = element_blank(),
      legend.position = "right",
      legend.box.background = element_rect(color = "gray80", fill = "white")
    )
  
  # Save PDM distribution plot
  ggsave(
    paste0("results/figures/032_kprototype_analysis/K", k, "_PDM_Distribution.pdf"),
    improved_plot,
    width = 10,
    height = 7,
    dpi = 300
  )
  
  # Save PDM distribution data
  save_data(
    pdm_result$data,
    paste0("032_kprototype_analysis/pdm_distribution_k", k, ".csv")
  )
}

# Create combined PDM distribution plot
if (length(kproto_k_values) > 1) {
  combined_pdm_plots <- lapply(kproto_k_values, function(k) {
    # Use improved chart style
    pdm_plots[[paste0("k", k)]]$plot + 
      ggtitle(paste0("K-Prototypes (k = ", k, ")")) +
      theme(
        axis.title = element_text(face = "bold", size = 14),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 11),
        plot.title = element_text(size = 16, face = "bold"),
        panel.grid.major.y = element_line(color = "gray95"),
        panel.grid.minor = element_blank(),
        legend.position = "right",
        legend.box.background = element_rect(color = "gray80", fill = "white")
      )
  })
  
  # 1. Stacked display
  combined_plot <- gridExtra::grid.arrange(
    grobs = combined_pdm_plots,
    ncol = 1
  )
  
  # Save stacked plot
  ggsave(
    "results/figures/032_kprototype_analysis/PDM_All_Stacked.pdf",
    combined_plot,
    width = 12,
    height = 7 * length(kproto_k_values),
    dpi = 300
  )
  
  # 2. Side by side display
  side_by_side_plot <- gridExtra::grid.arrange(
    grobs = combined_pdm_plots,
    ncol = length(kproto_k_values)
  )
  
  # Save side by side plot
  ggsave(
    "results/figures/032_kprototype_analysis/PDM_All_SideBySide.pdf",
    side_by_side_plot,
    width = 8 * length(kproto_k_values),
    height = 8,
    dpi = 300
  )
}

# 3. Create comparison charts - Compare PDM distribution for different clusters within each k value
for (k in kproto_k_values) {
  result_key <- paste0("k", k)
  result_df <- kproto_results[[result_key]]$results
  
  # Get all clusters
  for (cluster_num in 1:k) {
    # Filter data for specific cluster
    cluster_data <- result_df %>%
      filter(Cluster == cluster_num) 
    
    # Calculate percentage of each PDM type in this cluster
    pdm_percent <- prop.table(table(cluster_data$PDM_Selected)) * 100
    pdm_df <- data.frame(
      PDM = names(pdm_percent),
      Percentage = as.numeric(pdm_percent)
    )
    
    # Sort by innovation order
    pdm_order <- c("Integrated Project Delivery (IPD)", 
                   "Progressive Design-Build", 
                   "Design-Build", 
                   "Construction Manager @ Risk", 
                   "Design-Bid-Build")
    pdm_df$PDM <- factor(pdm_df$PDM, levels = pdm_order)
    
    # Set colors
    custom_colors <- c(
      "Integrated Project Delivery (IPD)" = "#a6cee3",    # Light blue
      "Progressive Design-Build" = "#b2df8a",             # Light green
      "Design-Build" = "#fbf069",                        # Light yellow
      "Construction Manager @ Risk" = "#fdbf6f",          # Light orange 
      "Design-Bid-Build" = "#cab2d6"                      # Light purple
    )
    
    # Short labels
    pdm_labels <- c(
      "Integrated Project Delivery (IPD)" = "IPD",
      "Progressive Design-Build" = "PDB",
      "Design-Build" = "DB",
      "Construction Manager @ Risk" = "CMAR",
      "Design-Bid-Build" = "DBB"
    )
    
    # Create bar chart
    p <- ggplot(pdm_df, aes(x = PDM, y = Percentage, fill = PDM)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = sprintf("%.1f%%", Percentage)), 
                position = position_stack(vjust = 0.5), 
                color = "black", 
                size = 4) +
      labs(
        title = paste0("PDM Distribution in Cluster ", cluster_num, " (k = ", k, ")"),
        x = "Project Delivery Method",
        y = "Percentage (%)"
      ) +
      theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(face = "bold", size = 16),
        axis.title = element_text(face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none"
      ) +
      scale_fill_manual(values = custom_colors) +
      scale_x_discrete(labels = pdm_labels)
    
    # Save chart
    ggsave(
      paste0("results/figures/032_kprototype_analysis/K", k, "_PDM_Distribution_Cluster", cluster_num, ".pdf"),
      p,
      width = 8,
      height = 6,
      dpi = 300
    )
  }
}

# 6. Cluster Profiles Analysis -----------------------------------------------

# Create separate radar charts for cluster profiles (organizational variables and ordinal variables)
for (k in kproto_k_values) {
  result_key <- paste0("k", k)
  centroids <- kproto_results[[result_key]]$centroids
  
  # Define different color palettes for different k values
  # This ensures that k=2 Cluster 1 and k=3 Cluster 1 use different colors
  if (k == 2) {
    colors <- c("#1b9e77", "#d95f02")  # Dark teal and orange for k=2
  } else if (k == 3) {
    colors <- c("#7570b3", "#e7298a", "#66a61e")  # Purple, pink, green for k=3
  } else {
    # For any other k values, use a different palette
    colors <- colorRampPalette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00"))(k)
  }
  
  # ==================== 6.1 Organizational Structure Variables Radar Chart ====================
  
  # Create radar data for organizational structure variables
  org_vars <- numerical_org_vars
  org_radar_data <- centroids %>%
    select(all_of(c("Cluster", org_vars))) %>%
    as.data.frame()
  
  # Normalize organizational variables for consistent scale (1-5 to match ordinal variables)
  org_radar_normalized <- org_radar_data
  
  for (var in org_vars) {
    # Get min and max of the variable
    var_min <- min(kproto_data[[var]])
    var_max <- max(kproto_data[[var]])
    
    # Min-max normalization to scale 1-5
    org_radar_normalized[[var]] <- 1 + 4 * (org_radar_data[[var]] - var_min) / (var_max - var_min)
  }
  
  # Prepare radar data
  radar_org_data <- org_radar_normalized %>%
    select(-Cluster) %>%
    as.data.frame()
  
  # Add row names for the radar chart
  rownames(radar_org_data) <- paste("Cluster", centroids$Cluster)
  
  # Add max and min rows required by fmsb
  radar_org_data <- rbind(
    rep(5, ncol(radar_org_data)),  # max values
    rep(1, ncol(radar_org_data)),  # min values
    radar_org_data
  )
  
  # Set row names for max and min
  rownames(radar_org_data)[1:2] <- c("max", "min")
  
  # Create organizational radar chart
  pdf(paste0("results/figures/032_kprototype_analysis/K", k, "_OrgStructure_All.pdf"), 
      width = 10, height = 8)
  
  # Plot radar chart with improved aesthetics
  radarchart(
    radar_org_data,
    pcol = colors,
    pfcol = adjustcolor(colors, alpha.f = 0.2), 
    plwd = 3,                                  # Increase line width 
    cglcol = "gray80",                           
    cglty = 1,
    axislabcol = "gray30",
    vlcex = 1.2,                               # Increase variable label font size
    title = paste("Organizational Structure Variables by Cluster (k =", k, ")"),
    caxislabels = c(1, 2, 3, 4, 5),            # Show scale labels
    calcex = 1.0                                # Increase scale label font size
  )
  
  # Add a legend with improved styling
  legend(
    "bottomright",
    legend = paste("Cluster", 1:k),
    col = colors,
    lwd = 3,
    pch = 20,
    pt.cex = 1.5,                                
    cex = 1.4,                                   # Increase legend font size
    bty = "n",                                   
    text.col = "black",
    bg = adjustcolor("white", alpha.f = 0.7)     
  )
  
  dev.off()
  
  # ==================== 6.2 Ordinal Variables Radar Chart ====================
  
  # Create radar data for ordinal variables
  ordinal_vars <- categorical_vars
  
  # Split ordinal variables into groups to avoid overcrowded radar charts
  ordinal_group1 <- ordinal_vars[1:min(11, length(ordinal_vars))]
  ordinal_group2 <- ordinal_vars[min(12, length(ordinal_vars)):length(ordinal_vars)]
  
  # Function to create radar chart for a group of ordinal variables
  create_ordinal_radar <- function(vars, group_name) {
    ordinal_radar_data <- centroids %>%
      select(all_of(c("Cluster", vars))) %>%
      as.data.frame()
    
    # Prepare radar data
    radar_ord_data <- ordinal_radar_data %>%
      select(-Cluster) %>%
      as.data.frame()
    
    # Add row names for the radar chart
    rownames(radar_ord_data) <- paste("Cluster", centroids$Cluster)
    
    # Add max and min rows required by fmsb
    radar_ord_data <- rbind(
      rep(5, ncol(radar_ord_data)),  # max values
      rep(1, ncol(radar_ord_data)),  # min values
      radar_ord_data
    )
    
    # Set row names for max and min
    rownames(radar_ord_data)[1:2] <- c("max", "min")
    
    # Create ordinal radar chart
    pdf(paste0("results/figures/032_kprototype_analysis/K", k, "_", group_name, "_All.pdf"), 
        width = 12, height = 10)
    
    # Plot radar chart
    radarchart(
      radar_ord_data,
      pcol = colors,
      pfcol = adjustcolor(colors, alpha.f = 0.2), 
      plwd = 3,                                 # Increase line width 
      cglcol = "gray80",                          
      cglty = 1,
      axislabcol = "gray30",
      vlcex = 1.0,                              # Increase variable label font size 
      title = paste("Ordinal Variables (", group_name, ") by Cluster (k =", k, ")"),
      caxislabels = c(1, 2, 3, 4, 5),            # Show scale labels
      calcex = 1.0                                # Increase scale label font size
    )
    
    # Add a legend
    legend(
      "bottomright",
      legend = paste("Cluster", 1:k),
      col = colors,
      lwd = 3,
      pch = 20,
      pt.cex = 1.5,                               
      cex = 1.4,                                  # Increase legend font size
      bty = "n",                                  
      text.col = "black",
      bg = adjustcolor("white", alpha.f = 0.7)    
    )
    
    dev.off()
  }
  
  # Create radar charts for ordinal variable groups
  create_ordinal_radar(ordinal_group1, "Group1")
  
  if(length(ordinal_group2) > 0) {
    create_ordinal_radar(ordinal_group2, "Group2")
  }
  
  # ==================== 6.3 Individual Cluster Radar Charts ====================

  # Create individual radar charts for each cluster
  for (cluster_num in 1:k) {
    # ===== Organizational Structure Variables Individual Radar Chart =====
    # Use radar_org_data that's already available
    cluster_org_data <- radar_org_data[c(1, 2, 2 + cluster_num), ]
    
    # Create individual radar chart
    pdf(paste0("results/figures/032_kprototype_analysis/K", k, "_Cluster", cluster_num, "_OrgStructure.pdf"), 
        width = 9, height = 7)
    
    # Use only the color for this cluster
    cluster_color <- colors[cluster_num]
    
    # Draw radar chart
    radarchart(
      cluster_org_data,
      pcol = cluster_color,
      pfcol = adjustcolor(cluster_color, alpha.f = 0.2),
      plwd = 3,
      cglcol = "gray80",
      cglty = 1,
      axislabcol = "gray30",
      vlcex = 1.2,
      title = paste("Organizational Structure Variables - Cluster", cluster_num, "(k =", k, ")"),
      caxislabels = c(1, 2, 3, 4, 5),
      calcex = 1.0
    )
    
    # Add legend
    legend(
      "bottomright",
      legend = paste("Cluster", cluster_num),
      col = cluster_color,
      lwd = 3,
      pch = 20,
      pt.cex = 1.5,
      cex = 1.4,
      bty = "n",
      text.col = "black",
      bg = adjustcolor("white", alpha.f = 0.7)
    )
    
    dev.off()
    
    # ===== Create individual radar charts for each group of ordinal variables =====
    # Initialize the comprehensive radar data here rather than relying on previous code
    all_vars_data <- centroids %>%
      select(-Cluster) %>%
      as.data.frame()
    
    # Add row names
    rownames(all_vars_data) <- paste("Cluster", centroids$Cluster)
    
    # Add max and min rows
    all_vars_data <- rbind(
      rep(5, ncol(all_vars_data)),  # max values
      rep(1, ncol(all_vars_data)),  # min values
      all_vars_data
    )
    
    # Set names for max and min rows
    rownames(all_vars_data)[1:2] <- c("max", "min")
    
    # Group 1 ordinal variables radar chart
    cluster_ord_data1 <- all_vars_data[c(1, 2, 2+cluster_num), colnames(all_vars_data) %in% ordinal_group1]
    
    # Only keep needed columns
    if (ncol(cluster_ord_data1) > 0) {
      pdf(paste0("results/figures/032_kprototype_analysis/K", k, "_Group1_Cluster", cluster_num, ".pdf"), 
          width = 10, height = 8)
      
      # Draw Group1 radar chart
      radarchart(
        cluster_ord_data1,
        pcol = cluster_color,
        pfcol = adjustcolor(cluster_color, alpha.f = 0.2),
        plwd = 3,
        cglcol = "gray80",
        cglty = 1,
        axislabcol = "gray30",
        vlcex = 1.0,
        title = paste("Ordinal Variables (Group 1) - Cluster", cluster_num, "(k =", k, ")"),
        caxislabels = c(1, 2, 3, 4, 5),
        calcex = 1.0
      )
      
      # Add legend
      legend(
        "bottomright",
        legend = paste("Cluster", cluster_num),
        col = cluster_color,
        lwd = 3,
        pch = 20,
        pt.cex = 1.5,
        cex = 1.4,
        bty = "n",
        text.col = "black",
        bg = adjustcolor("white", alpha.f = 0.7)
      )
      
      dev.off()
    }
    
    # Group 2 (if exists) ordinal variables radar chart
    if (length(ordinal_group2) > 0) {
      cluster_ord_data2 <- all_vars_data[c(1, 2, 2+cluster_num), colnames(all_vars_data) %in% ordinal_group2]
      
      # Only keep needed columns
      if (ncol(cluster_ord_data2) > 0) {
        pdf(paste0("results/figures/032_kprototype_analysis/K", k, "_Group2_Cluster", cluster_num, ".pdf"), 
            width = 10, height = 8)
        
        # Draw Group2 radar chart
        radarchart(
          cluster_ord_data2,
          pcol = cluster_color,
          pfcol = adjustcolor(cluster_color, alpha.f = 0.2),
          plwd = 3,
          cglcol = "gray80",
          cglty = 1,
          axislabcol = "gray30",
          vlcex = 1.0,
          title = paste("Ordinal Variables (Group 2) - Cluster", cluster_num, "(k =", k, ")"),
          caxislabels = c(1, 2, 3, 4, 5),
          calcex = 1.0
        )
        
        # Add legend
        legend(
          "bottomright",
          legend = paste("Cluster", cluster_num),
          col = cluster_color,
          lwd = 3,
          pch = 20,
          pt.cex = 1.5,
          cex = 1.4,
          bty = "n",
          text.col = "black",
          bg = adjustcolor("white", alpha.f = 0.7)
        )
        
        dev.off()
      }
    }
  }
  
  # ==================== 6.3 Combined Radar Chart (keeps original functionality) ====================
  
  # Create radar chart data for all variables (backward compatibility)
  radar_data <- centroids %>%
    select(-Cluster) %>%
    as.data.frame()
  
  # Add row names for the radar chart
  rownames(radar_data) <- paste("Cluster", centroids$Cluster)
  
  # Add max and min rows required by fmsb
  radar_data <- rbind(
    rep(5, ncol(radar_data)),  # max values
    rep(1, ncol(radar_data)),  # min values
    radar_data
  )
  
  # Set row names for max and min
  rownames(radar_data)[1:2] <- c("max", "min")
  
  # Create radar chart
  pdf(paste0("results/figures/032_kprototype_analysis/K", k, "_Radar_All.pdf"), 
      width = 12, height = 12)
  
  # Plot radar chart with improved aesthetics
  radarchart(
    radar_data,
    pcol = colors,
    pfcol = adjustcolor(colors, alpha.f = 0.2), 
    plwd = 3,                                 # Thicker lines 
    cglcol = "gray80",                         
    cglty = 1,
    axislabcol = "gray30",
    vlcex = 1.0,                               # Increase variable label font size
    title = paste("K-Prototypes Cluster Profiles (k =", k, ")"),
    caxislabels = c(1, 2, 3, 4, 5),            # Show scale labels  
    calcex = 1.0                                # Increase scale label font size
  )
  
  # Add a legend with improved styling
  legend(
    "bottomright",
    legend = paste("Cluster", 1:k),
    col = colors,
    lwd = 3,
    pch = 20,
    pt.cex = 1.5,                              
    cex = 1.4,                                 # Increase legend font size 
    bty = "n",                                 
    text.col = "black",
    bg = adjustcolor("white", alpha.f = 0.7)  
  )
  
  dev.off()
  
  # Also save cluster characteristics summary table
  cluster_char <- kproto_results[[result_key]]$results %>%
    group_by(Cluster) %>%
    summarise(across(all_of(c(numerical_org_vars, categorical_vars)), mean))
  
  save_data(
    cluster_char,
    paste0("032_kprototype_analysis/cluster_characteristics_k", k, ".csv")
  )
}

# 7. Statistical Validation --------------------------------------------------

# Function to calculate silhouette scores for cluster validation
calculate_silhouette <- function(data, clusters) {
  # Calculate Gower distance matrix
  gower_dist <- daisy(data, metric = "gower")
  
  # Calculate silhouette
  sil <- silhouette(clusters, gower_dist)
  
  # Create silhouette data frame
  sil_df <- data.frame(
    ID = 1:length(clusters),
    Cluster = clusters,
    Silhouette = sil[, "sil_width"]
  )
  
  # Calculate average silhouette by cluster
  avg_sil_by_cluster <- sil_df %>%
    group_by(Cluster) %>%
    summarise(
      AvgSilhouette = mean(Silhouette),
      Count = n()
    )
  
  return(list(
    silhouette_data = sil_df,
    silhouette_by_cluster = avg_sil_by_cluster,
    avg_silhouette = mean(sil[, "sil_width"])
  ))
}

# Calculate and save silhouette scores for each clustering
silhouette_results <- data.frame(
  k = numeric(0),
  avg_silhouette = numeric(0)
)

for (k in kproto_k_values) {
  result_key <- paste0("k", k)
  clusters <- kproto_results[[result_key]]$results$Cluster
  
  # Calculate silhouette
  sil_result <- calculate_silhouette(kproto_mixed_data, clusters)
  
  # Save silhouette data
  save_data(
    sil_result$silhouette_data,
    paste0("032_kprototype_analysis/silhouette_data_k", k, ".csv")
  )
  
  # Save silhouette by cluster
  save_data(
    sil_result$silhouette_by_cluster,
    paste0("032_kprototype_analysis/silhouette_by_cluster_k", k, ".csv")
  )
  
  # Add to overall results
  silhouette_results <- rbind(
    silhouette_results,
    data.frame(
      k = k,
      avg_silhouette = sil_result$avg_silhouette
    )
  )
}

# Save overall silhouette results
save_data(silhouette_results, "032_kprototype_analysis/silhouette_summary.csv")

# Print completion message
cat("\nK-Prototype Clustering Analysis complete!\n")
cat("Visualizations saved to results/figures/032_kprototype_analysis/\n")
cat("Results data saved to results/tables/032_kprototype_analysis/\n")
cat("\nSilhouette Summary:\n")
print(silhouette_results) 