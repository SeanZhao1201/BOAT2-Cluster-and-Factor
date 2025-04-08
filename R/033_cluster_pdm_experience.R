# BOAT2 Cluster and Factor Analysis - Cluster vs PDM Experience Visualization
# This script visualizes the relationship between k-prototype clusters and PDM experience

# 1. Load Setup and Data -----------------------------------------------------
source("R/00_setup.R")

# Create subdirectories for results if they don't exist
dirs <- c(
  "results/figures/033_cluster_pdm_experience",
  "results/tables/033_cluster_pdm_experience"
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

# Load cluster results from previous analysis for k=2 and k=3
k_values <- c(2, 3)
cluster_data <- list()

for (k in k_values) {
  cluster_file <- paste0("results/tables/032_kprototype_analysis/kproto_clusters_k", k, ".csv")
  if (file.exists(cluster_file)) {
    cluster_data[[paste0("k", k)]] <- read.csv(cluster_file)
    cat("Loaded cluster data for k =", k, "\n")
  } else {
    cat("Cluster file not found for k =", k, "\n")
  }
}

# 2. Define PDM Experience Variables -----------------------------------------
pdm_experience_vars <- c(
  "PDM_Experience_DBB",
  "PDM_Experience_DB",
  "PDM_Experience_PDB", 
  "PDM_Experience_CMAR",
  "PDM_Experience_IPD"
)

# Create more intuitive labels for visualization
pdm_experience_labels <- c(
  "PDM_Experience_DBB" = "Design-Bid-Build (Traditional)",
  "PDM_Experience_DB" = "Design-Build",
  "PDM_Experience_PDB" = "Progressive Design-Build", 
  "PDM_Experience_CMAR" = "Construction Manager @ Risk",
  "PDM_Experience_IPD" = "Integrated Project Delivery (Innovative)"
)

# 3. Analysis Functions -----------------------------------------------------

# Function to prepare PDM experience data grouped by cluster
prepare_pdm_experience_data <- function(cluster_results, pdm_vars) {
  # Verify PDM variables exist in the dataset
  missing_vars <- setdiff(pdm_vars, colnames(cluster_results))
  if (length(missing_vars) > 0) {
    stop("The following PDM experience variables are missing from cluster results: ", 
         paste(missing_vars, collapse=", "), 
         "\nMake sure the 032_kprototype_analysis.R script has been run with the updated code.")
  }
  
  # Calculate mean experience values by cluster
  pdm_experience_by_cluster <- cluster_results %>%
    group_by(Cluster) %>%
    summarise(across(all_of(pdm_vars), 
                     list(Mean = mean, SD = sd, Median = median),
                     .names = "{.col}_{.fn}"),
              Count = n()) %>%
    mutate(Cluster = paste("Cluster", Cluster))
  
  # Reshape data for easier plotting (long format)
  pdm_exp_long <- pdm_experience_by_cluster %>%
    pivot_longer(
      cols = contains("_Mean"),
      names_to = "PDM_Type",
      values_to = "Mean_Experience"
    ) %>%
    mutate(PDM_Type = gsub("_Mean", "", PDM_Type)) %>%
    # Add SD values
    left_join(
      pdm_experience_by_cluster %>%
        pivot_longer(
          cols = contains("_SD"),
          names_to = "PDM_Type",
          values_to = "SD_Experience"
        ) %>%
        mutate(PDM_Type = gsub("_SD", "", PDM_Type)) %>%
        select(Cluster, PDM_Type, SD_Experience),
      by = c("Cluster", "PDM_Type")
    )
  
  # Order PDM types by innovation level
  pdm_exp_long$PDM_Type <- factor(
    pdm_exp_long$PDM_Type,
    levels = c("PDM_Experience_DBB", "PDM_Experience_CMAR", 
               "PDM_Experience_DB", "PDM_Experience_PDB", "PDM_Experience_IPD")
  )
  
  return(list(
    summary = pdm_experience_by_cluster,
    long_format = pdm_exp_long
  ))
}

# 4. Process data and create visualizations for each k value ----------------
for (k in k_values) {
  data_key <- paste0("k", k)
  
  if (data_key %in% names(cluster_data)) {
    # Prepare data
    pdm_exp_data <- prepare_pdm_experience_data(
      cluster_data[[data_key]], 
      pdm_experience_vars
    )
    
    # Save summary data
    save_data(
      pdm_exp_data$summary,
      paste0("033_cluster_pdm_experience/pdm_experience_summary_k", k, ".csv")
    )
    
    # Save long format data for plots
    save_data(
      pdm_exp_data$long_format,
      paste0("033_cluster_pdm_experience/pdm_experience_plot_data_k", k, ".csv")
    )
    
    # Define cluster colors
    if (k == 2) {
      colors <- c("#1b9e77", "#d95f02")  # Dark teal and orange for k=2
    } else if (k == 3) {
      colors <- c("#7570b3", "#e7298a", "#66a61e")  # Purple, pink, green for k=3
    } else {
      colors <- colorRampPalette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00"))(k)
    }
    
    # Create cluster names
    cluster_names <- paste("Cluster", 1:k)
    
    # ================== 4.1 Bar Chart (Mean Experience by Cluster) ==================
    
    # Create bar chart of mean experience levels by cluster (without error bars)
    bar_plot <- ggplot(pdm_exp_data$long_format, 
                      aes(x = PDM_Type, y = Mean_Experience, fill = Cluster)) +
      geom_bar(stat = "identity", position = position_dodge(width = 0.9), alpha = 0.8) +
      # Add text labels showing the mean values
      geom_text(
        aes(label = sprintf("%.1f", Mean_Experience)),
        position = position_dodge(width = 0.9),
        vjust = -0.5,
        size = 3,
        fontface = "bold"
      ) +
      labs(
        title = paste("Mean PDM Experience by Cluster (k =", k, ")"),
        x = "Project Delivery Method", 
        y = "Mean Experience Level (1-5)",
        fill = "Cluster"
      ) +
      scale_fill_manual(values = colors) +
      scale_x_discrete(labels = function(x) gsub("PDM_Experience_", "", x)) +
      theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(face = "bold", size = 16),
        axis.title = element_text(face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 11),
        legend.position = "right",
        panel.grid.major.y = element_line(color = "gray95"),
        panel.grid.minor = element_blank(),
        legend.background = element_rect(fill = "white", color = "gray90")
      ) +
      scale_y_continuous(limits = c(0, 5.5), breaks = 1:5)  # Increased upper limit to accommodate text
    
    # Save the bar chart
    ggsave(
      paste0("results/figures/033_cluster_pdm_experience/K", k, "_PDM_Experience_Bar.pdf"),
      bar_plot,
      width = 10,
      height = 7,
      dpi = 300
    )
    
    # ================== 4.2 Radar Chart ==================
    
    # Convert to wide format for radar chart
    radar_data <- pdm_exp_data$summary %>%
      select(Cluster, contains("_Mean")) %>%
      mutate(across(contains("_Mean"), ~ ., .names = "{gsub('_Mean', '', .col)}")) %>%
      select(Cluster, all_of(pdm_experience_vars)) %>%
      as.data.frame()
    
    # Set row names for radar chart
    rownames(radar_data) <- radar_data$Cluster
    
    # Remove Cluster column
    radar_data$Cluster <- NULL
    
    # Add max and min rows required by fmsb
    radar_data <- rbind(
      rep(5, ncol(radar_data)),  # max values
      rep(1, ncol(radar_data)),  # min values
      radar_data
    )
    
    # Set row names for max and min
    rownames(radar_data)[1:2] <- c("max", "min")
    
    # Create radar chart
    pdf(paste0("results/figures/033_cluster_pdm_experience/K", k, "_PDM_Experience_Radar.pdf"), 
        width = 10, height = 8)
    
    # Plot radar chart with improved aesthetics
    radarchart(
      radar_data,
      pcol = colors,
      pfcol = adjustcolor(colors, alpha.f = 0.2), 
      plwd = 3,
      cglcol = "gray80",
      cglty = 1,
      axislabcol = "gray30",
      vlcex = 1.2,
      title = paste("PDM Experience by Cluster (k =", k, ")"),
      caxislabels = c(1, 2, 3, 4, 5),
      calcex = 1.0
    )
    
    # Add a legend with improved styling
    legend(
      "bottomright",
      legend = cluster_names,
      col = colors,
      lwd = 3,
      pch = 20,
      pt.cex = 1.5,
      cex = 1.4,
      bty = "n",
      text.col = "black",
      bg = adjustcolor("white", alpha.f = 0.7)
    )
    
    dev.off()
    
    # ================== 4.3 Individual Cluster Radar Charts ==================
    
    # Create individual radar charts for each cluster
    for (cluster_num in 1:k) {
      cluster_name <- paste("Cluster", cluster_num)
      
      # Extract data for this cluster
      cluster_radar_data <- radar_data[c(1, 2, 2 + which(rownames(radar_data)[-c(1, 2)] == cluster_name)), ]
      
      # Create individual radar chart
      pdf(paste0("results/figures/033_cluster_pdm_experience/K", k, "_", cluster_name, "_PDM_Experience_Radar.pdf"), 
          width = 9, height = 7)
      
      # Use only the color for this cluster
      cluster_color <- colors[cluster_num]
      
      # Draw radar chart
      radarchart(
        cluster_radar_data,
        pcol = cluster_color,
        pfcol = adjustcolor(cluster_color, alpha.f = 0.2),
        plwd = 3,
        cglcol = "gray80",
        cglty = 1,
        axislabcol = "gray30",
        vlcex = 1.2,
        title = paste("PDM Experience -", cluster_name, "(k =", k, ")"),
        caxislabels = c(1, 2, 3, 4, 5),
        calcex = 1.0
      )
      
      # Add legend
      legend(
        "bottomright",
        legend = cluster_name,
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
    
    # ================== 4.4 Heatmap ==================
    
    # Extract mean values for heatmap
    heatmap_data <- pdm_exp_data$long_format %>%
      select(Cluster, PDM_Type, Mean_Experience)
    
    # Create a matrix for the heatmap
    heatmap_matrix <- heatmap_data %>%
      pivot_wider(
        names_from = PDM_Type,
        values_from = Mean_Experience
      ) %>%
      column_to_rownames("Cluster") %>%
      as.matrix()
    
    # Create shorter and cleaner labels for heatmap
    colnames(heatmap_matrix) <- c("DBB", "CMAR", "DB", "PDB", "IPD")
    
    # Create heatmap
    pdf(paste0("results/figures/033_cluster_pdm_experience/K", k, "_PDM_Experience_Heatmap.pdf"), 
        width = 9, height = 6)
    
    # Create color palette ranging from light to dark blue
    heatmap_colors <- colorRampPalette(c("#f7fbff", "#deebf7", "#c6dbef", "#9ecae1", "#6baed6", "#4292c6", "#2171b5", "#084594"))(100)
    
    # Create a function to add text to the heatmap cells
    add_cell_text <- function(j, i, x, y, width, height, fill) {
      text(x, y, sprintf("%.2f", heatmap_matrix[i, j]), cex = 1.2, col = "black", font = 2)
    }
    
    # Draw heatmap with improved aesthetics
    heatmap(
      heatmap_matrix,
      Rowv = NA,  # No clustering of rows
      Colv = NA,  # No clustering of columns
      col = heatmap_colors,
      scale = "none",
      margins = c(8, 8),
      main = paste("PDM Experience Heatmap (k =", k, ")"),
      cexRow = 1.2,
      cexCol = 1.2,
      add.expr = {
        # This adds the cell values to the heatmap
        for (i in 1:nrow(heatmap_matrix)) {
          for (j in 1:ncol(heatmap_matrix)) {
            text(j, i, sprintf("%.1f", heatmap_matrix[i, j]), cex = 1.2)
          }
        }
      }
    )
    
    # Add a legend/color scale
    legend_breaks <- seq(1, 5, length.out = 5)
    legend_labels <- as.character(legend_breaks)
    legend("topright", 
           legend = legend_labels, 
           fill = heatmap_colors[seq(1, length(heatmap_colors), length.out = length(legend_breaks))],
           title = "Experience Level",
           cex = 1.0,
           bty = "n")
    
    dev.off()
    
    # Also create an alternative heatmap using ggplot2 for better text display
    ggplot_heatmap <- ggplot(heatmap_data, 
                           aes(x = PDM_Type, y = Cluster, fill = Mean_Experience)) +
      geom_tile(color = "white", linewidth = 0.5) +
      geom_text(aes(label = sprintf("%.1f", Mean_Experience)), 
                color = "black", size = 4, fontface = "bold") +
      scale_fill_gradientn(colors = heatmap_colors, limits = c(1, 5), name = "Experience\nLevel") +
      scale_x_discrete(labels = function(x) gsub("PDM_Experience_", "", x)) +
      labs(
        title = paste("PDM Experience Heatmap (k =", k, ")"),
        x = "Project Delivery Method",
        y = "Cluster"
      ) +
      theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(face = "bold", size = 16),
        axis.title = element_text(face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
        axis.text.y = element_text(size = 12),
        panel.grid = element_blank()
      )
    
    # Save the ggplot heatmap
    ggsave(
      paste0("results/figures/033_cluster_pdm_experience/K", k, "_PDM_Experience_GGHeatmap.pdf"),
      ggplot_heatmap,
      width = 9,
      height = 6,
      dpi = 300
    )
    
    # ================== 4.5 Experience Profiles for PDM_Selected ==================
    
    # Create summary of PDM experience by selected PDM
    pdm_selected_experience <- cluster_data[[data_key]] %>%
      mutate(Selected_PDM = PDM_Selected) %>%
      group_by(Selected_PDM) %>%
      summarise(across(all_of(pdm_experience_vars), 
                       list(Mean = mean, SD = sd),
                       .names = "{.col}_{.fn}"),
                Count = n())
    
    # Save summary by selected PDM
    save_data(
      pdm_selected_experience,
      paste0("033_cluster_pdm_experience/pdm_selected_experience_k", k, ".csv")
    )
    
    # Create long format for plotting
    pdm_selected_long <- pdm_selected_experience %>%
      pivot_longer(
        cols = contains("_Mean"),
        names_to = "PDM_Type",
        values_to = "Mean_Experience"
      ) %>%
      mutate(PDM_Type = gsub("_Mean", "", PDM_Type)) %>%
      # Add SD values
      left_join(
        pdm_selected_experience %>%
          pivot_longer(
            cols = contains("_SD"),
            names_to = "PDM_Type",
            values_to = "SD_Experience"
          ) %>%
          mutate(PDM_Type = gsub("_SD", "", PDM_Type)) %>%
          select(Selected_PDM, PDM_Type, SD_Experience),
        by = c("Selected_PDM", "PDM_Type")
      )
    
    # Order PDM types by innovation level
    pdm_selected_long$PDM_Type <- factor(
      pdm_selected_long$PDM_Type,
      levels = c("PDM_Experience_DBB", "PDM_Experience_CMAR", 
                 "PDM_Experience_DB", "PDM_Experience_PDB", "PDM_Experience_IPD")
    )
    
    # Define PDM selection colors (matching the ones used previously)
    pdm_selection_colors <- c(
      "Integrated Project Delivery (IPD)" = "#a6cee3",    # Light blue
      "Progressive Design-Build" = "#b2df8a",             # Light green
      "Design-Build" = "#fbf069",                        # Light yellow
      "Construction Manager @ Risk" = "#fdbf6f",          # Light orange 
      "Design-Bid-Build" = "#cab2d6"                      # Light purple
    )
    
    # Create bar chart of mean experience levels by selected PDM
    selected_pdm_plot <- ggplot(pdm_selected_long, 
                               aes(x = PDM_Type, y = Mean_Experience, fill = Selected_PDM)) +
      geom_bar(stat = "identity", position = position_dodge(width = 0.9), alpha = 0.8) +
      geom_errorbar(
        aes(ymin = Mean_Experience - SD_Experience, 
            ymax = Mean_Experience + SD_Experience),
        position = position_dodge(width = 0.9),
        width = 0.25
      ) +
      labs(
        title = paste("Mean PDM Experience by Selected PDM (k =", k, ")"),
        x = "Project Delivery Method Experience", 
        y = "Mean Experience Level (1-5)",
        fill = "Selected PDM"
      ) +
      scale_fill_manual(values = pdm_selection_colors) +
      scale_x_discrete(labels = function(x) gsub("PDM_Experience_", "", x)) +
      theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(face = "bold", size = 16),
        axis.title = element_text(face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 11),
        legend.position = "right",
        panel.grid.major.y = element_line(color = "gray95"),
        panel.grid.minor = element_blank(),
        legend.background = element_rect(fill = "white", color = "gray90")
      ) +
      scale_y_continuous(limits = c(0, 5), breaks = 1:5)
    
    # Save the selected PDM bar chart
    ggsave(
      paste0("results/figures/033_cluster_pdm_experience/K", k, "_Selected_PDM_Experience_Bar.pdf"),
      selected_pdm_plot,
      width = 12,
      height = 7,
      dpi = 300
    )
    
    # ================== 4.6 Cluster by PDM Experience Cross Analysis ==================
    
    # Create summary data that shows distribution of experience levels in each cluster
    cluster_exp_distribution <- NULL
    
    for (pdm_exp_var in pdm_experience_vars) {
      # Extract just this experience variable and cluster
      temp_df <- cluster_data[[data_key]] %>%
        select(Cluster, all_of(pdm_exp_var)) %>%
        # Convert to factors for distribution analysis
        mutate(Experience_Level = factor(!!sym(pdm_exp_var), levels = 1:5))
      
      # Calculate distribution
      exp_dist <- temp_df %>%
        group_by(Cluster, Experience_Level) %>%
        summarise(Count = n(), .groups = "drop") %>%
        # Calculate percentage
        group_by(Cluster) %>%
        mutate(
          Percentage = Count / sum(Count) * 100,
          PDM_Type = pdm_exp_var
        )
      
      # Combine data
      cluster_exp_distribution <- bind_rows(cluster_exp_distribution, exp_dist)
    }
    
    # Save the distribution data
    save_data(
      cluster_exp_distribution,
      paste0("033_cluster_pdm_experience/cluster_experience_distribution_k", k, ".csv")
    )
    
    # Create stacked bar charts for experience distribution by cluster
    for (pdm_exp_var in pdm_experience_vars) {
      # Filter data for this PDM type
      pdm_dist_data <- cluster_exp_distribution %>%
        filter(PDM_Type == pdm_exp_var)
      
      # Create plot
      exp_dist_plot <- ggplot(pdm_dist_data, 
                             aes(x = Cluster, y = Percentage, fill = Experience_Level)) +
        geom_bar(stat = "identity", position = "stack") +
        geom_text(
          aes(label = sprintf("%.1f%%", Percentage)),
          position = position_stack(vjust = 0.5),
          size = 3.5,
          color = "black"
        ) +
        labs(
          title = paste("Distribution of", gsub("PDM_Experience_", "", pdm_exp_var), "Experience by Cluster"),
          x = "Cluster",
          y = "Percentage",
          fill = "Experience Level"
        ) +
        scale_fill_brewer(palette = "Blues", direction = 1) +
        theme_minimal(base_size = 14) +
        theme(
          plot.title = element_text(face = "bold", size = 16),
          axis.title = element_text(face = "bold"),
          legend.position = "right",
          panel.grid.major.y = element_line(color = "gray95"),
          panel.grid.minor = element_blank(),
          legend.background = element_rect(fill = "white", color = "gray90")
        )
      
      # Save the plot
      ggsave(
        paste0("results/figures/033_cluster_pdm_experience/K", k, "_", 
               gsub("PDM_Experience_", "", pdm_exp_var), "_Distribution.pdf"),
        exp_dist_plot,
        width = 9,
        height = 6,
        dpi = 300
      )
    }
  }
}

# 5. Create joint analysis comparing k=2 and k=3 ----------------------------

# Create a comparative analysis if both k=2 and k=3 data are available
if (all(c("k2", "k3") %in% names(cluster_data))) {
  # Extract the PDM experience data for both k values
  k2_exp_data <- prepare_pdm_experience_data(cluster_data[["k2"]], pdm_experience_vars)$long_format %>%
    mutate(k_value = "k = 2")
  
  k3_exp_data <- prepare_pdm_experience_data(cluster_data[["k3"]], pdm_experience_vars)$long_format %>%
    mutate(k_value = "k = 3")
  
  # Combine both datasets
  combined_exp_data <- bind_rows(k2_exp_data, k3_exp_data)
  
  # Save the combined data
  save_data(
    combined_exp_data,
    "033_cluster_pdm_experience/combined_pdm_experience.csv"
  )
  
  # Create a combined visualization
  # This plot compares all clusters side by side for each PDM experience type
  for (pdm_exp_var in pdm_experience_vars) {
    exp_data <- combined_exp_data %>%
      filter(PDM_Type == pdm_exp_var)
    
    # Create faceted bar plot
    pdm_comparison_plot <- ggplot(exp_data, 
                                 aes(x = Cluster, y = Mean_Experience, fill = k_value)) +
      geom_bar(stat = "identity", position = position_dodge(width = 0.9), alpha = 0.8) +
      geom_errorbar(
        aes(ymin = Mean_Experience - SD_Experience, 
            ymax = Mean_Experience + SD_Experience),
        position = position_dodge(width = 0.9),
        width = 0.25
      ) +
      labs(
        title = paste("Comparison of", gsub("PDM_Experience_", "", pdm_exp_var), "Experience by Cluster"),
        x = "Cluster", 
        y = "Mean Experience Level (1-5)",
        fill = "Clustering"
      ) +
      scale_fill_manual(values = c("k = 2" = "#1f78b4", "k = 3" = "#33a02c")) +
      theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(face = "bold", size = 16),
        axis.title = element_text(face = "bold"),
        legend.position = "right",
        panel.grid.major.y = element_line(color = "gray95"),
        panel.grid.minor = element_blank(),
        legend.background = element_rect(fill = "white", color = "gray90")
      ) +
      scale_y_continuous(limits = c(0, 5), breaks = 1:5)
    
    # Save the comparison plot
    ggsave(
      paste0("results/figures/033_cluster_pdm_experience/", 
             gsub("PDM_Experience_", "", pdm_exp_var), "_Comparison.pdf"),
      pdm_comparison_plot,
      width = 10,
      height = 6,
      dpi = 300
    )
  }
}

# Print completion message
cat("\nCluster vs PDM Experience Analysis complete!\n")
cat("Visualizations saved to results/figures/033_cluster_pdm_experience/\n")
cat("Results data saved to results/tables/033_cluster_pdm_experience/\n") 