# BOAT2 Cluster and Factor Analysis - Cluster vs PDM Experience Visualization (k=4 and k=5)
# This script visualizes the relationship between k-prototype clusters and PDM experience
# Specifically focusing on k=4 and k=5 clusters

# 1. Load Setup and Data -----------------------------------------------------
source("R/000_setup.R")

# Create subdirectories for results if they don't exist
dirs <- c(
  "results/figures/033.1_cluster_pdm_experience_k45",
  "results/tables/033.1_cluster_pdm_experience_k45"
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

# Load cluster results from previous analysis for k=4 and k=5
k_values <- c(4, 5)
cluster_data <- list()

# Load k=4 results from 032
cluster_file_k4 <- "results/tables/032_kprototype_analysis/kproto_clusters_k4.csv"
if (file.exists(cluster_file_k4)) {
  cluster_data[["k4"]] <- read.csv(cluster_file_k4)
  cat("Loaded cluster data for k = 4 from 032_kprototype_analysis\n")
} else {
  cat("Cluster file not found for k = 4\n")
}

# Load k=5 results from 032_A
cluster_file_k5 <- "results/tables/032_A_kprototype_analysis/kproto_clusters_k5.csv"
if (file.exists(cluster_file_k5)) {
  cluster_data[["k5"]] <- read.csv(cluster_file_k5)
  cat("Loaded cluster data for k = 5 from 032_A_kprototype_analysis\n")
} else {
  cat("Cluster file not found for k = 5\n")
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

# Define PDM types with short names
pdm_short_names <- c(
  "PDM_Experience_DBB" = "DBB",
  "PDM_Experience_CMAR" = "CMAR",
  "PDM_Experience_DB" = "DB",
  "PDM_Experience_PDB" = "PDB",
  "PDM_Experience_IPD" = "IPD"
)

# 3. Analysis Functions -----------------------------------------------------

# Function to prepare PDM experience data grouped by cluster
prepare_pdm_experience_data <- function(cluster_results, pdm_vars) {
  # Verify PDM variables exist in the dataset
  missing_vars <- setdiff(pdm_vars, colnames(cluster_results))
  if (length(missing_vars) > 0) {
    stop("The following PDM experience variables are missing from cluster results: ", 
         paste(missing_vars, collapse=", "))
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
  
  # Create a boxplot-ready format (original data in long format)
  pdm_boxplot_data <- cluster_results %>%
    pivot_longer(
      cols = all_of(pdm_vars),
      names_to = "PDM_Type",
      values_to = "Experience_Level"
    ) %>%
    mutate(Cluster = paste("Cluster", Cluster))
  
  # Order PDM types by innovation level
  pdm_exp_long$PDM_Type <- factor(
    pdm_exp_long$PDM_Type,
    levels = c("PDM_Experience_DBB", "PDM_Experience_CMAR", 
               "PDM_Experience_DB", "PDM_Experience_PDB", "PDM_Experience_IPD")
  )
  
  pdm_boxplot_data$PDM_Type <- factor(
    pdm_boxplot_data$PDM_Type,
    levels = c("PDM_Experience_DBB", "PDM_Experience_CMAR", 
               "PDM_Experience_DB", "PDM_Experience_PDB", "PDM_Experience_IPD")
  )
  
  return(list(
    summary = pdm_experience_by_cluster,
    long_format = pdm_exp_long,
    boxplot_data = pdm_boxplot_data
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
      paste0("033.1_cluster_pdm_experience_k45/pdm_experience_summary_k", k, ".csv")
    )
    
    # Save boxplot data
    save_data(
      pdm_exp_data$boxplot_data,
      paste0("033.1_cluster_pdm_experience_k45/pdm_experience_boxplot_data_k", k, ".csv")
    )
    
    # Define cluster colors - different color schemes for k=4 and k=5
    if (k == 4) {
      colors <- c("#4285F4", "#EA4335", "#FBBC05", "#34A853")  # Google colors for k=4
    } else if (k == 5) {
      colors <- c("#4285F4", "#EA4335", "#FBBC05", "#34A853", "#FF6D01")  # Google colors + orange for k=5
    } else {
      colors <- colorRampPalette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00"))(k)
    }
    
    # ================== Create Main Box Plot of PDM Experience by Cluster ==================
    
    # Add sample counts to the data
    cluster_counts <- pdm_exp_data$boxplot_data %>%
      group_by(Cluster, PDM_Type) %>%
      summarise(n = n(), .groups = "drop")
    
    # Create comprehensive box plot with sample sizes
    box_plot <- ggplot(pdm_exp_data$boxplot_data, 
                      aes(x = PDM_Type, y = Experience_Level, fill = Cluster)) +
      geom_boxplot(alpha = 0.8, outlier.shape = 21, outlier.size = 2, width = 0.7) +
      stat_summary(fun = mean, geom = "point", shape = 23, size = 3, color = "black", fill = "white") +
      # Add sample size for each box
      geom_text(
        data = cluster_counts,
        aes(label = paste0("n=", n), y = 0.7),
        position = position_dodge(width = 0.7),
        size = 3.5, fontface = "bold", vjust = 0
      ) +
      labs(
        title = paste("PDM Experience by Cluster (k =", k, ")"),
        subtitle = paste("Total samples:", nrow(cluster_data[[data_key]])),
        x = "Project Delivery Method", 
        y = "Experience Level (1-4)",
        fill = "Cluster"
      ) +
      scale_fill_manual(values = colors) +
      scale_x_discrete(labels = pdm_short_names) +
      theme_minimal(base_size = 16) +
      theme(
        plot.title = element_text(face = "bold", size = 20),
        plot.subtitle = element_text(size = 16, color = "gray40"),
        axis.title = element_text(face = "bold", size = 16),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
        axis.text.y = element_text(size = 14),
        legend.position = "right",
        legend.text = element_text(size = 14),
        legend.title = element_text(face = "bold", size = 16),
        panel.grid.major.y = element_line(color = "gray95"),
        panel.grid.minor = element_blank(),
        legend.background = element_rect(fill = "white", color = "gray90")
      ) +
      scale_y_continuous(limits = c(0.5, 4.5), breaks = 1:4)
    
    # Save the main box plot
    ggsave(
      paste0("results/figures/033.1_cluster_pdm_experience_k45/K", k, "_PDM_Experience_Box.pdf"),
      box_plot,
      width = 12,
      height = 8,
      dpi = 300
    )
    
    # Also save a PNG version for easier viewing
    ggsave(
      paste0("results/figures/033.1_cluster_pdm_experience_k45/K", k, "_PDM_Experience_Box.png"),
      box_plot,
      width = 12,
      height = 8,
      dpi = 300
    )
    
    # ================== Create Bar Chart of Mean PDM Experience by Cluster ==================
    
    # Prepare data for bar chart
    mean_data <- pdm_exp_data$long_format
    
    # Create bar chart
    bar_chart <- ggplot(mean_data, 
                       aes(x = PDM_Type, y = Mean_Experience, fill = Cluster)) +
      geom_bar(stat = "identity", position = "dodge", alpha = 0.9, width = 0.7) +
      geom_errorbar(
        aes(ymin = Mean_Experience - SD_Experience/2, 
            ymax = Mean_Experience + SD_Experience/2),
        position = position_dodge(width = 0.7),
        width = 0.25
      ) +
      labs(
        title = paste("Mean PDM Experience by Cluster (k =", k, ")"),
        subtitle = paste("Total samples:", nrow(cluster_data[[data_key]])),
        x = "Project Delivery Method", 
        y = "Mean Experience Level (1-4)",
        fill = "Cluster"
      ) +
      scale_fill_manual(values = colors) +
      scale_x_discrete(labels = pdm_short_names) +
      theme_minimal(base_size = 16) +
      theme(
        plot.title = element_text(face = "bold", size = 20),
        plot.subtitle = element_text(size = 16, color = "gray40"),
        axis.title = element_text(face = "bold", size = 16),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
        axis.text.y = element_text(size = 14),
        legend.position = "right",
        legend.text = element_text(size = 14),
        legend.title = element_text(face = "bold", size = 16),
        panel.grid.major.y = element_line(color = "gray95"),
        panel.grid.minor = element_blank(),
        legend.background = element_rect(fill = "white", color = "gray90")
      ) +
      scale_y_continuous(limits = c(0, 4.5), breaks = 0:4) +
      geom_text(
        aes(label = sprintf("%.1f", Mean_Experience), y = Mean_Experience + SD_Experience/2 + 0.2),
        position = position_dodge(width = 0.7),
        size = 3.5,
        fontface = "bold"
      )
    
    # Save the bar chart
    ggsave(
      paste0("results/figures/033.1_cluster_pdm_experience_k45/K", k, "_PDM_Experience_Bar.pdf"),
      bar_chart,
      width = 12,
      height = 8,
      dpi = 300
    )
    
    # Also save a PNG version for easier viewing
    ggsave(
      paste0("results/figures/033.1_cluster_pdm_experience_k45/K", k, "_PDM_Experience_Bar.png"),
      bar_chart,
      width = 12,
      height = 8,
      dpi = 300
    )
    
    # ================== Create Radar Chart of Mean PDM Experience by Cluster ==================
    
    # Prepare data for radar chart
    radar_data <- pdm_exp_data$long_format %>%
      select(Cluster, PDM_Type, Mean_Experience) %>%
      spread(key = PDM_Type, value = Mean_Experience)
    
    # Define radar chart segments based on PDM types
    radar_segments <- colnames(radar_data)[-1]  # Exclude Cluster column
    
    # Define radar chart function
    create_radar_chart <- function(data, segments, colors, title) {
      # Calculate positions
      n_segments <- length(segments)
      angles <- seq(0, 2 * pi, length.out = n_segments + 1)
      
      # Create a list to hold the data for each cluster
      radar_plots <- list()
      
      # Process each cluster
      for (i in 1:nrow(data)) {
        cluster_name <- data$Cluster[i]
        values <- as.numeric(data[i, segments])
        
        # Create a data frame for this cluster's radar
        cluster_data <- data.frame(
          Segment = rep(segments, each = 2),
          Angle = rep(angles[-length(angles)], each = 2),
          Value = rep(values, each = 2),
          Cluster = cluster_name
        )
        
        radar_plots[[i]] <- cluster_data
      }
      
      # Combine all cluster data
      radar_plot_data <- do.call(rbind, radar_plots)
      
      # Create the radar chart
      p <- ggplot(radar_plot_data, aes(x = Angle, y = Value, group = Cluster, color = Cluster)) +
        geom_polygon(aes(fill = Cluster), alpha = 0.2) +
        geom_path(linewidth = 1.5) +
        coord_polar() +
        scale_y_continuous(limits = c(0, 4), breaks = 1:4) +
        scale_fill_manual(values = colors) +
        scale_color_manual(values = colors) +
        theme_minimal() +
        theme(
          axis.text.x = element_blank(),
          axis.text.y = element_text(size = 12),
          axis.title = element_blank(),
          panel.grid.major.x = element_line(color = "gray80"),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_line(color = "gray90"),
          legend.position = "right",
          legend.title = element_text(face = "bold", size = 14),
          legend.text = element_text(size = 12),
          plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
          plot.subtitle = element_text(size = 14, hjust = 0.5, color = "gray40")
        ) +
        labs(
          title = title,
          subtitle = "PDM Experience Profile by Cluster (1-4 scale)"
        )
      
      # Add segment labels at appropriate angles
      segment_labels <- data.frame(
        Segment = segments,
        Angle = angles[-length(angles)],
        Label = pdm_short_names[segments],
        x = 4.5 * cos(angles[-length(angles)]),
        y = 4.5 * sin(angles[-length(angles)])
      )
      
      p <- p + geom_text(
        data = segment_labels,
        aes(x = x, y = y, label = Label),
        inherit.aes = FALSE,
        size = 5,
        fontface = "bold"
      )
      
      return(p)
    }
    
    # Create and save radar chart
    radar_chart <- create_radar_chart(
      radar_data,
      radar_segments,
      colors,
      paste("PDM Experience Radar Chart (k =", k, ")")
    )
    
    # Save the radar chart
    ggsave(
      paste0("results/figures/033.1_cluster_pdm_experience_k45/K", k, "_PDM_Experience_Radar.pdf"),
      radar_chart,
      width = 10,
      height = 8,
      dpi = 300
    )
    
    # Also save a PNG version for easier viewing
    ggsave(
      paste0("results/figures/033.1_cluster_pdm_experience_k45/K", k, "_PDM_Experience_Radar.png"),
      radar_chart,
      width = 10,
      height = 8,
      dpi = 300
    )
    
    cat("Created visualizations for k =", k, "\n")
  }
}

# Print completion message
cat("\nCluster vs PDM Experience Analysis for k=4 and k=5 complete!\n")
cat("Results saved to results/tables/033.1_cluster_pdm_experience_k45/\n")
cat("Visualizations saved to results/figures/033.1_cluster_pdm_experience_k45/\n") 