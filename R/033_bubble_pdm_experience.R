# BOAT2 Cluster and Factor Analysis - Cluster vs PDM Experience Bubble Visualization
# This script visualizes the relationship between k-prototype clusters and PDM experience using bubble charts

# 1. Load Setup and Data -----------------------------------------------------
source("R/000_setup.R")
library(ggrepel) # For improved label positioning

# Create subdirectories for results if they don't exist
dirs <- c(
  "results/figures/033_bubble_pdm_experience",
  "results/tables/033_bubble_pdm_experience"
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

# Define experience level labels
experience_level_labels <- c(
  "1" = "Never used & not familiar",
  "2" = "Never used but familiar",
  "3" = "Used once or twice",
  "4" = "Used many times"
)

# 3. Analysis Functions -----------------------------------------------------

# Function to prepare PDM experience data grouped by cluster
prepare_bubble_data <- function(cluster_results, pdm_vars) {
  # Verify PDM variables exist in the dataset
  missing_vars <- setdiff(pdm_vars, colnames(cluster_results))
  if (length(missing_vars) > 0) {
    stop("The following PDM experience variables are missing from cluster results: ", 
         paste(missing_vars, collapse=", "))
  }
  
  # Create bubble data (count by PDM, Experience Level, and Cluster)
  bubble_data <- cluster_results %>%
    pivot_longer(
      cols = all_of(pdm_vars),
      names_to = "PDM_Type",
      values_to = "Experience_Level"
    ) %>%
    group_by(Cluster, PDM_Type, Experience_Level) %>%
    summarise(Count = n(), .groups = "drop") %>%
    mutate(
      Cluster = paste("Cluster", Cluster),
      PDM_Label = gsub("PDM_Experience_", "", PDM_Type),
      Percentage = Count / sum(Count) * 100
    )
  
  # Order PDM types by innovation level
  bubble_data$PDM_Type <- factor(
    bubble_data$PDM_Type,
    levels = c("PDM_Experience_DBB", "PDM_Experience_CMAR", 
               "PDM_Experience_DB", "PDM_Experience_PDB", "PDM_Experience_IPD")
  )
  
  # Set Experience_Level as factor for better control
  bubble_data$Experience_Level <- factor(
    bubble_data$Experience_Level,
    levels = c("1", "2", "3", "4")
  )
  
  return(bubble_data)
}

# 4. Process data and create visualizations for each k value ----------------
for (k in k_values) {
  data_key <- paste0("k", k)
  
  if (data_key %in% names(cluster_data)) {
    # Prepare data
    bubble_data <- prepare_bubble_data(
      cluster_data[[data_key]], 
      pdm_experience_vars
    )
    
    # Save bubble data
    save_data(
      bubble_data,
      paste0("033_bubble_pdm_experience/pdm_experience_bubble_data_k", k, ".csv")
    )
    
    # Define cluster colors
    if (k == 2) {
      colors <- c("#1b9e77", "#d95f02")  # Dark teal and orange for k=2
    } else if (k == 3) {
      colors <- c("#7570b3", "#e7298a", "#66a61e")  # Purple, pink, green for k=3
    } else {
      colors <- colorRampPalette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00"))(k)
    }
    
    # ================== Create Bubble Chart of PDM Experience by Cluster ==================
    # Get overall total for each cluster
    cluster_totals <- bubble_data %>%
      group_by(Cluster) %>%
      summarise(Total = sum(Count), .groups = "drop")
    
    # Add label info for bubbles
    bubble_data_with_labels <- bubble_data %>%
      left_join(cluster_totals, by = "Cluster") %>%
      mutate(
        RelativeSize = Count / Total,
        Label = paste0(Count, " (", round(Count/Total*100, 1), "%)"),
        PDM_Label = ifelse(PDM_Type == "PDM_Experience_DBB", "DBB",
                    ifelse(PDM_Type == "PDM_Experience_DB", "DB",
                    ifelse(PDM_Type == "PDM_Experience_PDB", "PDB",
                    ifelse(PDM_Type == "PDM_Experience_CMAR", "CMAR", "IPD"))))
      )
    
    # Create combined bubble chart
    bubble_chart <- ggplot(bubble_data_with_labels, 
                           aes(x = PDM_Type, y = Experience_Level, size = Count, 
                               color = Cluster, fill = Cluster)) +
      geom_point(alpha = 0.7, shape = 21, stroke = 1.5) +
      geom_text_repel(aes(label = Label), 
                     size = 3, 
                     color = "black",
                     box.padding = 0.5,
                     point.padding = 0.5,
                     segment.color = "gray50") +
      facet_wrap(~ Cluster, ncol = 1) +
      labs(
        title = paste("PDM Experience Distribution by Cluster (k =", k, ")"),
        subtitle = paste("Bubble size represents count, with percentage relative to cluster total"),
        x = "Project Delivery Method", 
        y = "Experience Level",
        size = "Count",
        color = "Cluster",
        fill = "Cluster"
      ) +
      scale_color_manual(values = colors) +
      scale_fill_manual(values = colors) +
      scale_x_discrete(labels = function(x) gsub("PDM_Experience_", "", x)) +
      scale_y_discrete(labels = experience_level_labels) +
      scale_size_area(max_size = 15) +
      theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(face = "bold", size = 16),
        plot.subtitle = element_text(size = 12, color = "gray40"),
        axis.title = element_text(face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
        axis.text.y = element_text(size = 10),
        legend.position = "right",
        panel.grid.major = element_line(color = "gray95"),
        panel.grid.minor = element_blank(),
        legend.background = element_rect(fill = "white", color = "gray90"),
        legend.title = element_text(face = "bold"),
        strip.text = element_text(face = "bold", size = 14),
        strip.background = element_rect(fill = "gray95", color = NA)
      )
    
    # Save the bubble chart
    ggsave(
      paste0("results/figures/033_bubble_pdm_experience/K", k, "_PDM_Experience_Bubble.pdf"),
      bubble_chart,
      width = 12,
      height = 10,
      dpi = 300
    )
    
    # Also save a PNG version for easier viewing
    ggsave(
      paste0("results/figures/033_bubble_pdm_experience/K", k, "_PDM_Experience_Bubble.png"),
      bubble_chart,
      width = 12,
      height = 10,
      dpi = 300
    )
    
    # =================== Create Alternative Version: Bubble Grid ==================
    # Create a version where PDM and Experience Level are both on x-axis in grid format
    bubble_grid <- ggplot(bubble_data_with_labels, 
                         aes(x = Experience_Level, y = PDM_Label, size = Count, 
                             color = Cluster, fill = Cluster)) +
      geom_point(alpha = 0.7, shape = 21, stroke = 1.5) +
      geom_text(aes(label = Count), 
               size = 3, 
               color = "black") +
      facet_wrap(~ Cluster) +
      labs(
        title = paste("PDM Experience Grid by Cluster (k =", k, ")"),
        subtitle = paste("Bubble size represents count of respondents"),
        x = "Experience Level", 
        y = "Project Delivery Method",
        size = "Count",
        color = "Cluster",
        fill = "Cluster"
      ) +
      scale_color_manual(values = colors) +
      scale_fill_manual(values = colors) +
      scale_x_discrete(labels = c("Never used &\nnot familiar", 
                                "Never used\nbut familiar", 
                                "Used once\nor twice", 
                                "Used\nmany times")) +
      scale_size_area(max_size = 15) +
      theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(face = "bold", size = 16),
        plot.subtitle = element_text(size = 12, color = "gray40"),
        axis.title = element_text(face = "bold"),
        axis.text.x = element_text(angle = 0, hjust = 0.5, size = 10),
        axis.text.y = element_text(size = 10),
        legend.position = "right",
        panel.grid.major = element_line(color = "gray95"),
        panel.grid.minor = element_blank(),
        legend.background = element_rect(fill = "white", color = "gray90"),
        legend.title = element_text(face = "bold"),
        strip.text = element_text(face = "bold", size = 14),
        strip.background = element_rect(fill = "gray95", color = NA)
      )
    
    # Save the bubble grid
    ggsave(
      paste0("results/figures/033_bubble_pdm_experience/K", k, "_PDM_Experience_BubbleGrid.pdf"),
      bubble_grid,
      width = 12,
      height = 8,
      dpi = 300
    )
    
    # Also save a PNG version for easier viewing
    ggsave(
      paste0("results/figures/033_bubble_pdm_experience/K", k, "_PDM_Experience_BubbleGrid.png"),
      bubble_grid,
      width = 12,
      height = 8,
      dpi = 300
    )
    
    # Log progress
    cat("Created bubble visualizations for k =", k, "\n")
  }
}

cat("\nCluster vs PDM Experience Bubble Chart Analysis complete!\n")
cat("Bubble charts saved to results/figures/033_bubble_pdm_experience/\n") 