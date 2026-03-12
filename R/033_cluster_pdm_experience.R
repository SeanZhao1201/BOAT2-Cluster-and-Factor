# BOAT2 Cluster and Factor Analysis - Cluster vs PDM Experience Visualization
# This script visualizes the relationship between k-prototype clusters and PDM experience

# 1. Load Setup and Data -----------------------------------------------------
source("R/000_setup.R")

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
      paste0("033_cluster_pdm_experience/pdm_experience_summary_k", k, ".csv")
    )
    
    # Save boxplot data
    save_data(
      pdm_exp_data$boxplot_data,
      paste0("033_cluster_pdm_experience/pdm_experience_boxplot_data_k", k, ".csv")
    )
    
    # Define cluster colors
    if (k == 2) {
      colors <- c("#1b9e77", "#d95f02")  # Dark teal and orange for k=2
    } else if (k == 3) {
      colors <- c("#7570b3", "#e7298a", "#66a61e")  # Purple, pink, green for k=3
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
        aes(label = paste0("n=", n), y = 0.8),
        position = position_dodge(width = 0.7),
        size = 3.8, fontface = "bold", vjust = 0
      ) +
      labs(
        title = paste("PDM Experience by Cluster (k =", k, ")"),
        subtitle = paste("Total samples:", nrow(cluster_data[[data_key]])),
        x = "Project Delivery Method", 
        y = "Experience Level (1-4)",
        fill = "Cluster"
      ) +
      scale_fill_manual(values = colors) +
      scale_x_discrete(labels = function(x) gsub("PDM_Experience_", "", x)) +
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
      paste0("results/figures/033_cluster_pdm_experience/K", k, "_PDM_Experience_Box.pdf"),
      box_plot,
      width = 11,
      height = 8,
      dpi = 300
    )
    
    # Also save a PNG version for easier viewing
    ggsave(
      paste0("results/figures/033_cluster_pdm_experience/K", k, "_PDM_Experience_Box.png"),
      box_plot,
      width = 11,
      height = 8,
      dpi = 300
    )
    
    # Skip the rest of the plots (individual PDM types, radar charts, etc.)
    cat("Created main box plot for k =", k, "\n")
  }
}

# Skip the rest of the comparisons
cat("\nCluster vs PDM Experience Analysis complete!\n")
cat("Main box plots saved to results/figures/033_cluster_pdm_experience/\n") 