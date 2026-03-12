# BOAT2 Cluster and Factor Analysis - PDM Experience Pie Charts by Cluster (k=4 and k=5)
# This script creates pie charts visualizing the distribution of PDM experience across clusters
# Specifically for k=4 and k=5 clusters from the k-prototype analysis

# 1. Load Setup and Data -----------------------------------------------------
source("R/000_setup.R")
library(patchwork)  # Add patchwork package for combining plots
library(gridExtra)  # For extracting and placing legends

# Create subdirectories for results if they don't exist
dirs <- c(
  "results/figures/033A.1_cluster_pdm_pie_charts_k45",
  "results/tables/033A.1_cluster_pdm_pie_charts_k45"
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

# Create more intuitive labels for visualization (removed parentheses)
pdm_experience_labels <- c(
  "PDM_Experience_DBB" = "Design-Bid-Build",
  "PDM_Experience_DB" = "Design-Build",
  "PDM_Experience_PDB" = "Progressive Design-Build", 
  "PDM_Experience_CMAR" = "Construction Manager @ Risk",
  "PDM_Experience_IPD" = "Integrated Project Delivery"
)

# 3. Define experience level categories (corrected to 1-4 scale) -------------
experience_categories <- c(
  "Never used & not familiar" = 1,
  "Never used but familiar" = 2,
  "Used once or twice" = 3,
  "Used many times" = 4
)

# Helper function to extract legend from a ggplot
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

# Helper function to format multi-line titles
format_title <- function(pdm_name, cluster_id) {
  # Insert line breaks at appropriate positions
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

# 4. Function to create pie charts of PDM experience by cluster -------------
create_pdm_pie_charts <- function(cluster_data, pdm_var, k) {
  # Create a more readable PDM name for titles
  pdm_name <- pdm_experience_labels[pdm_var]
  if (is.na(pdm_name)) {
    pdm_name <- gsub("PDM_Experience_", "", pdm_var)
  }
  
  # Prepare data for pie charts
  pie_data <- cluster_data %>%
    group_by(Cluster) %>%
    count(!!sym(pdm_var)) %>%
    mutate(
      Percentage = n / sum(n) * 100,
      Label = paste0(n, " (", round(Percentage, 1), "%)"),
      Experience = factor(!!sym(pdm_var), 
                          levels = unname(experience_categories),
                          labels = names(experience_categories))
    )
  
  # Define a professional color palette for experience levels (4 levels)
  experience_colors <- c(
    "Never used & not familiar" = "#E41A1C",    # Red
    "Never used but familiar" = "#FF7F00",      # Orange
    "Used once or twice" = "#4DAF4A",           # Green
    "Used many times" = "#377EB8"               # Blue
  )
  
  # Define cluster-specific titles
  cluster_titles <- paste("Cluster", unique(cluster_data$Cluster))
  
  # Create a pie chart for each cluster
  plots <- list()
  
  for (cluster_id in unique(cluster_data$Cluster)) {
    # Filter data for this cluster
    cluster_pie_data <- pie_data %>%
      filter(Cluster == cluster_id)
    
    # Ensure all experience levels are represented (even if count is 0)
    all_experiences <- data.frame(
      Experience = factor(names(experience_categories), 
                          levels = names(experience_categories))
    )
    
    cluster_pie_data <- all_experiences %>%
      left_join(cluster_pie_data, by = "Experience") %>%
      mutate(
        Cluster = ifelse(is.na(Cluster), cluster_id, Cluster),
        n = ifelse(is.na(n), 0, n),
        Percentage = ifelse(is.na(Percentage), 0, Percentage),
        Label = ifelse(is.na(Label), paste0(n, " (", round(Percentage, 1), "%)"), Label)
      )
    
    # Total count for this cluster
    total_count <- sum(cluster_pie_data$n)
    
    # Format title with appropriate line breaks
    formatted_title <- format_title(pdm_name, cluster_id)
    
    # Create pie chart - include legend only in the first plot for reference
    show_legend <- ifelse(cluster_id == min(unique(cluster_data$Cluster)), TRUE, FALSE)
    
    p <- ggplot(cluster_pie_data, aes(x = "", y = Percentage, fill = Experience)) +
      geom_bar(stat = "identity", width = 0.6, color = "white") +  # Narrower bar width creates donut effect
      coord_polar("y", start = 0) +
      geom_text(aes(label = Label), 
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
        legend.position = ifelse(show_legend, "bottom", "none"),
        legend.title = element_text(face = "bold")
      ) +
      guides(fill = guide_legend(nrow = 2))  # Set legend to display in 2 rows
    
    plots[[paste0("cluster_", cluster_id)]] <- p
  }
  
  # For k=4 and k=5, we need to adjust the layout
  if (k == 4) {
    ncol_value <- 2  # 2x2 grid for 4 clusters
    plot_height <- 10
  } else if (k == 5) {
    ncol_value <- 3  # 2 rows: 3 and 2 charts for 5 clusters
    plot_height <- 12
  } else {
    ncol_value <- length(plots)  # Default behavior
    plot_height <- 8
  }
  
  # Combine all plots into a single figure with shared legend
  combined_plot <- wrap_plots(plots) + 
    plot_layout(ncol = ncol_value, guides = "collect") &
    theme(legend.position = "bottom",
          legend.box = "horizontal",
          legend.title = element_text(face = "bold"))
  
  # Add main title
  final_plot <- combined_plot + 
    plot_annotation(
      title = paste0(pdm_name, " Experience by Cluster (k=", k, ")"),
      theme = theme(plot.title = element_text(face = "bold", size = 16, hjust = 0.5))
    )
  
  # Save the combined plot
  ggsave(
    paste0("results/figures/033A.1_cluster_pdm_pie_charts_k45/K", k, "_", gsub("PDM_Experience_", "", pdm_var), "_Pie_Charts.pdf"),
    final_plot,
    width = 12,
    height = plot_height,
    dpi = 300
  )
  
  # Also save as PNG for easier viewing
  ggsave(
    paste0("results/figures/033A.1_cluster_pdm_pie_charts_k45/K", k, "_", gsub("PDM_Experience_", "", pdm_var), "_Pie_Charts.png"),
    final_plot,
    width = 12,
    height = plot_height,
    dpi = 300
  )
  
  return(plots)
}

# 5. Create pie charts for each PDM type and k-value ------------------------
for (k in k_values) {
  data_key <- paste0("k", k)
  
  if (data_key %in% names(cluster_data)) {
    cat("\nGenerating pie charts for k =", k, "\n")
    
    # For each PDM experience variable, create pie charts
    for (pdm_var in pdm_experience_vars) {
      cat("Creating pie charts for", pdm_var, "\n")
      charts <- create_pdm_pie_charts(cluster_data[[data_key]], pdm_var, k)
    }
    
    # Create a consolidated data table for experience distribution
    experience_summary <- data.frame()
    
    for (pdm_var in pdm_experience_vars) {
      pdm_summary <- cluster_data[[data_key]] %>%
        group_by(Cluster, !!sym(pdm_var)) %>%
        summarise(Count = n(), .groups = "drop") %>%
        mutate(
          PDM_Type = pdm_var,
          Experience_Level = !!sym(pdm_var)
        ) %>%
        select(Cluster, PDM_Type, Experience_Level, Count)
      
      experience_summary <- bind_rows(experience_summary, pdm_summary)
    }
    
    # Save the consolidated summary table
    save_data(
      experience_summary,
      paste0("033A.1_cluster_pdm_pie_charts_k45/pdm_experience_distribution_k", k, ".csv")
    )
  }
}

cat("\nPDM Experience Pie Chart Analysis for k=4 and k=5 complete!\n")
cat("Pie charts saved to results/figures/033A.1_cluster_pdm_pie_charts_k45/\n")
cat("Data tables saved to results/tables/033A.1_cluster_pdm_pie_charts_k45/\n")