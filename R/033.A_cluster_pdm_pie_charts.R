# BOAT2 Cluster and Factor Analysis - PDM Experience Pie Charts by Cluster
# This script creates pie charts visualizing the distribution of PDM experience across clusters

# 1. Load Setup and Data -----------------------------------------------------
source("R/000_setup.R")
library(patchwork)  # Add patchwork package for combining plots
library(gridExtra)  # For extracting and placing legends

# Create subdirectories for results if they don't exist
dirs <- c(
  "results/figures/033A_cluster_pdm_pie_charts",
  "results/tables/033A_cluster_pdm_pie_charts"
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
  
  # --- FIX: Sort cluster IDs numerically before looping ---
  sorted_clusters <- sort(unique(cluster_data$Cluster))
  cat(paste0("  Generating plots in order: ", paste(sorted_clusters, collapse=", "), "\n")) # Debug message
  
  for (cluster_id in sorted_clusters) { # Use sorted IDs
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
  
  # Combine all plots into a single figure with shared legend at bottom
  combined_plot <- wrap_plots(plots, ncol = length(plots)) + 
    plot_layout(guides = "collect") &
    theme(legend.position = "bottom",
          legend.box = "horizontal",
          legend.title = element_text(face = "bold"))
  
  # Add main title
  final_plot <- combined_plot + 
    plot_annotation(
      title = paste0(pdm_name, " Experience by Cluster"),
      theme = theme(plot.title = element_text(face = "bold", size = 16, hjust = 0.5))
    )
  
  # Save the combined plot
  ggsave(
    paste0("results/figures/033A_cluster_pdm_pie_charts/K", k, "_", gsub("PDM_Experience_", "", pdm_var), "_Pie_Charts.pdf"),
    final_plot,
    width = 10,
    height = 6,
    dpi = 300
  )
  
  # Also save as PNG for easier viewing
  ggsave(
    paste0("results/figures/033A_cluster_pdm_pie_charts/K", k, "_", gsub("PDM_Experience_", "", pdm_var), "_Pie_Charts.png"),
    final_plot,
    width = 10,
    height = 6,
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
      paste0("033A_cluster_pdm_pie_charts/pdm_experience_distribution_k", k, ".csv")
    )
  }
}

cat("\nPDM Experience Pie Chart Analysis complete!\n")
cat("Pie charts saved to results/figures/033A_cluster_pdm_pie_charts/\n") 