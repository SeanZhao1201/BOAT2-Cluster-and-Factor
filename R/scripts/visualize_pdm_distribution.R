# BOAT2 Cluster and Factor Analysis - PDM Distribution Visualization
# This script creates visualizations of PDM distribution for different clustering solutions

# Load required packages
source("R/00_setup.R")

# Load clustering results
k2_results <- read.csv("results/tables/kproto_clusters_k2.csv")
k3_results <- read.csv("results/tables/kproto_clusters_k3.csv")

# Define PDM type order
pdm_order <- c(
  "Design-Bid-Build",
  "Design-Build",
  "Construction Manager @ Risk",
  "Progressive Design-Build",
  "Integrated Project Delivery (IPD)"
)

# Function to create PDM distribution plot
create_pdm_distribution_plot <- function(data, k_value) {
  # Create contingency table
  pdm_table <- table(data$PDM_Type, data$Cluster)
  
  # Convert to data frame for ggplot
  pdm_df <- as.data.frame(pdm_table)
  colnames(pdm_df) <- c("PDM_Type", "Cluster", "Count")
  pdm_df$Cluster <- paste("Cluster", pdm_df$Cluster)
  
  # Order PDM types
  pdm_df$PDM_Type <- factor(pdm_df$PDM_Type, levels = pdm_order)
  
  # Calculate percentages
  pdm_df <- pdm_df %>%
    group_by(Cluster) %>%
    mutate(Percentage = Count / sum(Count) * 100)
  
  # Create plot
  p <- ggplot(pdm_df, aes(x = Cluster, y = Percentage, fill = PDM_Type)) +
    geom_bar(stat = "identity", position = "stack") +
    geom_text(
      aes(label = sprintf("%.1f%%", Percentage)),
      position = position_stack(vjust = 0.5),
      size = 3,
      color = "black"  # Ensure text is readable
    ) +
    labs(
      title = paste("PDM Distribution by K-Prototypes Cluster (k=", k_value, ")"),
      x = "Cluster",
      y = "Percentage",
      fill = "PDM Type"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.title = element_text(hjust = 0.5),
      legend.position = "right",
      legend.text = element_text(size = 8)
    ) +
    scale_fill_brewer(palette = "Pastel1")  # Using a pastel color palette
  
  return(p)
}

# Create plots for K=2 and K=3
k2_plot <- create_pdm_distribution_plot(k2_results, 2)
k3_plot <- create_pdm_distribution_plot(k3_results, 3)

# Save plots
ggsave("results/figures/pdm_distribution_k2.pdf", k2_plot, width = 10, height = 7)
ggsave("results/figures/pdm_distribution_k3.pdf", k3_plot, width = 10, height = 7)

# Create a combined plot
combined_plot <- grid.arrange(
  k2_plot + theme(legend.position = "none"),
  k3_plot,
  ncol = 2,
  widths = c(1, 1.3)
)

# Save combined plot
ggsave("results/figures/pdm_distribution_combined.pdf", combined_plot, width = 16, height = 7)

# Print summary statistics
cat("\nK=2 Clustering PDM Distribution:\n")
k2_table <- table(k2_results$PDM_Type, k2_results$Cluster)
print(k2_table)
cat("\nPercentages:\n")
print(round(prop.table(k2_table, margin = 2) * 100, 1))

cat("\nK=3 Clustering PDM Distribution:\n")
k3_table <- table(k3_results$PDM_Type, k3_results$Cluster)
print(k3_table)
cat("\nPercentages:\n")
print(round(prop.table(k3_table, margin = 2) * 100, 1)) 