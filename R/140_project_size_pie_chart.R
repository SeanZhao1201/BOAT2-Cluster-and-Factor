# BOAT2 Cluster and Factor Analysis - Project Size Distribution Pie Chart
# This script creates a pie chart showing the distribution of project sizes

# 1. Load Setup and Data -----------------------------------------------------
source("R/000_setup.R")

# Create subdirectories for results if they don't exist
dirs <- c(
  "results/figures/140_project_size_pie_chart",
  "results/tables/140_project_size_pie_chart"
)

for (dir in dirs) {
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE)
    cat(paste("Created directory:", dir, "\n"))
  }
}

# Helper function for saving plots
save_plot <- function(plot, filename, width = 10, height = 8, dpi = 300) {
  full_path <- file.path("results/figures/140_project_size_pie_chart", filename)
  ggsave(
    filename = full_path,
    plot = plot,
    width = width,
    height = height,
    dpi = dpi
  )
  cat(paste("Saved plot to:", full_path, "\n"))
}

# Helper function for saving data
save_data <- function(data, filename) {
  full_path <- file.path("results/tables/140_project_size_pie_chart", filename)
  write.csv(
    x = data,
    file = full_path,
    row.names = FALSE
  )
  cat(paste("Saved data to:", full_path, "\n"))
}

# Load the pre-cleaned data which contains Project_Size
data <- read.csv("data/pre_cleaned_data.csv")
cat("Loaded dataset with", nrow(data), "rows and", ncol(data), "columns.\n")

# 2. Data Preparation --------------------------------------------------------

# Check Project_Size variable
cat("\nProject_Size variable summary:\n")
print(table(data$Project_Size, useNA = "ifany"))

# Clean and categorize project sizes
# Create ordered factor for project sizes
project_size_levels <- c(
  "less than 25k sq ft (7.6k sqm)",
  "25k - 75k sq ft (7.6k - 23k sqm)", 
  "75k - 125k sq ft (23k - 38k sqm)",
  "greater than 125k sq ft (38k sqm)"
)

# Clean the data and create summary
project_size_clean <- data %>%
  filter(!is.na(Project_Size) & Project_Size != "") %>%
  mutate(
    Project_Size_Clean = case_when(
      Project_Size == "less than 25k sq ft (7.6k sqm)" ~ "Less than 25k sq ft\n(<2,320 sq m)",
      Project_Size == "25k - 75k sq ft (7.6k - 23k sqm)" ~ "25k - 75k sq ft\n(2,320 - 6,970 sq m)",
      Project_Size == "75k - 125k sq ft (23k - 38k sqm)" ~ "75k - 125k sq ft\n(6,970 - 11,600 sq m)",
      Project_Size == "greater than 125k sq ft (38k sqm)" ~ "Greater than 125k sq ft\n(>11,600 sq m)",
      TRUE ~ as.character(Project_Size)
    )
  ) %>%
  filter(Project_Size_Clean %in% c(
    "Less than 25k sq ft\n(<2,320 sq m)",
    "25k - 75k sq ft\n(2,320 - 6,970 sq m)", 
    "75k - 125k sq ft\n(6,970 - 11,600 sq m)",
    "Greater than 125k sq ft\n(>11,600 sq m)"
  ))

# Create summary statistics
project_size_summary <- project_size_clean %>%
  count(Project_Size_Clean, name = "count") %>%
  mutate(
    percentage = round(count / sum(count) * 100, 2),
    label = paste0(count, " (", percentage, "%)")
  ) %>%
  arrange(factor(Project_Size_Clean, levels = c(
    "Less than 25k sq ft\n(<2,320 sq m)",
    "25k - 75k sq ft\n(2,320 - 6,970 sq m)", 
    "75k - 125k sq ft\n(6,970 - 11,600 sq m)",
    "Greater than 125k sq ft\n(>11,600 sq m)"
  )))

# Print summary
cat("\nProject Size Distribution Summary:\n")
print(project_size_summary)

# Save summary data
save_data(project_size_summary, "project_size_distribution.csv")

# 3. Create Color Palette ----------------------------------------------------

# Create sophisticated blue to purple gradient
project_size_colors <- c(
  "#E3F2FD",  # Very light blue
  "#90CAF9",  # Light blue  
  "#5C6BC0",  # Medium blue-purple
  "#512DA8"   # Deep purple
)

names(project_size_colors) <- c(
  "Less than 25k sq ft\n(<2,320 sq m)",
  "25k - 75k sq ft\n(2,320 - 6,970 sq m)", 
  "75k - 125k sq ft\n(6,970 - 11,600 sq m)",
  "Greater than 125k sq ft\n(>11,600 sq m)"
)

# 4. Create Pie Chart --------------------------------------------------------

# Create the pie chart with improved label positioning and colors
pie_chart <- ggplot(project_size_summary, aes(x = "", y = count, fill = Project_Size_Clean)) +
  geom_bar(stat = "identity", width = 1, color = "white", size = 1.5) +
  coord_polar("y", start = 0) +
  geom_text(aes(label = label, 
                color = Project_Size_Clean),
            position = position_stack(vjust = 0.5),
            size = 4.5,
            fontface = "bold") +
  scale_fill_manual(values = project_size_colors, 
                   name = "Project Size",
                   breaks = names(project_size_colors)) +
  scale_color_manual(values = c(
    "Less than 25k sq ft\n(<2,320 sq m)" = "black",
    "25k - 75k sq ft\n(2,320 - 6,970 sq m)" = "black", 
    "75k - 125k sq ft\n(6,970 - 11,600 sq m)" = "black",
    "Greater than 125k sq ft\n(>11,600 sq m)" = "white"
  ), guide = "none") +
  labs(title = "Project Size Distribution",
       subtitle = "n=109",
       x = NULL, y = NULL) +
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold", 
                             margin = margin(b = 10), color = "#2C3E50"),
    plot.subtitle = element_text(hjust = 0.5, size = 14, 
                                margin = margin(b = 20), color = "#34495E"),
    legend.title = element_text(size = 14, face = "bold", color = "#2C3E50"),
    legend.text = element_text(size = 12, color = "#2C3E50"),
    legend.position = "right",
    legend.key.size = unit(1, "cm"),
    legend.margin = margin(l = 20, r = 10, t = 0, b = 0),
    legend.spacing.y = unit(0.5, "cm"),
    plot.margin = unit(c(1, 1, 1, 1), "cm"),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )

# Save the pie chart
save_plot(pie_chart, "project_size_pie_chart.pdf", width = 12, height = 8)

# 5. Create Alternative Horizontal Bar Chart ---------------------------------

# Create a horizontal bar chart as an alternative visualization
bar_chart <- ggplot(project_size_summary, aes(x = reorder(Project_Size_Clean, count), y = count, fill = Project_Size_Clean)) +
  geom_bar(stat = "identity", color = "white", size = 1) +
  geom_text(aes(label = label), 
            hjust = -0.1, 
            color = "#2C3E50", 
            size = 4, 
            fontface = "bold") +
  scale_fill_manual(values = project_size_colors, guide = "none") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  coord_flip() +
  labs(title = "Project Size Distribution",
       subtitle = "n=109",
       x = "Project Size",
       y = "Count") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold", 
                             margin = margin(b = 10), color = "#2C3E50"),
    plot.subtitle = element_text(hjust = 0.5, size = 14, 
                                margin = margin(b = 20), color = "#34495E"),
    axis.title = element_text(size = 12, face = "bold", color = "#2C3E50"),
    axis.text = element_text(size = 11, color = "#2C3E50"),
    axis.text.y = element_text(hjust = 0),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    plot.margin = unit(c(1, 1, 1, 1), "cm"),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )

# Save the bar chart
save_plot(bar_chart, "project_size_bar_chart.pdf", width = 12, height = 8)

# 6. Summary Statistics ------------------------------------------------------

cat("\n============ PROJECT SIZE ANALYSIS SUMMARY ============\n")
cat("Total valid project size records:", sum(project_size_summary$count), "\n")
cat("Most common project size:", project_size_summary$Project_Size_Clean[which.max(project_size_summary$count)], "\n")
cat("Percentage of largest projects (>125k sq ft):", 
    project_size_summary$percentage[project_size_summary$Project_Size_Clean == "Greater than 125k sq ft\n(>11,600 sq m)"], "%\n")

# Create detailed summary table
detailed_summary <- project_size_summary %>%
  mutate(
    cumulative_count = cumsum(count),
    cumulative_percentage = cumsum(percentage)
  )

save_data(detailed_summary, "project_size_detailed_summary.csv")

cat("\nProject size analysis complete!\n")
cat("Generated pie chart and bar chart visualizations\n")
cat("Saved summary tables to results/tables/140_project_size_pie_chart/\n") 