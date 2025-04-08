# Load required packages and data
source("R/00_setup.R")

# Load data
kproto_data <- read.csv("results/tables/kproto_data.csv")

# Define variable groups
kproto_numerical_vars <- c(
  "Org_Structure_Employees",
  "Org_Structure_Locations",
  "Org_Structure_Depts",
  "Org_Structure_Layers"
)

kproto_categorical_vars <- setdiff(
  colnames(kproto_data), 
  c("ID", "PDM_Type", kproto_numerical_vars)
)

# Create mixed dataset
kproto_mixed_data <- kproto_data %>%
  select(all_of(c(kproto_numerical_vars, kproto_categorical_vars)))

# Convert ordinal variables to ordered factors
kproto_mixed_data <- kproto_mixed_data %>%
  mutate(across(all_of(kproto_categorical_vars), ~ ordered(round(.), levels = 1:5)))

# Run K-Prototype clustering
set.seed(123)
kproto_result <- clustMixType::kproto(
  kproto_mixed_data, 
  k = 2,
  verbose = TRUE
)

# Print results
print(str(kproto_result))

# Save results
kproto_results <- kproto_data %>%
  mutate(Cluster = kproto_result$cluster)

write.csv(
  kproto_results,
  "results/tables/kproto_clusters_k2.csv",
  row.names = FALSE
)

# Create centroids dataframe
kproto_centroids <- data.frame(
  Cluster = 1:2,
  kproto_result$centers
)

write.csv(
  kproto_centroids,
  "results/tables/kproto_centroids_k2.csv",
  row.names = FALSE
) 