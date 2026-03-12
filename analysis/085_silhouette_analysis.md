# Silhouette Score Analysis (Post 1 Outlier Removal)

**Script:** `R/085_silhouette_analysis_post_1_removal.R`
**Figures:** `results/figures/085_silhouette_post_1_removal/`

## Method

Silhouette widths were computed using Gower distance on the mixed (numerical + categorical) dataset after removing one outlier case (ORG_Employees = 3700). K-prototypes clustering was run for k=2 through k=10 with nstart=10 and a fixed seed (12345).

## Results

| k  | Avg Silhouette Width |
|----|---------------------|
| 2  | 0.169               |
| 3  | 0.105               |
| **4**  | **0.104**       |
| 5  | 0.082               |
| 6  | 0.084               |
| 7  | 0.017               |
| 8  | 0.026               |
| 9  | 0.042               |
| 10 | 0.023               |

## Interpretation

- All silhouette scores are relatively low (<0.2), which is typical for mixed-type data where Gower distance dilutes cluster structure across many categorical variables.
- **k=2** yields the highest silhouette (0.169), but provides overly coarse groupings with limited practical interpretability.
- **k=3 and k=4 are nearly identical** (0.105 vs 0.104), suggesting that splitting from 3 to 4 clusters does not degrade cluster cohesion.
- A notable drop occurs after k=4 (to 0.082 at k=5), indicating that further splitting introduces noise rather than meaningful structure.
- Combined with the **Elbow plot** (script 080) showing WSS diminishing returns at k=4, and the **domain interpretability** of the 4-cluster solution, **k=4 is justified** as the optimal choice balancing statistical evidence and practical relevance.
