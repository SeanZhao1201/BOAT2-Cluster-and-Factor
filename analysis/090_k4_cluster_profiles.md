# K=4 Cluster Profiles (Post 1 Outlier Removal)

**Script:** `R/090_kprototype_analysis_post_1_removal_k4.R`
**Data:** `results/tables/090_kprototype_post_1_removal_k4/`

## Cluster Sizes

| Cluster | n   | % of Total |
|---------|-----|------------|
| 1       | 3   | 2.8%       |
| 2       | 24  | 22.2%      |
| 3       | 53  | 49.1%      |
| 4       | 28  | 25.9%      |
| **Total** | **108** | **100%** |

## Cluster Median Profiles

| Variable | Cluster 1 | Cluster 2 | Cluster 3 | Cluster 4 |
|----------|-----------|-----------|-----------|-----------|
| ORG_Employees | 250 | 22.5 | 10 | 15 |
| ORG_Locations | 1 | 2 | 2 | 2 |
| ORG_Departments | 30 | 5 | 4 | 5.5 |
| ORG_Layers | 2 | 3 | 3 | 3 |
| DIST_Authority_Dispersion | 5 | 4 | 4 | 3 |
| DIST_Authority_Delegation | 4 | 4 | 4 | 2 |
| DIST_Informal_Communication | 5 | 4 | 4 | 3 |
| DIST_Informal_Procedure | 5 | 5 | 4 | 3 |
| STY_DataDriven | 5 | 4 | 4 | 3 |
| STY_Participation_Inclusion | 5 | 4 | 4 | 3 |
| STY_Participation_Relational | 5 | 5 | 4 | 3 |
| STY_Adaptive_Informal | 5 | 4 | 4 | 3 |
| STY_Adaptive_Changeable | 5 | 4 | 4 | 2.5 |
| STY_Authoritative_Threats | 1 | 1 | 2 | 3 |
| STY_Authoritative_Compliance | 1 | 1 | 2 | 3 |
| CUL_Command | 4 | 3.5 | 3 | 4 |
| CUL_Symbolic | 5 | 5 | 4 | 3 |
| CUL_Formal | 5 | 4 | 4 | 3 |
| CUL_Experimental | 5 | 4 | 4 | 3 |
| CUL_Learning | 5 | 5 | 4 | 3 |
| FLEX_OpenToNewIdeas | 4 | 5 | 4 | 3 |
| FLEX_OpenToChanges | 5 | 4 | 4 | 3 |
| RISK_Tolerance | 4 | 4 | 3 | 2 |
| ENV_SustainedGrowth | 4 | 5 | 4 | 3 |
| ENV_HighriskIndustry | 1 | 2 | 2 | 3 |
| ENV_IndustryStability | 4 | 4.5 | 4 | 3 |

## Cluster Characterizations

- **Cluster 1 (n=3):** "High-Performing Large Organizations" - Very large org (250 employees, 30 depts), highest scores across nearly all decision-making dimensions (median 5), lowest authoritative style (1), high risk tolerance. Very small sample - interpret with caution.
- **Cluster 2 (n=24):** "Collaborative Mid-Size Organizations" - Mid-size orgs (22.5 employees), high participation and relational scores (5), strong learning culture (5), very open to new ideas (5), sustained growth environment (5), low authoritative style (1).
- **Cluster 3 (n=53):** "Moderate Mainstream Organizations" - The largest cluster. Smaller orgs (10 employees), moderate scores across all dimensions (median 4), moderate risk tolerance (3), balanced decision-making approach.
- **Cluster 4 (n=28):** "Hierarchical/Conservative Organizations" - Mid-size orgs (15 employees), notably lower scores on participation (3), flexibility (3), and adaptiveness (2.5). Higher authoritative style (3), higher command culture (4), low risk tolerance (2), hostile environment (3).
