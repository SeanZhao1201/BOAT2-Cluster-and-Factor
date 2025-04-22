import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from sklearn.preprocessing import StandardScaler
from sklearn.metrics import pairwise_distances
import skfuzzy as fuzz
import math
import seaborn as sns

# Set plot style
plt.style.use('ggplot')
sns.set_context("paper")

# Load the dataset
print("Loading dataset...")
data = pd.read_csv('data/BOAT2_Data_Enhanced.csv')
print(f"Loaded dataset with {data.shape[0]} rows and {data.shape[1]} columns")

# Define variable groups
# Organization structure variables
org_vars = [
    "ORG_Size_Employees",
    "ORG_Complexity_Locations",
    "ORG_Complexity_Departments",
    "ORG_Hierarchy_Layers"
]

# Decision variables
dec_vars = [
    "DEC_Authority_Dispersion",
    "DEC_Authority_Delegation",
    "DEC_Process_InformalCommunication",
    "DEC_Process_InformalProcedures"
]

# Style variables
sty_vars = [
    "STY_Analytical_DataDriven",
    "STY_Participative_Inclusion",
    "STY_Participative_Relational",
    "STY_Organic_InformalStructure",
    "STY_Organic_Adaptability",
    "STY_Directive_Threats",
    "STY_Directive_Compliance"
]

# Culture variables
cul_vars = [
    "CUL_Authority_Hierarchical",
    "CUL_Integration_Vision",
    "CUL_Integration_Systematic",
    "CUL_Innovation_Experimental",
    "CUL_Collaboration_Stakeholder"
]

# Flexibility variables
flex_vars = [
    "FLEX_Cognitive_Receptivity",
    "FLEX_Behavioral_Adaptability"
]

# Risk and environment variables
risk_env_vars = [
    "RISK_Appetite_Investment",
    "ENV_Context_Growth",
    "ENV_Context_Volatility",
    "ENV_Context_Stability"
]

# PDM variables (to exclude from clustering)
pdm_vars = [
    "PDM_Selected",
    "PDM_Experience_DBB",
    "PDM_Experience_DB",
    "PDM_Experience_PDB",
    "PDM_Experience_CMAR",
    "PDM_Experience_IPD"
]

# Define variable groups for radar charts
group1_vars = dec_vars + sty_vars
group2_vars = cul_vars + flex_vars + risk_env_vars

# All variables to be used for clustering (excluding PDM variables)
all_vars = org_vars + group1_vars + group2_vars

# Prepare data for clustering
print("Preparing data for clustering...")
cluster_data = data[all_vars].copy()

# Handle missing values if any (replace with median)
for col in cluster_data.columns:
    if cluster_data[col].isnull().any():
        median_value = cluster_data[col].median()
        cluster_data[col] = cluster_data[col].fillna(median_value)
        print(f"Filled {cluster_data[col].isnull().sum()} missing values in {col} with median {median_value}")

# Scale organization variables separately since they're not Likert scales
org_data = cluster_data[org_vars].copy()
scaler = StandardScaler()
org_data_scaled = pd.DataFrame(
    scaler.fit_transform(org_data),
    columns=org_vars,
    index=org_data.index
)

# For Likert scale variables, min-max scale to [1, 5] range
likert_vars = group1_vars + group2_vars
likert_data = cluster_data[likert_vars].copy()

# Combine scaled data
X_processed = pd.concat([org_data_scaled, likert_data], axis=1)
print(f"Processed data shape: {X_processed.shape}")

# Function to create a custom distance metric for mixed variable types
def custom_distance(X):
    """
    Create a custom distance matrix using Manhattan distance
    which is more appropriate for Likert scale data
    """
    distance_matrix = pairwise_distances(X, metric='manhattan')
    return distance_matrix

# Perform Fuzzy C-means clustering
print("Performing Fuzzy C-means clustering with K=2...")
k = 2  # Number of clusters
m = 2  # Fuzziness parameter (usually between 1.5 and 2.5)

# Convert data to numpy array
X_array = X_processed.values

# Calculate distance matrix
distance_matrix = custom_distance(X_array)

# Initialize cntr randomly
n_samples = X_array.shape[0]
cntr = X_array[np.random.choice(n_samples, k, replace=False)]

# Perform Fuzzy C-means clustering
cntr, u, u0, d, jm, p, fpc = fuzz.cluster.cmeans(
    X_array.T,  # Note: skfuzzy expects features as rows, samples as columns
    k,
    m,
    error=0.005,
    maxiter=1000,
    init=None
)

# Convert output back to original orientation
cntr = cntr.T
u = u.T

# Get cluster assignments and membership scores
cluster_membership = np.argmax(u, axis=1)
data['Cluster'] = cluster_membership + 1  # 1-indexed clusters
data['Membership_Score'] = np.max(u, axis=1)

# Calculate cluster centers
cluster_centers = pd.DataFrame()
cluster_stats = pd.DataFrame()

for i in range(k):
    # Filter data for this cluster
    cluster_mask = data['Cluster'] == i+1
    cluster_data_i = data[cluster_mask]
    
    # Calculate statistics
    cluster_centers[f'Cluster_{i+1}'] = cluster_data_i[all_vars].mean()
    cluster_stats[f'Cluster_{i+1}_Size'] = [sum(cluster_mask)]
    cluster_stats[f'Cluster_{i+1}_Min'] = cluster_data_i[all_vars].min()
    cluster_stats[f'Cluster_{i+1}_Max'] = cluster_data_i[all_vars].max()
    cluster_stats[f'Cluster_{i+1}_Median'] = cluster_data_i[all_vars].median()

print("Clustering complete!")
print(f"Cluster sizes: {[sum(data['Cluster'] == i+1) for i in range(k)]}")

# Save clustering results
print("Saving clustering results...")
cluster_results_file = 'fuzzy_cmeans_cluster_results.csv'
data.to_csv(cluster_results_file, index=False)
print(f"Cluster results saved to {cluster_results_file}")

# Save cluster centers
cluster_centers_file = 'fuzzy_cmeans_cluster_centers.csv'
cluster_centers.to_csv(cluster_centers_file)
print(f"Cluster centers saved to {cluster_centers_file}")

# Create radar chart function
def create_radar_chart(data, group_name, variables, title=None):
    """Create a radar chart for cluster centers."""
    # Number of variables
    N = len(variables)
    
    # Format variable names
    formatted_vars = [var.split('_', 1)[1].replace('_', '\n') if '_' in var else var for var in variables]
    
    # Create angles for the radar chart
    angles = np.linspace(0, 2*np.pi, N, endpoint=False).tolist()
    
    # The plot needs to be closed
    angles += angles[:1]
    
    # Set up the figure
    fig, ax = plt.subplots(figsize=(10, 8), subplot_kw=dict(polar=True))
    
    # Add lines for each cluster
    colors = ['#4285F4', '#EA4335']  # Google colors for clusters 1 and 2
    for i in range(k):
        cluster_values = [data[f'Cluster_{i+1}'][var] for var in variables]
        # Close the plot
        cluster_values += cluster_values[:1]
        
        # Plot the cluster line
        ax.plot(angles, cluster_values, 'o-', linewidth=2, color=colors[i], 
                label=f'Cluster {i+1}', alpha=0.8)
        ax.fill(angles, cluster_values, color=colors[i], alpha=0.1)
    
    # Set y-limits for consistency
    if all(var in likert_vars for var in variables):
        ax.set_ylim(1, 5)  # Likert scale range
        plt.yticks(np.arange(1, 6), fontsize=8)
    else:
        # For organizational variables, use data-driven limits
        min_val = min([data[f'Cluster_{i+1}'][var] for i in range(k) for var in variables])
        max_val = max([data[f'Cluster_{i+1}'][var] for i in range(k) for var in variables])
        # Add some padding
        padding = (max_val - min_val) * 0.1
        ax.set_ylim(max(0, min_val - padding), max_val + padding)
    
    # Add variable labels
    formatted_vars += formatted_vars[:1]  # Complete the circle
    plt.xticks(angles, formatted_vars, fontsize=10)
    
    # Add title
    if title:
        plt.title(title, size=14, y=1.1)
    else:
        plt.title(f'Fuzzy C-means Clustering: {group_name} Variables', size=14, y=1.1)
    
    # Add legend
    plt.legend(loc='upper right', bbox_to_anchor=(0.1, 0.1))
    
    # Return the figure
    return fig, ax

# Generate radar charts
print("Generating radar charts...")

# 1. All variables radar chart
fig_all, ax_all = create_radar_chart(
    cluster_centers, 
    'All', 
    all_vars,
    'Fuzzy C-means Clustering (K=2): All Variables'
)
fig_all.savefig('fuzzy_cmeans_radar_all.png', bbox_inches='tight', dpi=300)
plt.close(fig_all)

# 2. Organization structure variables radar chart
fig_org, ax_org = create_radar_chart(
    cluster_centers, 
    'Organization Structure', 
    org_vars,
    'Fuzzy C-means Clustering (K=2): Organization Structure Variables'
)
fig_org.savefig('fuzzy_cmeans_radar_org.png', bbox_inches='tight', dpi=300)
plt.close(fig_org)

# 3. Group 1 variables radar chart (Decision & Style)
fig_group1, ax_group1 = create_radar_chart(
    cluster_centers, 
    'Decision & Style', 
    group1_vars,
    'Fuzzy C-means Clustering (K=2): Decision & Style Variables'
)
fig_group1.savefig('fuzzy_cmeans_radar_group1.png', bbox_inches='tight', dpi=300)
plt.close(fig_group1)

# 4. Group 2 variables radar chart (Culture, Flexibility & Environment)
fig_group2, ax_group2 = create_radar_chart(
    cluster_centers, 
    'Culture, Flexibility & Environment', 
    group2_vars,
    'Fuzzy C-means Clustering (K=2): Culture, Flexibility & Environment Variables'
)
fig_group2.savefig('fuzzy_cmeans_radar_group2.png', bbox_inches='tight', dpi=300)
plt.close(fig_group2)

# 5. Create a PDM distribution chart
def create_pdm_distribution_chart():
    pdm_distribution = pd.crosstab(
        data['Cluster'], 
        data['PDM_Selected'],
        normalize='index'
    ) * 100  # Convert to percentage
    
    # Define PDM order
    pdm_levels = [
        "Design-Bid-Build",
        "Construction Manager @ Risk",
        "Design-Build",
        "Progressive Design-Build",
        "Integrated Project Delivery (IPD)"
    ]
    
    # Reorder columns if they exist
    existing_pdms = [pdm for pdm in pdm_levels if pdm in pdm_distribution.columns]
    pdm_distribution = pdm_distribution[existing_pdms]
    
    # Create stacked bar chart
    fig, ax = plt.subplots(figsize=(12, 7))
    
    pdm_colors = {
        "Design-Bid-Build": "#D46A6A",             # Red (DBB)
        "Construction Manager @ Risk": "#E3C567",  # Yellow (CMAR)
        "Design-Build": "#9CCF9C",                 # Light green (DB)
        "Progressive Design-Build": "#4A8F4A",     # Dark green (PDB)
        "Integrated Project Delivery (IPD)": "#6A95CA" # Blue (IPD)
    }
    
    # Create a stacked bar for each cluster
    pdm_distribution.plot(
        kind='bar', 
        stacked=True,
        color=[pdm_colors.get(pdm, 'gray') for pdm in pdm_distribution.columns],
        ax=ax
    )
    
    # Add labels and title
    ax.set_xlabel('Cluster', fontsize=12)
    ax.set_ylabel('Percentage (%)', fontsize=12)
    ax.set_title('Project Delivery Method Distribution by Cluster', fontsize=14)
    
    # Add exact percentages as text
    for i, cluster in enumerate(pdm_distribution.index):
        bottom = 0
        for pdm in pdm_distribution.columns:
            if pdm in pdm_distribution.columns and not np.isnan(pdm_distribution.loc[cluster, pdm]):
                value = pdm_distribution.loc[cluster, pdm]
                if value > 0:  # Only add text if value is greater than 0
                    ax.text(i, bottom + value/2, f'{value:.1f}%', 
                            ha='center', va='center', fontsize=10, fontweight='bold')
                bottom += value
    
    # Add cluster sizes as text
    for i, cluster in enumerate(pdm_distribution.index):
        cluster_size = sum(data['Cluster'] == cluster)
        ax.text(i, 105, f'n={cluster_size}', ha='center', fontsize=12, fontweight='bold')
    
    # Customize legend with short names
    pdm_short_names = {
        "Design-Bid-Build": "DBB",
        "Construction Manager @ Risk": "CMAR",
        "Design-Build": "DB",
        "Progressive Design-Build": "PDB",
        "Integrated Project Delivery (IPD)": "IPD"
    }
    
    handles, labels = ax.get_legend_handles_labels()
    ax.legend(
        handles, 
        [pdm_short_names.get(label, label) for label in labels],
        title='Project Delivery Method',
        loc='upper right'
    )
    
    # Adjust y-axis
    ax.set_ylim(0, 110)  # Make room for cluster size text
    
    return fig, ax

# Create PDM distribution chart
fig_pdm, ax_pdm = create_pdm_distribution_chart()
fig_pdm.savefig('fuzzy_cmeans_pdm_distribution.png', bbox_inches='tight', dpi=300)
plt.close(fig_pdm)

print("Radar charts and PDM distribution chart created successfully!")
print("Analysis complete!") 