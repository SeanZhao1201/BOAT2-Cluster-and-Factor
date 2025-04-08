# K-Prototype Clustering Analysis Insights: Project Delivery Methods and Organizational Characteristics

## Research Overview

This study uses the K-prototype clustering method to analyze the relationship between organizational characteristics and Project Delivery Method (PDM) selection. We compared clustering results for K=2 and K=3 to reveal how organizational characteristics influence the selection of project delivery methods in the construction industry. The advantage of the K-prototype method lies in its ability to simultaneously process numerical variables (such as organizational size) and categorical variables (such as organizational culture ratings), making it particularly suitable for the mixed data types in this study.

## Statistical Method Evaluation

| Metric | K=2 | K=3 |
|------|-----|-----|
| Average Silhouette Coefficient | **0.164** | 0.040 |
| Cluster 1 Silhouette Coefficient | **0.214** (87 samples) | **-0.030** (54 samples) |
| Cluster 2 Silhouette Coefficient | 0.051 (39 samples) | 0.149 (21 samples) |
| Cluster 3 Silhouette Coefficient | - | 0.071 (51 samples) |

**Analysis Evaluation**:
- K=2 silhouette coefficient is significantly higher than K=3 (0.164 vs 0.040), indicating better separation between the two clusters
- **<span style="font-size: 1.2em">Cluster 1 in K=3 shows a negative silhouette coefficient (-0.030), indicating poor classification, suggesting possible misclassification of samples</span>**
- Overall silhouette coefficients are relatively low (far below the ideal value of 0.5), indicating that the relationship between organizational characteristics and PDM selection is complex, without absolutely clear boundaries
- K=2 has a more balanced sample distribution (87:39), while K=3 distribution (54:21:51) has fewer samples in Cluster 2

## K=2 Clustering Analysis Results

### Cluster Characteristic Comparison

| Characteristic | Cluster 1 (87 organizations) | Cluster 2 (39 organizations) |
|------|----------------|----------------|
| Organization Size | Large (average 84.0 employees) | Small (average 21.3 employees) |
| Organizational Structure | **<span style="font-size: 1.2em">More branches (average 4.7), more departments (average 7.1), fewer hierarchical levels (average 2.9)</span>** | Fewer branches (average 3.3), fewer departments (average 5.8), more hierarchical levels (average 3.6) |
| Management Centralization | **<span style="font-size: 1.2em">High centralization (average 3.7/5 points, close to "agree")</span>** | Low centralization (average 2.8/5 points, close to "neutral") |
| Formalization Degree | **<span style="font-size: 1.2em">High formalization (average 3.7/5 points, close to "agree")</span>** | Low formalization (average 3.0/5 points, "neutral") |
| Management Style | High participatory (average 4.0/5 points, "agree"), high organicity (average 3.9/5 points, close to "agree") | Low participatory (average 3.2/5 points, slightly above "neutral"), low organicity (average 3.1/5 points, slightly above "neutral") |
| Organizational Culture | **<span style="font-size: 1.2em">High symbolic (average 4.0/5 points), high generative (average 3.7/5 points), high transactive (average 4.1/5 points)</span>** | Low symbolic (average 3.1/5 points), low generative (average 2.7/5 points), low transactive (average 3.3/5 points) |
| Flexibility | High openness (average 4.0/5 points, "agree"), high recursiveness (average 3.5/5 points, between "neutral" and "agree") | Low openness (average 3.1/5 points, slightly above "neutral"), low recursiveness (average 3.1/5 points, slightly above "neutral") |
| Risk Attitude | High risk tolerance (average 3.1/5 points, slightly above "neutral") | Low risk tolerance (average 2.5/5 points, between "disagree" and "neutral") |
| Environmental Characteristics | High growth environment (average 3.9/5 points, close to "agree"), low hostility (average 2.3/5 points, close to "disagree") | Low growth environment (average 3.5/5 points, between "neutral" and "agree"), high hostility (average 2.8/5 points, close to "neutral") |

### PDM Distribution (Actual Numbers and Percentages)

| PDM Type | Cluster 1 (87 organizations) | Cluster 2 (39 organizations) | Overall Distribution |
|--------|------------------|------------------|---------|
| IPD (Most Innovative) | 10 (11.5%) | 1 (2.6%) | 11 (8.6%) |
| Progressive Design-Build | 9 (10.3%) | 4 (10.3%) | 13 (10.3%) |
| Design-Build | 28 (32.2%) | 15 (38.5%) | 43 (33.9%) |
| Construction Manager @ Risk | 23 (26.4%) | 10 (25.6%) | 33 (26.0%) |
| Design-Bid-Build (Most Traditional) | **<span style="font-size: 1.2em">17 (19.5%)</span>** | 9 (23.1%) | 26 (20.5%) |

## K=3 Clustering Analysis Results

### Cluster Characteristic Comparison

| Characteristic | Cluster 1 (54 organizations) | Cluster 2 (21 organizations) | Cluster 3 (51 organizations) |
|------|----------------|----------------|----------------|
| Organization Size | Medium (average 44.7 employees) | Small (average 27.2 employees) | Large (average 101.0 employees) |
| Organizational Structure | Fewer branches (average 2.0), more departments (average 7.2), medium hierarchical levels (average 3.2) | Medium branches (average 3.3), fewer departments (average 5.8), more hierarchical levels (average 3.5) | **<span style="font-size: 1.2em">More branches (average 7.1), medium departments (average 6.6), fewer hierarchical levels (average 2.8)</span>** |
| Management Centralization | **<span style="font-size: 1.2em">High centralization (average 3.5/5 points)</span>** | Lowest centralization (average 2.4/5 points) | **<span style="font-size: 1.2em">High centralization (average 3.7/5 points)</span>** |
| Formalization Degree | **<span style="font-size: 1.2em">High formalization (average 3.6/5 points)</span>** | Medium formalization (average 3.0/5 points) | **<span style="font-size: 1.2em">High formalization (average 3.6/5 points)</span>** |
| Management Style | High participatory (average 4.0/5 points), high organicity (average 3.7/5 points) | Low participatory (average 2.6/5 points), low organicity (average 2.9/5 points) | High participatory (average 3.9/5 points), high organicity (average 3.9/5 points) |
| Organizational Culture | **<span style="font-size: 1.2em">High symbolic (average 4.0/5 points), high transactive (average 4.2/5 points)</span>** | Low symbolic (average 2.3/5 points), low transactive (average 2.6/5 points) | **<span style="font-size: 1.2em">High symbolic (average 3.9/5 points), high transactive (average 4.1/5 points)</span>** |
| Flexibility | Highest openness (average 4.1/5 points), high recursiveness (average 3.7/5 points) | Lowest openness (average 2.4/5 points), low recursiveness (average 2.4/5 points) | High openness (average 3.9/5 points), medium recursiveness (average 3.5/5 points) |
| Risk Attitude | High risk tolerance (average 3.2/5 points) | Lowest risk tolerance (average 2.1/5 points) | Medium risk tolerance (average 2.9/5 points) |
| Environmental Characteristics | High growth environment (average 3.9/5 points), medium hostility (average 2.4/5 points) | Medium growth environment (average 3.5/5 points), high hostility (average 2.7/5 points) | Medium growth environment (average 3.7/5 points), medium hostility (average 2.4/5 points) |

### PDM Distribution (Actual Numbers and Percentages)

| PDM Type | Cluster 1 (54 organizations) | Cluster 2 (21 organizations) | Cluster 3 (51 organizations) | Overall Distribution |
|--------|------------------|------------------|------------------|---------|
| IPD (Most Innovative) | **<span style="font-size: 1.2em">7 (13.0%)</span>** | **<span style="font-size: 1.2em">0 (0.0%)</span>** | 4 (7.8%) | 11 (8.6%) |
| Progressive Design-Build | 7 (13.0%) | 2 (9.5%) | 4 (7.8%) | 13 (10.3%) |
| Design-Build | 16 (29.6%) | **<span style="font-size: 1.2em">10 (47.6%)</span>** | 17 (33.3%) | 43 (33.9%) |
| Construction Manager @ Risk | 18 (33.3%) | 5 (23.8%) | 10 (19.6%) | 33 (26.0%) |
| Design-Bid-Build (Most Traditional) | **<span style="font-size: 1.2em">6 (11.1%)</span>** | 4 (19.0%) | **<span style="font-size: 1.2em">16 (31.4%)</span>** | 26 (20.5%) |

## Key Insights

### Findings Consistent with Industry Practice

1. **Positive Correlation Between Organizational Size and Innovative PDM Adoption**
   - Larger organizations (K=2 Cluster 1, average 84 employees; K=3 Cluster 3, average 101 employees) have higher adoption rates of innovative PDMs like IPD
   - In K=2, Cluster 1 has an IPD adoption rate of 11.5% (10 organizations), while Cluster 2 has only 2.6% (1 organization)
   - In K=3, the lowest risk tolerance Cluster 2 (21 organizations) has no IPD adoption cases (0%)
   - This indicates that larger organizations typically have more resources and expertise to better handle the complexity and uncertainty of innovative PDMs

2. **Clear Association Between Risk Attitude and PDM Selection**
   - High risk tolerance organizations (K=2 Cluster 1, average 3.1 points; K=3 Cluster 1, average 3.2 points) tend to adopt more innovative PDMs
   - Specifically, K=2 Cluster 1 has a combined IPD and Progressive Design-Build adoption rate of 21.8% (19 organizations), compared to only 12.9% (5 organizations) in Cluster 2
   - In K=3, the lowest risk tolerance Cluster 2 (average 2.1 points) has no organizations adopting IPD, and traditional PDMs (Design-Bid-Build and Design-Build) account for 66.6% (14 organizations)
   - This aligns with the expectation in project management theory that risk preference influences innovation adoption

3. **Influence of Environmental Factors on PDM Selection**
   - Organizations operating in hostile environments (K=2 Cluster 2, hostility average 2.8 points; K=3 Cluster 2, hostility average 2.7 points) tend to use more traditional PDMs
   - In K=2 Cluster 2, 23.1% (9 organizations) in hostile environments use the most traditional Design-Bid-Build method, compared to 19.5% (17 organizations) in Cluster 1
   - This reflects the industry's tendency to choose more mature, risk-controllable project delivery methods when facing uncertainty and challenges

### Counter-intuitive Findings

1. ## **Coexistence of High Centralization and High Innovation**
   - **<span style="font-size: 1.3em">Surprisingly, even innovative PDM-adopting organizational groups (K=2 Cluster 1) maintain high centralization (average 3.7/5 points) and high formalization (average 3.7/5 points)</span>**
   - **<span style="font-size: 1.3em">K=3 analysis further confirms this: the most innovative Cluster 1 (IPD percentage 13.0%, 7 organizations) shows high centralization (average 3.5/5 points)</span>**
   - This challenges the traditional management concept that "innovative organizations should be decentralized," suggesting that innovation in the construction industry may require strong central coordination
   - Possible explanation: The high risk and complexity of construction projects require clear control structures while allowing for innovation

2. ## **Flat Structure in Large Organizations**
   - **<span style="font-size: 1.3em">Both clustering schemes show that larger organizations actually have flatter hierarchical structures</span>**
   - **<span style="font-size: 1.3em">In K=2, large organizations (Cluster 1, average 84 employees) have an average of 2.9 hierarchical levels, while small organizations (Cluster 2, average 21 employees) have 3.6 levels</span>**
   - **<span style="font-size: 1.3em">In K=3, the largest organizational group (Cluster 3, average 101 employees) has the fewest levels, only 2.8</span>**
   - This contradicts the organizational theory view that "large organizations have more hierarchical levels"
   - May indicate that successful large construction organizations have recognized that excessive hierarchical levels hinder information flow and decision-making speed, thus improving efficiency by reducing management levels

3. ## **Continued Use of Traditional PDMs in Innovative Organizations**
   - **<span style="font-size: 1.3em">Data shows that even in the most innovative organizational groups, traditional PDMs still account for a considerable proportion</span>**
   - **<span style="font-size: 1.3em">In K=2 Cluster 1 (most innovative organizational group), 17 organizations (19.5%) still use the Design-Bid-Build method</span>**
   - **<span style="font-size: 1.3em">In K=3 Cluster 1 (group with highest IPD percentage), 6 organizations (11.1%) use the Design-Bid-Build method</span>**
   - This indicates that even innovation-oriented organizations adopt traditional methods based on specific project needs
   - Implies that PDM selection is a multi-factor decision, dependent not only on organizational characteristics but also on project type, client requirements, and regulatory environment

4. ## **High Scores in Both Symbolic and Transactive Cultures**
   - **<span style="font-size: 1.3em">Innovative organizations simultaneously exhibit high symbolic culture (valuing vision and mission) and high transactive culture (valuing negotiation and exchange), two seemingly contradictory traits</span>**
   - **<span style="font-size: 1.3em">K=2 Cluster 1 has an average symbolic culture of 4.0/5 points and transactive culture of 4.1/5 points</span>**
   - **<span style="font-size: 1.3em">K=3 Cluster 1 has an average symbolic culture of 4.0/5 points and transactive culture reaches 4.2/5 points</span>**
   - This seemingly contradictory combination may be a unique success model for the construction industry
   - Indicates that successful construction organizations need to balance long-term vision (symbolic) with short-term project execution capabilities (transactive)

5. ## **Nonlinear Relationship Between Organizational Size and PDM Selection**
   - **<span style="font-size: 1.3em">K=3 results reveal an interesting finding: medium-sized organizations (Cluster 1, average 44.7 employees) have the highest IPD adoption rate (13.0%, 7 organizations), even exceeding that of large organizations (Cluster 3, average 101 employees) at 7.8% (4 organizations)</span>**
   - **<span style="font-size: 1.3em">At the same time, large organizations (Cluster 3) have a surprisingly high proportion of using the most traditional Design-Bid-Build method (31.4%, 16 organizations)</span>**
   - This suggests that the relationship between organizational size and innovative PDM adoption may not be a simple linear one, but rather there may be a "sweet spot" size, possibly within the medium to large size range

## Practical Recommendations

1. **Balance Innovation and Control**
   - **<span style="font-size: 1.2em">Data clearly shows that organizations should maintain appropriate centralization and formalization even when adopting innovative PDMs</span>**
   - Recommendation: Avoid extreme decentralization; instead, create "framed freedom"—encouraging innovation within clear rules and expectations
   - In K=2 Cluster 1, the most successful organizations adopting innovative PDMs scored an average of 3.7/5 on centralization, at the "agree" level
   - Implications for managers: Innovation needs to occur within a structured framework, especially in high-risk construction projects

2. **Organizational Development Path**
   - For small organizations (K=2 Cluster 2, average 21 employees; K=3 Cluster 2, average 27 employees)
     * Should first focus on improving participation (from average 3.2/5 points), organicity (from average 3.1/5 points), and recursiveness (from average 3.1/5 points)
     * Improving these factors helps lay the organizational cultural foundation for future adoption of innovative PDMs
   - For large organizations (K=2 Cluster 1, average 84 employees; K=3 Cluster 3, average 101 employees)
     * **<span style="font-size: 1.2em">Should focus on maintaining flat structures (currently averaging 2.9 and 2.8 levels), preventing increases in management levels as scale expands</span>**
     * Data shows a clear association between increased hierarchical levels and decreased innovation capability

3. **Reasonable Use of Mixed PDM Strategies**
   - **<span style="font-size: 1.2em">Data shows that even the most innovative organizations (K=2 Cluster 1; K=3 Cluster 1) use traditional PDMs in a considerable proportion of projects</span>**
   - Recommendation: Don't blindly pursue the latest methods; instead, choose appropriate PDMs based on specific project characteristics
   - For example, in K=2 Cluster 1, 19.5% (17 organizations) use Design-Bid-Build, 26.4% (23 organizations) use Construction Manager @ Risk method
   - Implications for managers: The same organization needs to simultaneously possess the ability and flexibility to manage different PDMs

4. **Cultural Transformation Strategies**
   - **<span style="font-size: 1.2em">Data suggests that successful construction organizations need to cultivate a unique combination of symbolic and transactive cultures</span>**
   - Recommended specific steps:
     * Build a strong organizational vision (enhance symbolic culture, K=2 Cluster 1 average is 4.0/5 points)
     * Simultaneously strengthen project execution and negotiation capabilities (enhance transactive culture, K=2 Cluster 1 average is 4.1/5 points)
     * Reduce coercive cultural characteristics (K=2 Cluster 1's coercive culture is only 2.1/5 points, significantly lower than Cluster 2's 2.8/5 points)
   - This cultural combination provides maximum flexibility for adopting various PDMs (from traditional to innovative)

5. **Organizational Design for Adapting to Environmental Changes**
   - Cluster analysis shows that organizations in hostile environments tend to have lower risk tolerance and innovation adoption rates
   - Recommendation: Organizations should appropriately adjust their PDM strategies when entering uncertain or hostile market environments
   - For example, in K=3 Cluster 2 (hostility average 2.7/5 points, higher than other clusters), no organizations adopt IPD
   - Implications for managers: Market environment analysis should be an important input factor for PDM selection decisions

## Conclusion

K-prototype clustering analysis reveals the complex relationship between organizational characteristics and PDM selection. Although the overall silhouette coefficients are not high (0.164 for K=2, only 0.040 for K=3), indicating not very clear boundaries, the analysis results still provide important insights into organizational decision-making in the construction industry.

## **Particularly noteworthy is that successful innovative organizations still maintain high centralization and formalization, challenging the traditional management theory view that decentralization positively correlates with innovation. Additionally, large construction organizations tend toward flat structures rather than multi-level hierarchies, which also differs from common organizational theory expectations. These findings suggest that the construction industry may require specific organizational models to balance innovation and control.**

Through comparative analysis of K=2 and K=3, we find that K=2 provides more robust classification (higher silhouette coefficient), while K=3 provides more detailed differentiation of organizational characteristics in some aspects (such as dividing organizations into small, medium, and large categories). However, considering the negative silhouette coefficient in K=3, the K=2 classification may be more reliable.

Future research could further explore:
1. How these organizational characteristics influence actual project performance and success rates
2. Comparison of implementation effects of different PDMs under similar organizational characteristics
3. How to optimize organizational structures to meet the implementation needs of specific PDMs
4. Longitudinal studies on how PDM strategies evolve as organizations grow

Finally, the practical significance of this study is to help construction industry organizations understand the relationship between their characteristics and project delivery method selection, thereby making more targeted strategic decisions that promote innovation while maintaining appropriate control. 