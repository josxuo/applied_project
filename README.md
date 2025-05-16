# applied_project

## Study Objective 

The objective of this analysis was to estimate trends in species counts, defined as the number of individuals of a given species observed per survey, using long-term data from the Neighborhood Bird Project (NBP). Two levels of analysis were conducted: (1) across all active NBP parks to assess larger-scale trends, and (2) on a park-by-park basis to explore local dynamics. 

## Data Structure and Challenges 

The NBP dataset contains more than 200,000 bird count observations from fixed survey points at ten Seattle-area greenspaces. Survey effort varies among NBP locations and years. Very few survey sites have complete data for every month of all years. In most cases, sites are missing one or more months of survey data per year.  

Because bird detection probabilities vary substantially over the course of the year (due to migration, nesting behavior, etc.) and across sites (for example, waterbirds much less likely to be observed at forested sites than sites near waterbodies), aggregating data across time or sites could introduce bias. To minimize this risk, we modeled individual survey counts rather than aggregating data over broader temporal or spatial scales. 

Some species in the NBP dataset were observed only once or a few times. Reliable trend estimates are difficult for species with such few observations. To ensure enough temporal spread for robust trend estimates, we only assessed species observed in ten or more years. 

Reliable estimation of count trends also relies on the assumption that detection probability remains constant across observers and over time. Unfortunately, this assumption is unlikely to hold for NBP data. Group sizes and the bird identification skills of surveyors may vary considerably between survey teams and throughout the study period. These differences are not consistently documented in the dataset, limiting our ability to control for them. 

As a result, observed changes in counts could reflect factors such as changes in auditory or visual acuity of surveyors over time, or smaller group sizes during surveys, rather than true population changes. 

That said, we expect the assumption of constant detection probability to be more reasonable for species that are conspicuous and easily identified—such as American Robins, American Crows, and Steller’s Jays—than for more cryptic or difficult-to-identify species. 

## Statistical Modeling Approach 

We used a generalized linear mixed models (GLMM) to estimate count trends. Each model included: 

Year as a continuous fixed effect to estimate long-term temporal trends. This term was scaled and centered (z-standardized) to improve model convergence. 

Month as a categorical fixed effect to control for seasonal variability in bird activity and detectability. 

Site as a random intercept to account for unmeasured variation in habitat and other site-level factors that influence bird counts. 

Given that count data were overdispersed relative to the Poisson distribution, we used the Negative Binomial 2 (NB2) distribution for the response variable. In the NB2 parameterization, the variance increases quadratically with the mean, making it better suited to handle the observed variability in the data. 

## Implementation 

Models were fit using the glmmTMB package in R. Separate models were fit for each species to estimate species-specific trends. To estimate overall trends, data from all active parks were included in a single model. For park-level analyses, models were subset by park. Only species with observations in at least ten years were included.  

Trend classification 

To classify species trends, we used the model coefficient associated with the z-standardized year variable. We adjusted this coefficient based on the original spread of the data and exponentiated it to express annual changes in abundance (“bkt_beta_yr”). Because bkt_beta_yr represents the annual change factor (with 1.00 meaning no change), we interpreted values ≥1.05 an Increasing trend and values ≤0.95 as a Decreasing trend; values between 0.95 and 1.05 were deemed Stable.  

Increasing: bkt_beta_yr ≥ 1.05 

Decreasing: bkt_beta_yr ≤ 0.95 

Stable: 0.95 < bkt_beta_yr < 1.05 

## Confidence classification 

We assessed confidence in each modeled trend using three diagnostic measures: the p-value of the year effect (p_yr), the model pseudo-R² (r2), and the dispersion parameter (disp). Based on these, we assigned High, Moderate, or Low confidence as follows: 

High confidence: p_yr ≤ 0.01, r2 ≥ 0.3, and disp ≤ 1.5 

Moderate confidence: p_yr ≤ 0.1, r2 ≥ 0.1, and disp ≤ 2 

Low confidence: all other cases 

High-confidence trends require a very significant year effect, a relatively strong model fit, and near-ideal dispersion.  

Moderate confidence allows a more lenient significance cutoff and modest model fit and tolerates slightly more overdispersion.  

Trends failing to meet the high or moderate criteria were marked as low confidence.  

To simplify presentation of results, high and moderate confidence estimates were lumped into a single category “Confident”. Low confidence estimates were renamed “Not confident”.  
