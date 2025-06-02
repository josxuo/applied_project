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

An observation-level random effect to account for additional unmeasured variation that could be related to day, observers, bird species, or other factors that influence bird counts. 

Prior to adding an observation-level random effect, the count data were overdispersed relative to the Poisson distribution, and we used the Negative Binomial 2 (NB2) distribution for the response variable. In the NB2 parameterization, the variance increases quadratically with the mean, making it better suited to handle the observed variability in the data. 

Adding the observation-level random effect helped address overdispersion. Dispersion statistics for models assuming a Poisson distribution with both observation-level and site-level random effects were reasonable. 

## Implementation 

Models were fit using the glmmTMB package in R. Separate models were fit for each species to estimate species-specific trends. To estimate overall trends, data from all active parks were included in a single model. For park-level analyses, models were subset by park. Only species with observations in at least ten years were included.  

## Presenting Results

Mean trend estimates and 95% confidence intervals were reported for 112 species. All trends are reported, regardless of statistical signficance. Estimates with wide confidence intervals, especially those that include zero, should be interpreted cautiously, if at all.
