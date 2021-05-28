# This script analyes the stats for the field data - we save the results for subsequent analysis and figure 4

library(tidyverse)
library(CanadaFire)

# load up the data

### Approach 0: Incubation
load('estimate-results/incubation-only-results.Rda')

my_results <- rss_filter(incubation_results) %>%
  select(Year,depth,model,rss_filter) %>%
  rename(data = rss_filter )

median_params_incubation <- summarize_median_parameters(my_results,'incubation')


# Load and process the parameters
### Approach 1: Field
load('estimate-results/field-approach-results.Rda')

my_results <- rss_filter(field_approach_results) %>%
  select(Year,depth,model,rss_filter) %>%
  rename(data = rss_filter )

params_field <- summarize_median_parameters(my_results,'field')


### Approach 2: Field - Linear
load('estimate-results/field-linear-approach-results.Rda')

my_results <- rss_filter(field_linear_approach_results) %>%
  select(Year,depth,model,rss_filter) %>%
  rename(data = rss_filter )

params_field_linear <- summarize_median_parameters(my_results,'field-linear')


### Approach 3: Incubation Field
# We don't need to do any filtering on the approach - just unnest and compute statistics

load('estimate-results/incubation-field-approach-results.Rda')

my_results <- incubation_field_approach_results %>%
  group_by(Year,depth,model) %>%
  nest()

params_incubation_field <- summarize_median_parameters(my_results,'incubation-field')


### Approach 4: Incubation Field Linear
# We don't need to do any filtering on the approach - just unnest and compute statistics
# This one is a little trickier - like the incubation field approach we need to compute rH, which comes from the mean parameters.
load('estimate-results/incubation-field-linear-approach-results.Rda')

my_results <- incubation_field_linear_approach_results %>%
  group_by(Year,depth,model) %>%
  nest()

params_incbuation_field_linear <- summarize_median_parameters(my_results,'incubation-field-linear')



#### OK, now we can compute things
median_params_field <- rbind(params_field,params_field_linear,params_incubation_field,params_incbuation_field_linear)

# OK, let's save these results - we will use them later!
save(median_params_field,file='estimate-results/stats-results/median-params-field.Rda')
save(median_params_incubation,file='estimate-results/stats-results/median-params-incubation.Rda')

