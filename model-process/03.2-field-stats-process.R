# This script analyes the stats for the field data - we save the results for subsequent analysis and figure 4

library(tidyverse)
library(CanadaFire)

# load up the data



# Load and process the parameters
### Approach 1: Field
load('estimate-results/field-approach-results.Rda')

my_results <- rss_filter(field_approach_results) %>%
  select(Year,depth,model,rss_filter) %>%
  rename(data = rss_filter )

my_expressions <- model_expressions %>%
  select(model,field_expressions) %>%
  rename(expressions = field_expressions)

my_data <- combined_data %>% select(-incubation) %>%
  rename(data = field)

field_median_model <- summarize_median_soil_respiration(my_results,my_expressions,my_data,"field")


### Approach 2: Field - Linear
load('estimate-results/field-linear-approach-results.Rda')

my_results <- rss_filter(field_linear_approach_results) %>%
  select(Year,depth,model,rss_filter) %>%
  rename(data = rss_filter )

my_expressions <- model_expressions %>%
  select(model,field_linear_expressions) %>%
  rename(expressions = field_linear_expressions)

my_data <- combined_data %>% select(-incubation) %>%
  rename(data = field)


field_linear_median_model <- summarize_median_soil_respiration(my_results,my_expressions,my_data,"field-linear")

### Approach 3: Incubation Field
# We don't need to do any filtering on the approach - just unnest and compute statistics

load('estimate-results/incubation-field-approach-results.Rda')

my_results <- incubation_field_approach_results %>%
  group_by(Year,depth,model) %>%
  nest()

my_expressions <- model_expressions %>%
  select(model,field_expressions) %>%
  rename(expressions = field_expressions)

my_data <- combined_data %>% select(-incubation) %>%
  rename(data = field)


incubation_field_median_model <- summarize_median_soil_respiration(my_results,my_expressions,my_data,"incubation-field")


### Approach 4: Incubation Field Linear
# We don't need to do any filtering on the approach - just unnest and compute statistics

load('estimate-results/incubation-field-linear-approach-results.Rda')

my_results <- incubation_field_linear_approach_results %>%
  group_by(Year,depth,model) %>%
  nest()

my_expressions <- model_expressions %>%
  select(model,incubation_field_linear_expressions) %>%
  rename(expressions = incubation_field_linear_expressions)

my_data <- combined_data %>% select(-incubation) %>%
  rename(data = field)

incubation_field_linear_median_model <- summarize_median_soil_respiration(my_results,my_expressions,my_data,"incubation-field-linear")

#### OK, now we can compute things
out_results <- rbind(field_median_model,field_linear_median_model,incubation_field_median_model,incubation_field_linear_median_model)

# A quick function that computes the AIC
compute_aic <- function(in_data,n_params) {
  n_obs <- dim(in_data)[1]
  model <- in_data$rModel
  observations <- in_data$rSoil
  ll <- -n_obs*(log(2*pi)+1+log((sum((model-observations)^2)/n_obs)))/2
  AIC <- -2*ll + 2*n_params

}



# This is a long nested list that computes model stats and linear coeff, w/ AIC!
model_fits <- out_results %>%
  mutate(lm_fit = map(.x=outputs,.f=~lm(rModel~rSoil,data=.x))) %>%
  mutate(stats = map(.x=lm_fit,.f=~broom::glance(.x))) %>%
  mutate(coeffs = map(.x=lm_fit,.f=~broom::tidy(.x))) %>%
  select(-lm_fit) %>%
  mutate(taylor_values = map(.x=outputs,.f=~taylor_compute(.x$rSoil,.x$rModel))) %>%
  mutate(aic = map2(.x=outputs,.y=n_params,.f=~compute_aic(.x,.y)))




# OK, let's save these results - we will use them later!
save(model_fits,file='estimate-results/stats-results/taylor-field.Rda')

