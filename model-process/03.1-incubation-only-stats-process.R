# This script analyes the stats for the incubation only data - we save the results for subsequent analysis and figure 3


library(CanadaFire)
library(tidyverse)

# Load up the estimated results parameters
load('estimate-results/incubation-linear-approach-data.Rda')

my_results <- estimate_data_linear %>%
  group_by(Year,depth,model) %>%
  nest()

my_expressions <- model_expressions %>%
  select(model,incubation_expressions) %>%
  rename(expressions = incubation_expressions)

my_data <- combined_data %>% select(-field) %>%
  rename(data = incubation)


incubation_median_model <- summarize_median_soil_respiration(my_results,my_expressions,my_data,"incubation")



# A quick function that computes the AIC
compute_aic <- function(in_data,n_params) {
  n_obs <- dim(in_data)[1]
  model <- in_data$rModel
  observations <- in_data$rSoil
  ll <- -n_obs*(log(2*pi)+1+log((sum((model-observations)^2)/n_obs)))/2
  AIC <- -2*ll + 2*n_params

}




### Make these into a plot
model_fits <- incubation_median_model %>%
  mutate(lm_fit = map(.x=outputs,.f=~lm(rModel~rSoil,data=.x))) %>%
  mutate(stats = map(.x=lm_fit,.f=~broom::glance(.x))) %>%
  mutate(coeffs = map(.x=lm_fit,.f=~broom::tidy(.x))) %>%
  select(-lm_fit) %>%
  mutate(taylor_values = map(.x=outputs,.f=~taylor_compute(.x$rSoil,.x$rModel))) %>%
  mutate(aic = map2(.x=outputs,.y=n_params,.f=~compute_aic(.x,.y)))


# OK, let's save these results - we will use them later!
save(model_fits,file='estimate-results/stats-results/taylor-incubation.Rda')



