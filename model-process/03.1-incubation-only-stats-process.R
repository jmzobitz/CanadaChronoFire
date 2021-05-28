# This script analyes the stats for the incubation only data - we save the results for subsequent analysis and figure 3


library(CanadaFire)
library(tidyverse)

# Load up median parameters
load('estimate-results/stats-results/median-params-incubation.Rda')

my_params <- median_params_incubation %>%
  rename(params=data)

my_expressions <- model_expressions %>%
  select(model,incubation_expressions) %>%
  rename(expressions = incubation_expressions)

my_data <- combined_data %>% select(-field) %>%
  rename(data = incubation)

# Join these all together
my_inputs <- my_params %>%
  inner_join(my_data,by=c("Year","depth")) %>%
  inner_join(my_expressions,by="model")



incubation_median_model <- my_inputs %>%
  mutate(outputs=pmap(list(data,params,expressions),.f=~compute_model_respiration(..1,..2,..3)),
         n_params = map(.x=params,.f=~(.x %>%  summarize(value = n()) %>% pull(value)))) %>%
  select(Year,depth,model,approach,n_params,outputs)



### Make these into a plot
model_fits <- incubation_median_model %>%
  mutate(lm_fit = map(.x=outputs,.f=~lm(rModel~rSoil,data=.x))) %>%
  mutate(stats = map(.x=lm_fit,.f=~broom::glance(.x))) %>%
  mutate(coeffs = map(.x=lm_fit,.f=~broom::tidy(.x))) %>%
  select(-lm_fit) %>%
  mutate(taylor_values = map(.x=outputs,.f=~taylor_compute(.x$rSoil,.x$rModel))) %>%
  mutate(aic = map2(.x=outputs,.y=n_params,.f=~compute_aic(.x$rSoil,.x$rModel,.y)))


# OK, let's save these results - we will use them later!
save(model_fits,file='estimate-results/stats-results/taylor-incubation.Rda')



