# This script analyes the stats for the field data - we save the results for subsequent analysis and figure 4

library(tidyverse)
library(CanadaFire)




# Load up median parameters
load('estimate-results/stats-results/median-params-field.Rda')

my_params <- median_params_field %>%
  rename(params=data)

my_data <- combined_data %>% select(-incubation) %>%
  rename(data = field)


# We need to do some cleanup on the respiration expressions
my_expressions <- respiration_expressions %>%
  select(model,rSoil_expression,rSoil_incubation_field_linear_expression,rSoil_field_linear_expression) %>%
  mutate(incubation_field = rSoil_expression) %>%
  rename(field = rSoil_expression,
         field_linear = rSoil_field_linear_expression,
         incubation_field_linear = rSoil_incubation_field_linear_expression) %>%
  pivot_longer(cols=c(-"model"),names_to="approach",values_to="expressions") %>%
  mutate(approach = str_replace_all(approach,"_","-"))  # Replace string names



# Join these all together
my_inputs <- my_params %>%
  inner_join(my_data,by=c("Year","depth")) %>%
  inner_join(my_expressions,by=c("model","approach"))


# Now we can go and compute these!

field_median_model <- my_inputs %>%
  mutate(outputs=pmap(list(data,params,expressions),.f=~compute_model_respiration(..1,..2,..3)),
         n_params = map(.x=params,.f=~(.x %>%  summarize(value = n()) %>% pull(value)))) %>%
  select(Year,depth,model,approach,n_params,outputs)



### Make these into a plot
model_fits <- field_median_model %>%
  mutate(lm_fit = map(.x=outputs,.f=~lm(rModel~rSoil,data=.x))) %>%
  mutate(stats = map(.x=lm_fit,.f=~broom::glance(.x))) %>%
  mutate(coeffs = map(.x=lm_fit,.f=~broom::tidy(.x))) %>%
  select(-lm_fit) %>%
  mutate(taylor_values = map(.x=outputs,.f=~taylor_compute(.x$rSoil,.x$rModel))) %>%
  mutate(aic = map2(.x=outputs,.y=n_params,.f=~compute_aic(.x$rSoil,.x$rModel,.y)))





# OK, let's save these results - we will use them later!
save(model_fits,file='estimate-results/stats-results/taylor-field.Rda')

