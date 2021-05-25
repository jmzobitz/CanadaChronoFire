# This script analyes the stats for the incubation only data - we save the results for subsequent analysis and figure 3


library(CanadaFire)
library(tidyverse)

# Load up the estimated results parameters
load('estimate-results/incubation-only-results.Rda')

# Now we need to determine the summary distribution, first filtering out by the RSS any duplicates and between the 25th - 75 percentile

parameter_summary <- rss_filter(incubation_results) %>%
  select(Year,depth,model,rss_filter) %>%
  mutate(summary_param = map(.x=rss_filter, .f=~(.x$params %>%
                                                   bind_rows() %>%
                                                   group_by(name) %>%
                                                   nest() %>%
                                                   mutate(fivenum = map(.x=data,.f=~(summary(.x$value) %>% enframe()) )   ) %>%
                                                   mutate(conf_int =  map(.x=data,.f=~(tibble(ci_value=quantile(.x$value, c(0.025, 0.5, 0.975)), quantile = c("q0.025", "q0.5", "q0.975")))  ) )))) %>% select(Year,depth,model,summary_param) %>%
  unnest(cols=summary_param) %>%
  select(-data) %>% group_by(Year,depth,model) %>%
  nest()


# Compute the median parameter values
median_params <- parameter_summary %>%
  unnest(col=c(data)) %>%
  unnest(col=conf_int) %>%
  filter(quantile=="q0.5") %>%
  select(Year,depth,model,name,ci_value) %>%
  group_by(Year,depth,model) %>%
  nest()

# Load up the expressions to compute respiration:

model_fn_inputs <-median_params %>%
  inner_join(model_expressions,by=c("model")) %>%
  inner_join(combined_data,by=c("Year","depth")) %>%
  rename(params = data)

# Compute the summary values and then we are ready to compute the taylor diagrams!
model_values_incubation <- model_fn_inputs %>%
  mutate(incubation_outputs = pmap(.l=list(incubation,params,incubation_expressions),.f=~compute_model_respiration(..1,..2,..3)) ) %>%
  mutate(n_params = map(.x=params,.f=~(.x %>%  summarize(value = n()) %>% pull(value)))) %>%
  select(Year,depth,model,incubation_outputs,n_params)


# This is a long nested list that computes model stats and linear coeff, w/ AIC!
model_fits <- model_values_incubation %>%
  mutate(lm_fit = map(.x=incubation_outputs,.f=~lm(rModel~rSoil,data=.x))) %>%
  mutate(stats = map(.x=lm_fit,.f=~broom::glance(.x))) %>%
  mutate(coeffs = map(.x=lm_fit,.f=~broom::tidy(.x))) %>%
  select(-lm_fit) %>%
  mutate(taylor_values = map(.x=incubation_outputs,.f=~taylor_compute(.x$rSoil,.x$rModel))) %>%
  mutate(aic = map2(.x=stats,.y=n_params,.f=~(unlist(-2*.x$logLik+2*.y))))


# OK, let's save these results - we will use them later!
save(model_fits,file='estimate-results/stats-results/taylor-incubation.Rda')

save(median_params,parameter_summary,file='estimate-results/stats-results/summary-parameter-incubation.Rda')


