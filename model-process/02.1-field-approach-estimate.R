# This computes the model estimates for the field approach - in this case parameters are only estimated using the field data

# Prepare the model expressions

library(nlsr)
library(tidyverse)
library(CanadaFire)  # Load up the package


# For this scenario we are only estimating with field data and ignoring the incubation data, so we need to overwrite in the field params the estimate variable with model_estimate (which says are you in the model or not) just so we can estimate data., so if it is in the model we then estimate it

estimate_data_revised <- estimate_data %>%
  mutate(field_params = map2(.x=field_params,.y=model_estimate,.f=~(mutate(.x,estimate=.y$estimate))))

# Define the number of iterations
n_iter <- 1000

# Define the out vector of data
out_list <- vector("list",length=n_iter)


# Just loop along each of the iterations
for(i in seq_along(out_list)) {
  print(i)
  out_list[[i]] <- estimate_data_revised %>%
    mutate(parameter_results = pmap(list(field_params,field_data,field_expressions),.f=~parameter_estimate(..1,..2,..3)))

}



# Now we map down vector to take a look at the results - hoisting things up as needed
estimate_combined <- out_list %>%
  bind_rows() %>%
  group_by(Year,depth,model) %>%
  mutate(iteration = 1:n()) %>%
  ungroup()

# Hoist up the parameters
field_approach_results <- estimate_combined %>%
  mutate(params = map(.x=parameter_results,.f=~enframe(.x$coefficients))) %>%
  mutate(params = map2(.x=params,.y=field_params,.f=~(filter(.y,estimate) %>%
                                                 select(name) %>%
                                                 inner_join(.x,by="name")
  ))) %>%
  hoist(parameter_results,"ssquares") %>%
  rename(rss = ssquares) %>%
  select(Year,depth,model,params,rss,iteration)


# Save results
save(field_approach_results,
     file='estimate-results/field-approach-results.Rda')

