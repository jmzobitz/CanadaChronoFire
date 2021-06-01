# This computes the model estimates for the field linear approach - in this case parameters are only estimated using the field data


# Prepare the model expressions
library(nlsr)
library(tidyverse)
library(CanadaChronoFire)  # Load up the package

####

# change the some names on the parameters (don't estimate RA params) and do some cleanup
estimate_data_rev <- estimate_data %>%
  mutate(model_estimate = map(.x=model_estimate,
                              .f=~(.x %>% mutate(estimate = if_else(name %in% c("kR","Q10R"),FALSE,estimate),
                                                 estimate = if_else(name =="gR",TRUE,estimate)                                                       ))),  # Change if we estimate them or not
         field_params = map2(.x=field_params,.y=model_estimate,.f=~(mutate(.x,estimate=.y$estimate))) # Rename the field params so we can estimate them
  )


####

# Number of iterations
n_iter <- 1000

out_list <- vector("list",length=n_iter)

# List we have for data going out
# Just loop along each of the iterations
for(i in seq_along(out_list)) {

  print(i)
  out_list[[i]] <- estimate_data_rev %>%
    mutate(parameter_results = pmap(list(field_params,field_data,field_linear_expressions),.f=~parameter_estimate(..1,..2,..3)))

}

# Now we map down vector to take a look at the results - hoisting things up as needed

estimate_combined <- out_list %>%
  bind_rows() %>%
  group_by(Year,depth,model) %>%
  mutate(iteration = 1:n()) %>%
  ungroup()

# Hoist up the parameters
field_linear_approach_results <- estimate_combined %>%
  mutate(params = map(.x=parameter_results,.f=~enframe(.x$coefficients))) %>%
  mutate(params = map2(.x=params,.y=field_params,.f=~(filter(.y,estimate) %>%
                                                        select(name) %>%
                                                        inner_join(.x,by="name")
  ))) %>%
  hoist(parameter_results,"ssquares") %>%
  rename(rss = ssquares) %>%
  select(Year,depth,model,params,rss,iteration)

save(field_linear_approach_results,
     file='estimate-results/field-linear-approach-results.Rda')
