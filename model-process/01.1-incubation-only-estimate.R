# This computes the model estimates for the incubation data only.  We've done a lot of the analysis and processing of data, this just tidies everything together.

# Prepare the model expressions

library(nlsr)
library(tidyverse)
library(CanadaFire)  # Load up the package



# Determine the number of iterations
n_iter <- 1000

# Create an outlist of vectors
out_list <- vector("list",length=n_iter)


# Just loop along each of the iterations
for(i in seq_along(out_list)) {
  print(i)
  out_list[[i]] <- estimate_data %>%
    mutate(parameter_results = pmap(list(incubation_params,incubation_data,incubation_expressions),.f=~parameter_estimate(..1,..2,..3)))

}



# Now we map down vector to take a look at the results - hoisting things up as needed

estimate_combined <- out_list %>% bind_rows() %>%
  group_by(Year,depth,model) %>% mutate(iteration = 1:n()) %>%
  ungroup()

# Hoist up the parameters
incubation_results <- estimate_combined %>%
  mutate(params = map(.x=parameter_results,.f=~enframe(.x$coefficients))) %>%
  mutate(params = map2(.x=params,.y=incubation_params,.f=~(filter(.y,estimate) %>%
                                                      select(name) %>%
                                                      inner_join(.x,by="name")
  ))) %>%
  hoist(parameter_results,"ssquares") %>%
  rename(rss = ssquares) %>%
  select(Year,depth,model,params,rss,iteration)


# Save the results to a file for subsequent processing
save(incubation_results,
     file='estimate-results/incubation-only-results.Rda')
