# This computes the model estimates for the uncubation field linear approach - in this case we use the results from the incubation only data

# Load up the processed filtered parameters - see
# 01.2-incubation-linear-process.R

# Prepare the model expressions
library(nlsr)
library(tidyverse)
library(FireResp)  # Load up the package

load('estimate-results/incubation-linear-approach-data.Rda')

####

# OK, we just need to add on the new expression we use to estimate things an away we go.

out_list <- vector("list",length=dim(estimate_data_linear)[1])



for(i in seq_along(out_list)) {

  print(i)

  curr_results <- parameter_estimate(estimate_data_linear$field_params[[i]],estimate_data_linear$field_data[[i]],estimate_data_linear$incubation_field_expressions[[i]])

  coefficients <- curr_results$coefficients %>% enframe() %>%
    inner_join(estimate_data_linear$model_estimate[[i]],by="name") %>%
    filter(estimate) %>%
    select(-estimate)

  rss <- curr_results$ssquares
  out_list[[i]] <- list(currIter = i,
                        params=coefficients,
                        rss = rss)



}

# We need the cofficients and the sum of squares extracted from the list -
# The vignette("rectangle") is helpful to sort that out
estimate_results <- tibble(results = out_list) %>%
  hoist(results,
        curr_iter = "currIter",
        params_new = "params",
        rss_new = "rss")


# Now we just want to join these up together.  # FILTER OUT IF IT IS IN THE MODEL

incubation_field_approach_results <- estimate_data_linear %>%
  inner_join(estimate_results,by="curr_iter")  %>%
  select(Year,depth,model,params_new,rss_new,iteration,model_estimate) %>%
  rename(params=params_new,rss=rss_new) %>%
  select(-model_estimate)




# Save results
save(incubation_field_approach_results,
     file='estimate-results/incubation-field-approach-results.Rda')


