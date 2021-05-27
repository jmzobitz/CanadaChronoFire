# This computes the model estimates for the uncubation field linear approach - in this case we use the results from the incubatino only data


load('estimate-results/incubation-linear-approach-data.Rda')

####

# First we need to change WHAT gets estimated in this approach - the incubation field linear only estimates f and gR - so we need to adjust BOTH field_estimate and model_estimate to collapse those parameters later.
estimate_data_rev <- estimate_data_linear  %>%
  mutate(field_params = map(.x=field_params,
                            .f=~(.x %>% mutate(estimate = if_else(name %in% c("f","gR"),TRUE,FALSE) )  )  ),
         model_estimate = map(.x=model_estimate,.f=~(.x %>% mutate(estimate = if_else(name %in% c("f","gR"),TRUE,estimate),
                                                                   estimate = if_else(name %in% c("Q10R","kR"),FALSE,estimate))  ) ) )

# OK, we just need to add on the new expression we use to estimate things an away we go.

out_list <- vector("list",length=dim(estimate_data_rev)[1])


for(i in seq_along(out_list)) {
  print(i)

  curr_results <- parameter_estimate(estimate_data_rev$field_params[[i]],estimate_data_rev$field_data[[i]],estimate_data_rev$incubation_field_linear_expressions[[i]])

  coefficients <- curr_results$coefficients %>% enframe()
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
        params_new= "params",
        rss = "rss")


# Now we just want to join these up together.  # FILTER OUT IF IT IS IN THE MODEL

incubation_field_linear_approach_results <- estimate_data_rev %>%
  inner_join(estimate_results,by="curr_iter")  %>%
  select(Year,depth,model,params_new,rss,iteration,model_estimate) %>%
  rename(params=params_new) %>%
  mutate(params = map2(.x=params,.y=model_estimate,.f=~(.x %>% inner_join(.y,by="name") %>% filter(estimate) %>% select(-estimate)))) %>%
  select(-model_estimate)




# Save results
save(incubation_field_linear_approach_results,
     file='estimate-results/incubation-field-linear-approach-results.Rda')

