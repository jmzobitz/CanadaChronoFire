# This computes the model estimates for the uncubation field linear approach - in this case we use the results from the incubatino only data

# Load up the estimated results parameters
load('estimate-results/incubation-only-results.Rda')

# Now we need to determine the summary distribution, first filtering out by the RSS any duplicates and between the 25th - 75 percentile

rss_unique <- rss_filter(incubation_results) %>%
  select(Year,depth,model,rss_filter) %>%
  unnest(cols=rss_filter)


# This is a function that computes rh from the incubation parameters.  It creates a new data frame for rH, CR, rSoil - all parameters that go into the field data.

compute_model_rh <- function(input_data,input_params,input_model_exp) {

  my_params <- input_params %>% select(name,value) %>% deframe()

  with(as.list(c(my_params)), {

    rSoil <- input_data$respiration
    rSoil_model <- eval(formula.tools::rhs(input_model_exp),envir=input_data) %>% as.vector()

    return(tibble(respiration = rSoil, rH = rSoil_model,CR=input_data$CR))

  })


}

# Join the parameters with the joint dataset, compute RH from the estimated parameters
estimate_data_rh_est <- rss_unique %>%
  left_join(estimate_data,by=c("Year","depth","model")) %>%
  mutate(rh_compute_data = pmap(list(field_data,params,incubation_expressions),.f=~compute_model_rh(..1,..2,..3)))


# Let's narrow down this dataset so we can declutter it:
estimate_data_input <- estimate_data_rh_est %>%
  select(Year,depth,model,iteration,params,rh_compute_data,field_params,model_estimate) %>%
  mutate(field_params = map(.x=field_params,
                              .f=~(.x %>% mutate(estimate = if_else(name %in% c("f","gR"),TRUE,FALSE) ) %>%
                                     filter(name %in% c("f","gR"))) ) )  # For the field data we are only estimate f and gM

####

# OK, we just need to add on the new expression we use to estimate things an away we go.
new_expression <- tibble(expr=c(respiration ~ (f*rH +gR*CR)))
estimate_data_rev <- cbind(estimate_data_input,new_expression) %>%
  mutate(curr_iter = 1:n())

# Map through the results



out_list <- vector("list",length=dim(estimate_data_rev)[1])


for(i in seq_along(out_list)) {
  print(i)

  curr_results <- parameter_estimate(estimate_data_rev$field_params[[i]],estimate_data_rev$rh_compute_data[[i]],estimate_data_rev$expr[[i]])

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
        params_ifl = "params",
        rss = "rss")

# Now we just want to join these up together.

incubation_field_linear_approach_results <- estimate_data_rev %>%
  inner_join(estimate_results,by="curr_iter")  %>%
  mutate(combined_params = map2(.x=params,.y=params_ifl,.f=~rbind(.x,.y))) %>%
  select(Year,depth,model,combined_params,rss,iteration) %>%
  rename(params=combined_params)

# Save results
save(incubation_field_linear_approach_results,
     file='estimate-results/incubation-field-linear-approach-results.Rda')


