# This code loads up the incubation data, does some filtering and preps it for the incubation-field and incubation-field-linear approaches


# Load up the estimated results parameters
load('estimate-results/incubation-only-results.Rda')

# Now we need to determine the summary distribution, first filtering out by the RSS any duplicates and between the 25th - 75 percentile

rss_unique <- rss_filter(incubation_results) %>%
  select(Year,depth,model,rss_filter) %>%
  unnest(cols=rss_filter) %>%
  inner_join(estimate_data,by=c("Year","depth","model"))

# Function that takes in a parameter set (with values) and copies them over to another one

parameter_unite <- function(new_params,old_params) {

  new_values <- old_params %>% left_join(new_params,by="name") %>%
    mutate(value.x=if_else(is.na(value.y),value.x,value.y)) %>%
    select(-value.y) %>%
    rename(value=value.x)

  return(new_values)
}

# Compute the heterotrophic respiration values for the field data, adding rH as a column to the field data

# This function does that for us
compute_model_rh <- function(input_data,input_params,input_model_exp) {

  my_params <- input_params %>% select(name,value) %>% deframe()

  with(as.list(c(my_params)), {

    respiration <- input_data$respiration
    rSoil_model <- eval(formula.tools::rhs(input_model_exp),envir=input_data) %>% as.vector()

    return(mutate(input_data,rH=rSoil_model))

  })


}

# Now we map!  Add rH as a column to the data, and then also copy over incubation parameters to the field data
estimate_data_linear <- rss_unique %>%
  mutate(field_data = pmap(list(field_data,incubation_params,incubation_expressions),.f=~compute_model_rh(..1,..2,..3))) %>%
  mutate(field_params = map2(.x=params,.y=field_params,.f=~parameter_unite(.x,.y)) ) %>%
  ungroup() %>%
  mutate(curr_iter = 1:n())



# Save this result - we will use it for computing taylor values
save(estimate_data_linear,
     file='estimate-results/incubation-linear-approach-data.Rda')
