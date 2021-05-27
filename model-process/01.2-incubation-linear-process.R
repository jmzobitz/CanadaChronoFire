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



# Copy over the parameters from the incubation to the field data
estimate_data_linear <- rss_unique %>%
  mutate(field_params = map2(.x=params,.y=field_params,.f=~parameter_unite(.x,.y)) ) %>%
  select(Year,depth,model,iteration,params,field_data,incubation_params,field_params,model_estimate,field_expressions,incubation_field_linear_expressions) %>%
  ungroup() %>%
  mutate(curr_iter = 1:n())

# Save this result - we will use it for computing taylor values
save(estimate_data_linear,
     file='estimate-results/incubation-linear-approach-data.Rda')
