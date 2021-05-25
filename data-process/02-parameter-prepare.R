# Load in the parameters for the models that we will process

library(tidyverse)
### Add in the parameters:

param_values <- readxl::read_xlsx('data-raw/incubation-model-param.xlsx',sheet = 'ranges') %>%
  select(-description,-units)

incubation_params <- readxl::read_xlsx('data-raw/incubation-model-param.xlsx',sheet = 'incubation') %>%
  select(-description,-units) %>%
  pivot_longer(cols = c("null":"quality-mult"),names_to = "model", values_to = "estimate") %>%
  mutate(type='incubation') %>%
  inner_join(param_values,by=c("name")) %>%
  group_by(model) %>%
  nest() %>%
  rename(incubation = data)

field_params <- readxl::read_xlsx('data-raw/incubation-model-param.xlsx',sheet = 'field') %>%
  select(-description,-units) %>%
  pivot_longer(cols = c("null":"quality-mult"),names_to = "model", values_to = "estimate") %>%
  mutate(type='field') %>%
  inner_join(param_values,by=c("name")) %>%
  group_by(model) %>%
  nest() %>%
  rename(field = data)

# A quick function to determine if we estimate a particular parameter
in_model <- function(i1,f1) {

  out_val <- tibble(name = i1$name, estimate = if_else(i1$estimate | f1$estimate,TRUE,FALSE) )
  return(out_val)
}


# Combined the parameters together so we know which model contains which particular parameters.
combined_params <- incubation_params %>%
  inner_join(field_params,by="model") %>%
  mutate(model_estimate= map2(.x=incubation,.y=field,.f=~in_model(.x,.y) ) )

# The vector combined_params is a nested list - first column is the model,
# second column is the incubation model parameters, third column field model parameters, and fourth column a boolean vector if the parameter is in the model (for any part)

# Export as a dataset to be used by the package
use_data(combined_params,overwrite=TRUE)

