# This final code creates a master list of joined incubation and field data together that we can use for parameter estimations.

# Assumes all antecedent scripts in the data-process folder have been run

library(tidyverse)
library(CanadaFire)

# Join the expressions to the parameters
combined_params_expr <- combined_params %>%
  inner_join(model_expressions,by=c("model"))


# Create a master nested list of incbuation and field data - this will be the primary input for everything.  We join this with the combined parameters expression
estimate_data <- combined_data %>%
  rename(incubation_data = incubation,
         field_data = field) %>%
  nest()  %>%
  mutate(data=map(.x=data,.f=~cbind(.x,combined_params_expr) ) ) %>%
  unnest(cols=c(data)) %>%
  rename(incubation_params = incubation,
         field_params = field)


# Save and use this data for the package
use_data(estimate_data,overwrite=TRUE)
