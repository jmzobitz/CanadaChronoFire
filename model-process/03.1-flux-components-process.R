# Load and process the parameters

library(FireResp)
library(tidyverse)

### Approach 1: Field
load('estimate-results/field-approach-results.Rda')

my_results <- rss_filter(field_approach_results) %>%
  select(Year,depth,model,rss_filter) %>%
  rename(data = rss_filter )

my_expressions <- respiration_expressions %>%
  select(model,RA_expression,RH_expression)

median_field <- summarize_flux_component_approach(my_results,my_expressions,'field')

### Approach 2: Field Linear
# Here rA = gR*cR, rH is the same as the field

load('estimate-results/field-linear-approach-results.Rda')

my_results <- rss_filter(field_linear_approach_results) %>%
  select(Year,depth,model,rss_filter) %>%
  rename(data = rss_filter )

my_expressions <- respiration_expressions %>%
  select(model,RA_linear_expression,RH_expression) %>%
  rename(RA_expression = RA_linear_expression)


# Then compute the median across the fluxes
median_field_linear <- summarize_flux_component_approach(my_results,my_expressions,'field-linear')


### Approach 3: Incubation Field
# We don't need to do any filtering on the approach - just nest results by the model

load('estimate-results/incubation-field-approach-results.Rda')

my_results <- incubation_field_approach_results %>%
  group_by(Year,depth,model) %>%
  nest()

my_expressions <- respiration_expressions %>%
  select(model,RA_expression,RH_expression)

median_incubation_field <- summarize_flux_component_approach(my_results,my_expressions,'incubation-field')

### Approach 4: Incubation Field Linear
# We don't need to do any filtering on the approach - just nest results by the model
# Here RA = gR*CR, RH = f*RH

load('estimate-results/incubation-field-linear-approach-results.Rda')

my_results <- incubation_field_linear_approach_results %>%
  group_by(Year,depth,model) %>%
  nest()

my_expressions <- respiration_expressions %>%
  select(model,RA_linear_expression,RH_incubation_field_linear_expression) %>%
  rename(RA_expression = RA_linear_expression,
         RH_expression = RH_incubation_field_linear_expression)

median_incubation_field_linear <- summarize_flux_component_approach(my_results,my_expressions,'incubation-field-linear')

# Now combine these all up to save!

median_flux_components = rbind(
  median_field,
  median_field_linear,
  median_incubation_field,
  median_incubation_field_linear
)


# OK, let's save these results - we will use them later!
save(median_flux_components,file='estimate-results/stats-results/median-flux-components.Rda')
