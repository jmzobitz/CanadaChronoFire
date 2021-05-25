# Compute the linear regression between CA and CS from the incubation data so we can apply this to the field data to have an estimate of CA that is approximately consistent with measurements in the incubation data.

library(tidyverse)
library(broom)


# For field data, assume there is some linear relationship between CA and CS in the incubation data that extends to the field data?

# Load in the incubation data
load('data-process/incubation-soil-data.Rda')

# Combine the area an plot together so we can do a grouping - group this by depth and site to predict the value of CA from the measured soil carbon (seems pretty robust)
joined_incubation %>%
  ggplot(aes(x=CS,y=CA)) + geom_point() +
  geom_smooth(method="lm") +
  facet_grid(depth~Year,scales="free")  # Maybe scale this by the proportion in each layer for some relationship?

# Now compute the linear regression between CA and CS (field data and save the states to a fiel)
CA_stats <- joined_incubation %>%
  filter(depth %in% c(5,10)) %>%
  group_by(Year,depth) %>%
  nest() %>%
  mutate(lm_fit = map(.x=data,.f=~lm(CA~CS,data=.x))) %>%
  mutate(tidied = map(lm_fit, tidy))

save(CA_stats,file='data-process/data-outputs/incubation-CA-stats.Rda')
