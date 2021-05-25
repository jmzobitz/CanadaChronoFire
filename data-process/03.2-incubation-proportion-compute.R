# Script to compute proportion of soil carbon up to a given depth

library(tidyverse)
library(CanadaFire)

# Load up the incubation data
load('data-process/data-outputs/incubation-soil-data.Rda')


# Determine the proportion of soil carbon to a given depth.
proportion_data <- soil_carbon_data %>%
  unite("Area_Plot",Area,Plot,sep="_") %>%
  mutate(Year = factor(Year,levels=c('N2012','N1990','N1968','NC'),
                       labels=c("2012","1990","1968","Control"))) %>%
  group_by(Year,Area_Plot) %>%
  mutate(tot = n()) %>%
  filter(tot >3) %>%  # Filter where we only have measurements at all depths
  mutate(proportion = soilC / sum(soilC),
         cum_prop = cumsum(soilC)/sum(soilC)) %>% na.omit() %>% ungroup()

# Now compute a fit of the cumululative proportion
# Exponential: y = 1 - exp(-kx)  So we want to plot: ln(1-y) to kx, no intercept
#nls(mass ~ a-b*exp(c*days),

proportion_data_fit <- proportion_data %>%
  group_by(Year,Area_Plot) %>%
  nest() %>%
  mutate(lo_fit = map(.x=data,.f=~(nls(cum_prop~1-exp(-k*depth),data=.x,start = list(k=0.5)))),
         stats = map(.x=lo_fit,.f=~broom::glance(.x)),
         fit_val = map(.x=lo_fit,.f=~broom::augment(.x,newdata=data.frame(depth = seq(0, 50, 0.1))))) %>%
  #select(Year,fit_val) %>%
  unnest(cols=fit_val) %>%
  ungroup()


# Now we take the derivative (rate of change) to find the proportion between 0, 5, and 10 cm
proportion_values <- proportion_data_fit %>%
  select(Year,Area_Plot,depth,.fitted) %>%
  filter(depth %in% c(0,5,10)) %>%
  group_by(Year,Area_Plot) %>%
  summarize(carbon_amt = diff(.fitted)) %>%
  mutate(depth = c(5,10)) %>%
  ungroup()


# Report out the median soil proportion for each depth
soil_proportions <- proportion_values %>%
  group_by(Year,depth) %>%
  summarize(proportion = median(carbon_amt)) %>%
  ungroup()



save(soil_proportions,
     proportion_data,
     proportion_data_fit,
     file='data-process/data-outputs/incubation-soil-proportion-year.Rda')



