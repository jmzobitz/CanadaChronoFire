# This code produces Figure 2: cumulative proportion plot

library(tidyverse)
load('data-process/data-outputs/incubation-soil-proportion-year.Rda')

# Compute the ensemble values from the fitted results
ensemble_proportion <- proportion_data_fit %>%
  select(Year,depth,.fitted) %>%
  group_by(Year,depth) %>%
  nest() %>%
  mutate(conf_int =  map(.x=data,.f=~(tibble(ci_value=quantile(.x$.fitted, c(0.025, 0.5, 0.975)), quantile = c("q0.025", "q0.5", "q0.975")))  ) ) %>%
  select(-data) %>%
  unnest(cols=c(conf_int)) %>%
  pivot_wider(names_from = "quantile",values_from="ci_value") %>%
  ungroup()

# Construct the figure for the results
p1 <- proportion_data_fit %>%
  ggplot(aes(x=depth)) +
  geom_point(data=proportion_data,aes(x=depth,y=cum_prop,group=Area_Plot,color=Area_Plot),inherit.aes = FALSE) +
  geom_ribbon(data=ensemble_proportion,aes(x=depth,ymin=q0.025,ymax=q0.975),inherit.aes = FALSE,alpha=0.3,fill='blue') +
  geom_line(data=ensemble_proportion,aes(x=depth,y=q0.5),inherit.aes = FALSE,color='blue',size=1) +
  facet_grid(.~Year) +
  scale_x_reverse() +
  coord_flip() + theme_fulbright() +
  theme(axis.text.x=element_text(angle =- 45, vjust = 0.5)) +
  labs(y="Cumulative soil carbon proportion",
       x="Depth (cm)") +
  guides(color=FALSE)

ggsave(filename='manuscript-figures/02-soil-proportion-fit.png',plot=p1,width=9)
