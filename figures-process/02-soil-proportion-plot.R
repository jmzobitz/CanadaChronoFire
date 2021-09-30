# This code produces Figure 2: cumulative proportion plot

library(tidyverse)
library(FireResp)
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

### Determine how much is in each median for each year at 30 cm
ensemble_proportion %>%
  filter(depth==30)

proportion_data_new <- proportion_data %>%
  separate(Area_Plot,into=c("Area","Plot"),sep='_',remove=FALSE)

# Construct the figure for the results
p1 <- proportion_data_fit %>%
  ggplot(aes(x=depth)) +
  geom_point(data=proportion_data_new,aes(x=depth,y=cum_prop,group=Area_Plot,color=Plot,shape=Area),inherit.aes = FALSE,size=3) +
  geom_ribbon(data=ensemble_proportion,aes(x=depth,ymin=q0.025,ymax=q0.975),inherit.aes = FALSE,alpha=0.3,fill='red') +
  geom_line(data=ensemble_proportion,aes(x=depth,y=q0.5),inherit.aes = FALSE,color='red',size=1) +
  facet_grid(.~Year) +
  scale_x_reverse() +
  coord_flip() + theme_canada() +
  theme(axis.text.x=element_text(angle =- 45, vjust = 0.5),
        strip.text.x = element_text(size=18)) +
  labs(y="Cumulative soil carbon proportion",
       x="Depth (cm)") +
  guides(color="none",shape="none")

ggsave(filename='manuscript-figures/02-soil-proportion-fit.png',plot=p1,width=13,height=5)
