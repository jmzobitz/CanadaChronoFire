# This script creates Figure 3: Taylor diagram of incubation only results

library(tidyverse)
library(CanadaChronoFire)
load('estimate-results/stats-results/taylor-incubation.Rda')

taylor_values <- model_fits_all %>%
  ungroup() %>%
  select(depth,model,taylor_values) %>%
  unnest(cols = c(taylor_values)) %>%
  mutate(depth = factor(depth,levels=c("5","10","ALL"),
                        labels=c("5 cm","10 cm","All depths")),
         model = factor(model,levels=c("null","microbe","quality","microbe-mult","quality-mult"),
                        labels=c("Null","Microbe","Quality","Microbe-mult","Quality-mult")))

t_plot <- taylor_plot(full=FALSE,max_R = 2)

curr_plot <- t_plot +
  geom_point(data=taylor_values,aes(x=x_coord,y=y_coord,color=model),size=4) +
  facet_grid(.~depth) +
  labs(y=expression(italic("\u03C3")[model] / italic("\u03C3")[meas]),color="Model:",x="") +
  theme_canada() +
  theme(axis.text = element_text(size=12) )


ggsave(curr_plot,filename = 'manuscript-figures/03-incubation-taylor.png',width = 22,height=10,units="cm")


