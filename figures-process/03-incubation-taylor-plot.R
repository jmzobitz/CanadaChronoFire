# This script creates Figure 3: Taylor diagram of incubation only results

library(tidyverse)
library(CanadaFire)
load('estimate-results/stats-results/taylor-incubation.Rda')

taylor_values <- model_fits %>%
  select(Year,depth,model,taylor_values) %>%
  unnest(cols = c(taylor_values)) %>%
  mutate(Year = factor(Year,levels=c('N2012','N1990','N1968','NC',"NALL"),
                       labels=c('2012','1990','1968','Control','All sites')),
         depth = factor(depth,levels=c("5","10","ALL"),
                        labels=c("5 cm","10 cm","All depths")),
         model = factor(model,levels=c("null","microbe","quality","microbe-mult","quality-mult"),
                        labels=c("Null","Microbe","Quality","Microbe-mult","Quality-mult")))

t_plot <- taylor_plot(full=FALSE,max_R = 2.5)

curr_plot <- t_plot +
  geom_point(data=taylor_values,aes(x=x_coord,y=y_coord,color=model),size=4) +
  facet_grid(Year~depth) +
  labs(y=expression(italic("\u03C3")[model] / italic("\u03C3")[meas]),color="Model:",x="") +
  theme_fulbright() +
  theme(axis.text = element_text(size=12) ) +
  scale_x_continuous(breaks=0:2) +
  scale_y_continuous(breaks=0:2) +
  guides(color=guide_legend(nrow=2,byrow=TRUE))


ggsave(curr_plot,filename = 'manuscript-figures/03-incubation-taylor.png',width = 15,height=22,units="cm")


