# This script creates Figure 4: Taylor diagram of different model approaches with field data


library(tidyverse)
library(CanadaFire)

load('estimate-results/stats-results/taylor-field.Rda')


taylor_values <- model_fits %>%
  select(Year,depth,model,approach,taylor_values) %>%
  unnest(cols = c(taylor_values)) %>%
  mutate(Year = factor(Year,levels=c('N2012','N1990','N1968','NC',"NALL"),
                       labels=c('2012','1990','1968','Control','All sites')),
         depth = factor(depth,levels=c("5","10","ALL"),
                        labels=c("5 cm","10 cm","All depths")),
         model = factor(model,levels=c("null","microbe","quality","microbe-mult","quality-mult"),
                        labels=c("Null","Microbe","Quality","Microbe-mult","Quality-mult")),
         approach = factor(approach,levels=c("field","field-linear","incubation-field","incubation-field-linear"),
                           labels=c("Field","Field Linear","Incubation \n Field","Incubation \n Field Linear")))


t_plot <- taylor_plot(full=TRUE,max_R = 2.5)

curr_plot <- t_plot +
  geom_point(data=taylor_values,aes(x=x_coord,y=y_coord,color=model,shape=approach),size=4) +
  facet_grid(Year~depth) +
  labs(y=expression(italic("\u03C3")[model] / italic("\u03C3")[meas]),color="Model:",shape="Parameter estimation approach:") +
  theme_fulbright() +
  theme(legend.position="bottom", legend.box="vertical", legend.margin=margin()) +
  scale_shape_manual(values=15:18)

#scale_shape_discrete(guide=FALSE)



ggsave(curr_plot,filename = 'manuscript-figures/04-field-taylor.png',width = 22,height=20,units="cm")




