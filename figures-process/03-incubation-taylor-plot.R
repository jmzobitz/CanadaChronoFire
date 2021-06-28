# This script creates Figure 3: Taylor diagram of incubation only results

library(tidyverse)
library(FireResp)
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
  geom_point(data=taylor_values,aes(x=x_coord,y=y_coord,color=model,shape=model,fill=model),size=4,alpha=0.6) +
  facet_grid(.~depth) +
  labs(y=expression(italic("\u03C3")[model] / italic("\u03C3")[meas]),color="Submodel:",shape="Submodel:",x="",fill="Submodel:") +
  theme_canada() +
  scale_shape_manual(values=c(21:25)) +
  theme(axis.text = element_text(size=12) )


ggsave(curr_plot,filename = 'manuscript-figures/03-incubation-taylor.png',width = 22,height=10,units="cm")


