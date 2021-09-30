# This script creates Figure 4: Taylor diagram of different model approaches with field data


library(tidyverse)
library(FireResp)

load('estimate-results/stats-results/taylor-field.Rda')


taylor_values <- model_fits_all %>%
  select(depth,model,approach,taylor_values) %>%
  unnest(cols = c(taylor_values)) %>%
  mutate(depth = factor(depth,levels=c("5","10","ALL"),
                        labels=c("5 cm","10 cm","All depths")),
         model = factor(model,levels=c("null","microbe","quality","microbe-mult","quality-mult"),
                        labels=c("Null","Microbe","Quality","Microbe-mult","Quality-mult")),
         approach = factor(approach,levels=c("field","field-linear","incubation-field","incubation-field-linear"),
                           labels=c("Field","Field Linear","Incubation \n Field","Incubation \n Field Linear")))



t_plot <- taylor_plot(max_R = 2)

curr_plot <- t_plot +
  geom_point(data=taylor_values,aes(x=x_coord,y=y_coord,color=model,shape=approach),size=4) +
  facet_grid(.~depth) +
  labs(y=expression(italic("\u03C3")[model] / italic("\u03C3")[meas]),color="Submodel:",shape="Parameter estimation approach:") +
  theme_canada() +
  theme(legend.position="bottom", legend.box="vertical", legend.margin=margin(),
        axis.text = element_text(size=18)) +
  scale_shape_manual(values=15:18)





ggsave(curr_plot,filename = 'manuscript-figures/04-field-taylor.png',width = 23,height=12,units="cm")

