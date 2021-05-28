# Make a plot of the ratio of autotrophic respiration for the different models.  This forms figure 8 in our manuscript

# Load in the data
load('estimate-results/stats-results/median-flux-components.Rda')

my_labeller2 <- as_labeller(c("incubation-field"="Incubation~Field","field-linear"="Field~Linear","incubation-field-linear"="Incubation~Field~Linear","field"="Field"),
                            default = label_parsed)

results_pa <- median_flux_components %>%
  filter(Year!='NALL') %>%
  unite(col="depth_mod",sep="_",depth,model,remove=FALSE) %>%
  mutate(Year = factor(Year,levels=c('N2012','N1990','N1968','NC'),
                       labels=c("2012","1990","1968","Control")),
         model = factor(model,levels=c("null","microbe","quality","microbe-mult","quality-mult"),labels=c("Null","Microbe","Quality","Microbe-mult","Quality-mult")),
         depth = factor(depth,levels=c("5","10","ALL"),
                        labels = c("5 cm","10 cm","All depths")) ) %>%

  ggplot() +
  geom_line(color='grey',aes(x=Year,y=rA_prop,group=depth_mod)) +
  geom_point(aes(color=model,shape=depth,x=Year,y=rA_prop,group=depth_mod),size=2) + facet_grid(.~approach,labeller = my_labeller2) +
  theme_fulbright() +
  theme(panel.grid.major.y =  element_line(colour = "grey50",linetype = 'dashed')) +
  labs(y=expression(R[A] / (R[A] + R[H]) ),shape = "Soil depth:",color="Model:") +
  theme(axis.text.x=element_text(angle = 45, vjust = 0.5)) + ylim(c(0,1))


ggsave(filename = 'manuscript-figures/08-ratio-results-model.png',plot = results_pa,width = 12,height=4)
