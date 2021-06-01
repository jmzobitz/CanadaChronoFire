# Make a plot of the different flux respiration components for the different models.  This forms figure 7 in our manuscript

library(CanadaChronoFire)
# Load in the data
load('estimate-results/stats-results/median-flux-components.Rda')


field_data <- combined_data %>%
  select(-incubation) %>%
  filter(Year %in% c("N2012","N1990","N1968","NC")) %>%
  unnest(cols=c(field)) %>%
  group_by(Year) %>%
  summarize(maxR = max(respiration),
            minR = min(respiration),
            medianR = median(respiration),
            q0.25 = quantile(respiration,prob=0.25),
            q0.75 = quantile(respiration,prob=0.75)) %>%
  mutate(Year = factor(Year,levels=c('N2012','N1990','N1968','NC'),
                       labels=c("2012","1990","1968","Control") ) ) %>%
  arrange(Year)

my_labeller <- as_labeller(c(rA="R[A]", rH="R[H]","incubation-field"="Incubation~Field","field-linear"="Field~Linear","incubation-field-linear"="Incubation~Field~Linear","field"="Field"),
                           default = label_parsed)



results_flux <- median_flux_components %>%
  filter(Year != 'NALL') %>%
  select(-rA_prop) %>%
  pivot_longer(names_to="flux",values_to="value",cols=c("rA","rH")) %>%
  unite(col="depth_mod",sep="_",depth,model,remove = FALSE) %>%
  mutate(Year = factor(Year,levels=c('N2012','N1990','N1968','NC'),
                       labels=c("2012","1990","1968","Control") ),
         model = factor(model,levels=c("null","microbe","quality","microbe-mult","quality-mult"),labels=c("Null","Microbe","Quality","Microbe-mult","Quality-mult")),
         depth = factor(depth,levels=c("5","10","ALL"),
                        labels = c("5 cm","10 cm","All depths")),
         flux = factor(flux,levels=c("rA","rH")) ) %>%
  arrange(Year) %>%
  ggplot() +
  geom_boxplot(data=filter(field_data,Year!='NALL'),aes(x=Year,lower=q0.25,upper=q0.75,middle=medianR,ymin=minR,ymax=maxR),alpha=0.2,stat="identity",fill='red',color='red') +
  geom_line(color='grey',aes(x=Year,value,group=depth_mod)) +
  geom_point(aes(color=model,shape=depth,x=Year,y=value,group=depth_mod),size=2) + facet_grid(flux~approach,labeller = my_labeller) +
  theme_canada() +
  labs(y=bquote(~Flux~'('~g~C~m^-2~d^-1*~')'),shape = "Soil depth:",color="Model:") +
  theme(axis.text.x=element_text(angle = 45, vjust = 0.5)) +
  coord_cartesian(ylim=c(0, 5))



ggsave(filename = 'manuscript-figures/07-flux-results-model.png',plot = results_flux,width = 11,height=5)
