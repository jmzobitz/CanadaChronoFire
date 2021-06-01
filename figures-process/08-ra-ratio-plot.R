# Make a plot of the ratio of autotrophic respiration for the different models.  This forms figure 8 in our manuscript
library(CanadaChronoFire)
library(tidyverse)
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
  theme_canada() +
  theme(panel.grid.major.y =  element_line(colour = "grey50",linetype = 'dashed')) +
  labs(y=expression(R[A] / (R[A] + R[H]) ),shape = "Soil depth:",color="Model:") +
  theme(axis.text.x=element_text(angle = 45, vjust = 0.5)) + ylim(c(0,1))



### Add data from riberio et al 2020 in the RA ratio plot.
### See https://data.mendeley.com/datasets/v7gxtvv9z3/1

riberio_data <- readxl::read_excel('data-raw/riberio-kumera.xlsx') %>%
  rename(age = 1) %>% na.omit()

# Determine a smoothed loess fit
riberio_fit <- loess(rA_prop~age,riberio_data)

# Extract out the modeled chronosequence ages for our study
riberio_predict_values <- broom::augment(riberio_fit, newdata=data.frame(Year=c("2012","1990","1968","Control"), age = c(3,25,47,NA)),se=TRUE) %>%
  mutate(lower_val = .fitted-2*.se.fit,
         upper_val = .fitted+2*.se.fit)



### Add the riberio data to our plot
updated_results <- results_pa +
  geom_crossbar(data=riberio_predict_values,aes(x=Year,y=.fitted,ymin=lower_val,ymax=upper_val),width=0.2,inherit.aes = TRUE,fill='green',color='green',alpha=0.1)


ggsave(filename = 'manuscript-figures/08-ratio-results-model.png',plot = updated_results,width = 12,height=4)
