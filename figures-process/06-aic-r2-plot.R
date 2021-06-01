# This script creates Figure 7 (AIC / R2 sparkline plot) and 2 other supplementary figures

library(gt)
library(tidyverse)

# Make a sparkline table of AIC and R2 values

load('estimate-results/stats-results/taylor-field.Rda')

# Set up a nested list of r2 and aic values
nested_value <- model_fits_all %>%
  mutate(aic = unlist(aic)) %>%
  unnest_wider(col=c("stats")) %>%
  select(depth,model,approach,adj.r.squared,aic) %>%
  mutate(model = factor(model,levels=c("null","microbe","quality","microbe-mult","quality-mult"))) %>%
  arrange(model) %>%
  rename(r2 = adj.r.squared) %>%
  group_by(depth,approach) %>%
  nest()


# Percentage of models with the lowest AIC?
nested_value %>%
  filter(approach =="incubation-field-linear") %>%
  mutate(model = map(.x=data,.f=~(.x %>% filter(is.finite(aic)) %>% slice_min(aic, n = 1,with_ties=FALSE)))) %>%
  unnest(cols=c(model)) %>%
  group_by(depth,model) %>%
  summarize(tot = n())


# The following two functions make the individual ggplots for the aic and r2 values
r2_plot <- function(in_data) {

  my_data <- in_data  %>%
    mutate(model=factor(model,levels=c("null","microbe", "quality", "microbe-mult","quality-mult"))) %>%
    select(model,r2) %>%
    na.omit()

  base_plot <- ggplot() +
    geom_blank() +
    theme_minimal() +
    theme(
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      panel.grid.major = element_line(colour = "grey50"),
      panel.grid.minor = element_blank(),
      axis.text = element_blank(),
      aspect.ratio = 9/16,
      plot.margin = margin(0, 0, 0, 0),
      plot.title = element_blank()
    ) +
    scale_y_continuous(breaks = c(0,0.25,0.5,0.75,1)) +
    coord_cartesian(ylim = c(-0.10, 1.10) )

  if(dim(my_data)[1]!=0) {

    my_data <- my_data %>%
      mutate(max_val = r2 == max(r2,na.rm=TRUE))

    p1 <- base_plot +
      geom_line(data=my_data,aes(x=model,y=r2,group=1),alpha=0.3,size=6,inherit.aes = TRUE) +
      geom_point(data=my_data,shape = 21, colour = "black", size = 18, stroke = 1,aes(x=model,y=r2,fill=max_val),inherit.aes = TRUE) +
      guides(fill=FALSE) +
      scale_fill_manual(limits = c("TRUE","FALSE"),values=c("#CC6666", "#9999CC"))


  } else { p1 <- base_plot}





  return(p1)

}


# Allow us to change the plot breaks
aic_plot <- function(in_data,break_vals=c(50,100,150),y_limits=c(20, 180)) {

  my_data <- in_data  %>%
    mutate(model=factor(model,levels=c("null","microbe", "quality", "microbe-mult","quality-mult"))) %>%
    select(model,aic) %>%
    na.omit()

  base_plot <- ggplot() +
    geom_blank() +
    theme_minimal() +
    theme(
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      panel.grid.major = element_line(colour = "grey50"),
      panel.grid.minor = element_blank(),
      axis.text = element_blank(),
      aspect.ratio = 9/16,
      plot.margin = margin(0, 0, 0, 0),
      plot.title = element_blank(),
    ) +
    scale_y_continuous(breaks = break_vals) +
    coord_cartesian(ylim = y_limits )


  if(dim(my_data)[1]!=0) {

    min_aic <- min(my_data$aic,na.rm=TRUE)
    max_aic <- max(my_data$aic,na.rm=TRUE)
    y_span <- c(max_aic-1.10*abs(max_aic-min_aic), min_aic+1.10*abs(max_aic-min_aic))

    my_data <- my_data %>%
      mutate(min_val = aic == min_aic)

    p1 <- base_plot +
      geom_line(data=my_data,aes(x=model,y=aic,group=1),alpha=0.3,size=6,inherit.aes = TRUE) +
      geom_point(data=my_data,shape = 21, colour = "black", size = 18, stroke = 1,aes(x=model,y=aic,fill=min_val),inherit.aes = TRUE) +
      guides(fill=FALSE) +
      scale_fill_manual(limits = c("TRUE","FALSE"),values=c("#CC6666", "#9999CC"))


  } else { p1 <- base_plot}


  return(p1)

}

# Test these out to make sure we have a plot
r2_plot(nested_value$data[[2]])
aic_plot(nested_value$data[[9]])



# Now loop through and make up the individual plots
aic_plots <- nested_value %>%
  unite("year_mod",depth,approach,sep="_") %>%
  select(year_mod,data) %>%
  rowwise() %>%
  mutate(
    AIC = (
      aic_plot(data)
    ) %>%
      ggplot_image(height = px(75)),
    data = NULL,

  )


r2_plots <- nested_value %>%
  unite("year_mod",depth,approach,sep="_") %>%
  select(year_mod,data) %>%
  rowwise() %>%
  mutate(
    R2 = (
      r2_plot(data)
    ) %>%
      ggplot_image(height = px(75)),
    data = NULL,
  )


# We have a table all set up now, next step is to separate by depth
aic_r2_table <- aic_plots %>%
  inner_join(r2_plots,by="year_mod") %>%
  separate(year_mod,into=c("Depth","Approach"),sep="_") %>%
  group_by(Depth,Approach) %>%
  nest() %>%
  pivot_wider(names_from = "Approach",values_from="data") %>%
  unnest_wider(col=c("field"),names_sep = ".") %>%
  unnest_wider(col=c("field-linear"),names_sep=".") %>%
  unnest_wider(col=c("incubation-field"),names_sep=".") %>%
  unnest_wider(col=c("incubation-field-linear"),names_sep=".") %>%
  relocate(c("Depth",
             "field.R2",
             "field-linear.R2",
             "incubation-field.R2",
             "incubation-field-linear.R2",
             "field.AIC",
             "field-linear.AIC",
             "incubation-field.AIC",
             "incubation-field-linear.AIC") ) %>%
  mutate(Depth = if_else(Depth=="5","5 cm",Depth),
         Depth = if_else(Depth=="10","10 cm",Depth),
         Depth = if_else(Depth=="ALL","All depths",Depth))



# Store the column names for later
column_names <- names(aic_r2_table)[-(1)]

aic_sparklines <- aic_r2_table %>%
  ungroup() %>%
  gt(rowname_col = "Depth")  %>%
  tab_spanner(
    label = html("Adjusted <em>R<sup>2</sup></em>"),
    columns = contains("R2")
  ) %>%
  tab_spanner(
    label = "AIC",
    columns = contains("AIC")
  ) %>%
  fmt_markdown(columns=vars(column_names)) %>%
  tab_options(data_row.padding = px(00)) %>%
  cols_label(
    "field.R2" = "Field",
    "field-linear.R2" = "Field Linear",
    "incubation-field.R2" = "Incubation Field",
    "incubation-field-linear.R2" = "Incubation Field Linear",
    "field.AIC" = "Field ",
    "field-linear.AIC" = "Field Linear",
    "incubation-field.AIC" = "Incubation Field",
    "incubation-field-linear.AIC" = "Incubation Field Linear"
  )



# Save the data for later use
#save(aic_r2_table,file = 'estimate-results/stats-results/sparkline-aic-data#.Rda')

gtsave(aic_sparklines,filename='manuscript-figures/06-aic-sparkline-table.png')

