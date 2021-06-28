# This script makes the parameter sparkline plots (figures 5 + supplementary in our paper.)

library(gt)
library(tidyverse)
library(FireResp)

# Load up median values of all the parameters and approaches
load('estimate-results/stats-results/median-params-field.Rda')




# Each row is a parameter value, year varies



parameter_ranges <- combined_params %>%
  select(model,field) %>%
  mutate(field = map(.x=field,.f=~select(.x,name,min_value,max_value)) )

# Now make a sparkline plot of parameters over time - filter NALL
my_data <- median_params_field %>%
  filter(Year != "NALL") %>%
  mutate(Year = factor(Year,levels=c('N2012','N1990','N1968','NC'))) %>%
  unnest(cols=c(data)) %>%
  rename(value = ci_value) %>%
  group_by(model,name,depth,approach) %>%
  arrange(Year) %>%
  nest()  %>%
  inner_join(parameter_ranges,by="model") %>%
  mutate(field = map2(.x=name,.y=field,.f=~filter(.y,name==.x) %>% select(-name))) %>%
  mutate(data = map2(.x=data,.y=field,.f=~(.x %>% mutate(not_edge_hitting = between(.x$value,1.01*.y$min_value,.99*.y$max_value)))))

# Percentage of edge hitting parameters?
my_data %>% unnest(cols=c(data)) %>%
  group_by(model) %>%
  summarize(pct = 1-sum(not_edge_hitting) / n()) %>%
  arrange(desc(pct))

# Just filtering on incubation-field-linear
my_data %>%
  filter(approach == "incubation-field-linear") %>%
  unnest(cols=c(data)) %>%
  group_by(model) %>%
  summarize(pct = 1-sum(not_edge_hitting) / n()) %>%
  arrange(desc(pct))


# This makes the ggplots for the data

parameter_plot <- function(in_data,param_range) {

  my_data <- in_data  %>%
    na.omit()

  base_plot <- ggplot() +
    geom_blank() +
    theme_minimal() +
    theme(
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      panel.grid.major.x = element_line(colour = "grey50"),
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      axis.text = element_blank(),
      aspect.ratio = 9/16,
      plot.margin = margin(0, 0, 0, 0),
      plot.title = element_blank()
    )


  if(dim(my_data)[1]!=0) {

    max_val <- param_range$max_value
    min_val <- param_range$min_value

    range <- max_val-min_val
    y_span <- c(max_val-1.10*range, min_val+1.10*range)



    p1 <- base_plot +
      geom_line(data=my_data,aes(x=Year,y=value,group=1),alpha=0.3,size=6,inherit.aes = TRUE) +
      geom_point(data=my_data,shape = 21, colour = "black", size = 18, stroke = 1,aes(x=Year,y=value,fill=not_edge_hitting),inherit.aes = TRUE) +
      guides(fill=FALSE) +
      coord_cartesian(ylim = y_span ) +
      scale_fill_manual(limits = c("TRUE","FALSE"),values=c("#CC6666", "#9999CC"))


  } else { p1 <- base_plot}


  return(p1)

}

parameter_plot(my_data$data[[1]],my_data$field[[1]])






parameter_plots <- my_data %>%
  rowwise() %>%
  mutate(
    parameter_plot = (
      parameter_plot(data,field)
    ) %>%
      ggplot_image(height = px(75)),
    data = NULL,

  )

# Now start to make the table
parameter_sparklines <- parameter_plots %>%
  select(-field) %>%
  pivot_wider(names_from = "depth",values_from = "parameter_plot",names_prefix = "d") %>%
  group_by(model,name,approach) %>%
  nest() %>%
  pivot_wider(names_from = "approach",values_from="data") %>%
  unnest_wider(col=c("field"),names_sep=".") %>%
  unnest_wider(col=c("field-linear"),names_sep=".") %>%
  unnest_wider(col=c("incubation-field"),names_sep=".") %>%
  unnest_wider(col=c("incubation-field-linear"),names_sep=".") %>% rename_with( ~ tolower(gsub("-", ".", .x, fixed = TRUE)))


# Works for field and incubation field
make_field_parameter_table <- function(input_vector,column_select,save_name,table_name=NULL) {

  test_data <- input_vector %>%
    select(c("model","name",column_select)) %>%
    na.omit() %>%
    pivot_wider(names_from = "name",values_from=column_select)


  col_names <<- names(test_data)[-(1)]

  out_plot <- test_data %>%
    mutate(model = factor(model,levels=c("null","microbe","quality","microbe-mult","quality-mult"),
                          labels =c("Null","Microbe","Quality","Microbe-mult","Quality-mult") )) %>%
    ungroup() %>%
    gt(rowname_col = "model")  %>%
    fmt_markdown(columns=vars(col_names)) %>%
    fmt_missing(columns = everything(),missing_text = "") %>%
   tab_options(data_row.padding = px(00)) %>%
   cols_label(
    "Q10M" = html("<em>Q<sub>10,M</sub></em>"),
    "Q10R" = html("<em>Q<sub>10,R</sub></em>"),
    "kR" = html("<em>k<sub>R</sub></em>"),
    "kM" = html("<em>k<sub>M</sub></em>"),
    "kA" = html("<em>k<sub>A</sub></em>"),
    "mu" = "\u03BC",
    "epsilon" = "\u03B5",
    "kS" = html("<em>k<sub>S</sub></em>")
 # "f" = html("<em>f</em>"),
#  "gR" = html("<em>g<sub>R</sub></em>")
   )

  # Add on the title if we have it
  if(!is.null(table_name)) { out_plot <- out_plot %>%
    tab_header(title = table_name)}

  #return(out_plot)

  # Save the plot
  gtsave(out_plot,filename=save_name,vwidth = 1200)

}

# Field linear
make_field_linear_parameter_table <- function(input_vector,column_select,save_name,table_name=NULL) {

  test_data <- input_vector %>%
    select(c("model","name",column_select)) %>%
    na.omit() %>%
    pivot_wider(names_from = "name",values_from=column_select)


  col_names <<- names(test_data)[-(1)]

  out_plot <- test_data %>%
    mutate(model = factor(model,levels=c("null","microbe","quality","microbe-mult","quality-mult"),
                          labels =c("Null","Microbe","Quality","Microbe-mult","Quality-mult") )) %>%
    ungroup() %>%
    gt(rowname_col = "model")  %>%
    fmt_markdown(columns=vars(col_names)) %>%
    fmt_missing(columns = everything(),missing_text = "") %>%
    tab_options(data_row.padding = px(00)) %>%
    cols_label(
      "Q10M" = html("<em>Q<sub>10,M</sub></em>"),
      #"Q10R" = html("<em>Q<sub>10,R</sub></em>"),
     # "kR" = html("<em>k<sub>R</sub></em>"),
      "kM" = html("<em>k<sub>M</sub></em>"),
      "kA" = html("<em>k<sub>A</sub></em>"),
      "mu" = "\u03BC",
      "epsilon" = "\u03B5",
      "kS" = html("<em>k<sub>S</sub></em>"),
      # "f" = html("<em>f</em>"),
        "gR" = html("<em>g<sub>R</sub></em>")
    )

  # Add on the title if we have it
  if(!is.null(table_name)) { out_plot <- out_plot %>%
    tab_header(title = table_name)}

  #return(out_plot)

  # Save the plot
  gtsave(out_plot,filename=save_name,vwidth = 1200)


}

# Incubation Field linear
make_incubation_field_linear_parameter_table <- function(input_vector,column_select,save_name,table_name=NULL) {

  test_data <- input_vector %>%
    select(c("model","name",column_select)) %>%
    na.omit() %>%
    pivot_wider(names_from = "name",values_from=column_select)


  col_names <<- names(test_data)[-(1)]

  out_plot <- test_data %>%
    mutate(model = factor(model,levels=c("null","microbe","quality","microbe-mult","quality-mult"),
                          labels =c("Null","Microbe","Quality","Microbe-mult","Quality-mult") )) %>%
    ungroup() %>%
    gt(rowname_col = "model")  %>%
    fmt_markdown(columns=vars(col_names)) %>%
    fmt_missing(columns = everything(),missing_text = "") %>%
    tab_options(data_row.padding = px(00)) %>%
    cols_label(
      "Q10M" = html("<em>Q<sub>10,M</sub></em>"),
      #"Q10R" = html("<em>Q<sub>10,R</sub></em>"),
      # "kR" = html("<em>k<sub>R</sub></em>"),
      "kM" = html("<em>k<sub>M</sub></em>"),
      "kA" = html("<em>k<sub>A</sub></em>"),
      "mu" = "\u03BC",
      "epsilon" = "\u03B5",
      "kS" = html("<em>k<sub>S</sub></em>"),
       "f" = html("<em>f</em>"),
      "gR" = html("<em>g<sub>R</sub></em>")
    )

  # Add on the title if we have it
  if(!is.null(table_name)) { out_plot <- out_plot %>%
    tab_header(title = table_name)}

  #return(out_plot)

  # Save the plot
  gtsave(out_plot,filename=save_name,vwidth = 1200)


}

# Just do the IFL at 5 cm here:

####

# Make the plot
 make_field_parameter_table(parameter_sparklines,
                            "field.d5",
                            save_name = 'manuscript-figures/S02.1.parameter-sparkline-table.field.d5.png',
                            table_name = "Field approach; 5 cm depth")

 make_field_parameter_table(parameter_sparklines,
                            "field.d10",
                            save_name = 'manuscript-figures/S02.2.parameter-sparkline-table.field.d10.png',
                            table_name = "Field approach; 10 cm depth")

 make_field_parameter_table(parameter_sparklines,
                            "field.dall",
                            save_name = 'manuscript-figures/S02.3.parameter-sparkline-table.field.dall.png',
                            table_name = "Field approach; All depths")

 ###  Field Linear
 make_field_linear_parameter_table(parameter_sparklines,
                            "field.linear.d5",
                            save_name = 'manuscript-figures/S03.1.parameter-sparkline-table.field.linear.d5.png',
                            table_name = "Field Linear approach; 5 cm depth")

 make_field_linear_parameter_table(parameter_sparklines,
                            "field.linear.d10",
                            save_name = 'manuscript-figures/S03.2.parameter-sparkline-table.field.linear.d10.png',
                            table_name = "Field Linear approach; 10 cm depth")

 make_field_linear_parameter_table(parameter_sparklines,
                            "field.linear.dall",
                            save_name = 'manuscript-figures/S03.3.parameter-sparkline-table.field.linear.dall.png',
                            table_name = "Field Linear approach; All depths")

 ###  Incuation Field
 make_field_parameter_table(parameter_sparklines,
                            "incubation.field.d5",
                            save_name = 'manuscript-figures/S04.1.parameter-sparkline-table.incubation.field.d5.png',
                            table_name = "Incubation Field approach; 5 cm depth")

 make_field_parameter_table(parameter_sparklines,
                            "incubation.field.d10",
                            save_name = 'manuscript-figures/S04.2.parameter-sparkline-table.incubation.field.d10.png',
                            table_name = "Incubation Field approach; 10 cm depth")

 make_field_parameter_table(parameter_sparklines,
                            "incubation.field.dall",
                            save_name = 'manuscript-figures/S04.3.parameter-sparkline-table.incubation.field.dall.png',
                            table_name = "Incubation Field approach; All depths")

 ###  Incuation Field Linear
 make_incubation_field_linear_parameter_table(parameter_sparklines,
                                   "incubation.field.linear.d5",
                                   save_name = 'manuscript-figures/S05.1.parameter-sparkline-table.incubation.field.linear.d5.png',
                                   table_name = "Incubation Field Linear approach; 5 cm depth")

 make_incubation_field_linear_parameter_table(parameter_sparklines,
                                   "incubation.field.linear.d10",
                                   save_name = 'manuscript-figures/S05.2.parameter-sparkline-table.incubation.field.linear.d10.png',
                                   table_name = "Incubation Field Linear approach; 10 cm depth")

 make_incubation_field_linear_parameter_table(parameter_sparklines,
                                   "incubation.field.linear.dall",
                                   save_name = 'manuscript-figures/S05.3.parameter-sparkline-table.incubation.field.linear.dall.png',
                                   table_name = "Incubation Field Linear approach; All depths")


 ###

 # The figure used for the manuscript
make_incubation_field_linear_parameter_table(parameter_sparklines,"incubation.field.linear.d5",
                                             save_name = 'manuscript-figures/05-parameter-sparkline-table.png')

# Save the data for later use
save(parameter_sparklines,file = 'estimate-results/stats-results/sparkline-parameter-data.Rda')


