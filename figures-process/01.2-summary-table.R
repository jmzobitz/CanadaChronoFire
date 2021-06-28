# This script creates the code for Table 1 in the manuscript
library(tidyverse)
library(gt)
library(FireResp)

# Soil proportions
load('data-process/data-outputs/incubation-soil-proportion-year.Rda')

soil_table <- soil_proportions %>%
  pivot_wider(names_from="depth",values_from="proportion") %>%
  mutate(ALL = `5` + `10`) %>%
  pivot_longer(names_to="depth",values_to="proportion",cols=c("5","10","ALL") ) %>%
  mutate(proportion = round(proportion,digits=2))


# Measured data


incub_sum_cols <- c("CS","CM","CA","respiration")
field_sum_cols <- c("temperature","fW","CS","CM","CA","CR","respiration")

# Rounding out of the columns
no_dec_cols <- c("CS","CR")
one_dec_cols <- c("temperature","CM","CA")
two_dec_cols <- c("respiration","fW")

# Id of the sites
site_order <- tibble(Year = c("N2012","N1990","N1968","NC","NALL"),
                     s_order= 1:5)

depth_order <- tibble(depth = c("5","10","ALL"), d_order = 1:3 )


my_table <- combined_data %>%
  mutate(inc_stats = map(.x=incubation,.f=~summarize(.x,across(.cols=incub_sum_cols,.fns=list(mean = mean, sd = sd),.names = "inc.{.col}.{.fn}")) ) ,
         field_stats = map(.x=field,.f=~summarize(.x,across(.cols=field_sum_cols,.fns=list(mean = mean, sd = sd),.names = "field.{.col}.{.fn}")))
  ) %>%
  select(Year,depth,inc_stats,field_stats) %>% # Select columns we want
  pivot_longer(names_to="name",values_to="value",cols=c("inc_stats","field_stats")) %>%
  mutate(value=map(.x=value,.f=~(.x %>%
                                   mutate(across(.cols=(contains("CS") | contains("CR")),.fns=~round(.x,digits=0)),
                                          across(.cols=(contains("temperature") | contains("CM") | contains("CA")),.fns=~round(.x,digits=1)),
                                          across(.cols=(contains("respiration") | contains("fW") ),.fns=~round(.x,digits=2))


                                   )))) %>%
  pivot_wider() %>%
  unnest(cols=c(inc_stats,field_stats)) %>% inner_join(site_order,by="Year") %>%
  mutate(Year = factor(Year,levels=c("N2012","N1990","N1968","NC"),
                       labels=c("2012","1990","1968","Control")) ) %>%
  inner_join(depth_order,by="depth") %>%
  arrange(s_order,d_order) %>%
  select(-s_order,-d_order) %>%
  inner_join(soil_table,by=c("Year","depth")) %>%
  relocate(proportion,.after="depth") %>%
  ungroup() %>%
  mutate(depth = factor(depth,levels=c("5","10","ALL"),
                        labels=c("5 cm","10 cm","All depths")))




# Now we need to write this out as a gt object
summary_soil_table <-
  gt(data = my_table,
     groupname_col = "Year",
     rowname_col = "depth") %>%
  tab_spanner(
    label = "Incubation Data",
    columns = c(contains("proportion"),contains("inc"))
  ) %>%
  tab_spanner(
    label = "Field Data",
    columns = contains("field")
  ) %>%
  cols_merge(
    columns = c("inc.CS.mean", "inc.CS.sd"),
    pattern = "{1} $\\pm$ {2}"
  ) %>%
  cols_merge(
    columns = c("inc.CA.mean", "inc.CA.sd"),
    pattern = "{1} $\\pm$ {2}"
  ) %>%
  cols_merge(
    columns = c("inc.CM.mean", "inc.CM.sd"),
    pattern = "{1} $\\pm$ {2}"
  ) %>%
  cols_merge(
    columns = c("inc.respiration.mean", "inc.respiration.sd"),
    pattern = "{1} $\\pm$ {2}"
  ) %>%
  ### FIELD DATA
  cols_merge(
    columns = c("field.temperature.mean", "field.temperature.sd"),
    pattern = "{1} $\\pm$ {2}"
  ) %>%
  cols_merge(
    columns = c("field.fW.mean", "field.fW.sd"),
    pattern = "{1} $\\pm$ {2}"
  ) %>%
  cols_merge(
    columns = c("field.CS.mean", "field.CS.sd"),
    pattern = "{1} $\\pm$ {2}"
  ) %>%
  cols_merge(
    columns = c("field.CM.mean", "field.CM.sd"),
    pattern = "{1} $\\pm$ {2}"
  ) %>%
  cols_merge(
    columns = c("field.CA.mean", "field.CA.sd"),
    pattern = "{1} $\\pm$ {2}"
  ) %>%
  cols_merge(
    columns = c("field.CR.mean", "field.CR.sd"),
    pattern = "{1} $\\pm$ {2}"
  ) %>%
  cols_merge(
    columns = c("field.respiration.mean", "field.respiration.sd"),
    pattern = "{1} $\\pm$ {2}"
  ) %>%
  cols_label(
    "inc.CS.mean" = "$C_{S}$",
    "inc.CM.mean" = "$C_{M}$",
    "inc.CA.mean" = "$C_{A}$",
    "inc.respiration.mean" = "$R_{H}$",
    "field.temperature.mean" = "$T_{soil}$",
    "field.fW.mean" = "$f_{W}$",
    "field.CS.mean" = "$C_{S}$",
    "field.CM.mean" = "$C_{M}$",
    "field.CA.mean" = "$C_{A}$",
    "field.CR.mean" = "$C_{R}$",
    "field.respiration.mean" = "$R_{S}$",
    "depth" = "Depth (cm)",
    "proportion" = "Total $C_{S}$ proportion"
  )  %>%
  tab_footnote(
    footnote = "Units are g C m-2",
    locations = cells_column_labels(
      columns = c(contains("CS"), contains("CM"), contains("CR"), contains("CA") )
    )
  ) %>%
  tab_footnote(
    footnote = html("Units are degrees Celsius"),
    locations = cells_column_labels(
      columns = c(contains("temperature"))
    )
  ) %>%
  tab_footnote(
    footnote = html("No units"),
    locations = cells_column_labels(
      columns = c("proportion","field.fW.mean")
    )
  ) %>%
  tab_footnote(
    footnote = html("Units are g C m-2 d-1"),
    locations = cells_column_labels(
      columns = c(contains("respiration"))
    )
  ) %>%
  opt_footnote_marks(marks = "standard") #%>%
# as_latex()



# Show the gt Table
summary_soil_table

tex_table <- summary_soil_table %>% as_latex()

gtsave(summary_soil_table,file = "manuscript-figures/summary_soil_table.tex")

save(summary_soil_table,file = "manuscript-figures/summary_soil_table.Rda")

gtsave(summary_soil_table, filename="manuscript-figures/summary_soil_table.png",vwidth=1200)

