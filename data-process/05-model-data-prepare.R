# Prepare in the data (joining incubation and field data) used for data assimilation.

# We also merge and add the data sets (All sites, all depths) for analysis.

# Data will be exported this as a package dataset we will be use in model analyses

# Load up incubation data
load("data-process/data-outputs/incubation-soil-data.Rda")

# Nest the incubation data by site
incubation_data <- joined_incubation %>% na.omit() %>% group_by(Year,depth) %>% nest() %>%
  rename(incubation = data)

# Load up the field data
load("data-process/data-outputs/soil-field-data.Rda")
field_data <- joined_field %>% na.omit() %>%
  rename(temperature = tSoil,respiration = rSoil) %>%
  group_by(Year,depth) %>%
  nest() %>%
  rename(field = data)


# Merge the incubation and field datasets together
joined_data <- incubation_data %>%
  inner_join(field_data,by=c("Year","depth")) %>%
  mutate(depth=as.character(depth))

# Next merge the data together, keeping depth as a grouping variable
merged_year <- joined_data %>% group_by(depth) %>%
  nest() %>%
  mutate(data = map(.x=data,.f=~list(incubation=bind_rows(.x$incubation), field = bind_rows(.x$field) )) ) %>%
  unnest_wider(data) %>%
  mutate(Year = 'NALL')

# Then get all the sites all / all depths wrapped up into one
merged_all <- merged_year %>%
  mutate(depth = 'ALL') %>%
  nest() %>%
  mutate(data = map(.x=data,.f=~list(incubation=bind_rows(.x$incubation), field = bind_rows(.x$field) )) ) %>%
  unnest_wider(data) %>%
  mutate(Year = 'NALL')

# Merge all the measurements from a site together
merged_site <- joined_data %>% group_by(Year) %>%
  nest() %>%
  mutate(data = map(.x=data,.f=~list(incubation=bind_rows(.x$incubation), field = bind_rows(.x$field) )) ) %>%
  unnest_wider(data) %>%
  mutate(depth = 'ALL')

# Then add this to the combined data
combined_data <- rbind(joined_data,merged_year,merged_site,merged_all)

# Export to the package
use_data(combined_data,overwrite=TRUE)
