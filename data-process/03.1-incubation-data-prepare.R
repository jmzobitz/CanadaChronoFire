# Prepare the incubation data for modeling activities
library(tidyverse)

### NOTE: In examining the historical fire maps we found that the 1969 fire * actually * occurred in 1968 (previous studies had this coded as 1969).  This is code that recodes these values

### STEP 1: read in the incubation data and convert units

### STEP 1a: the soil Carbon amounts in each plot (gC m2)
soil_carbon_data <- readxl::read_xlsx('data-raw/Canada C kg m2.xlsx') %>%
  rename(Year = year,
         Area = line,
         Plot = plot,
         soilC = "C kg m2") %>%
  mutate(Year = as.character(Year),
         soilC = soilC*1000) %>%  # Convert to gC m2
  mutate(Year = if_else(Year == "3","N2012",Year),
         Year = if_else(Year == "25","N1990",Year),
         Year = if_else(Year == "46","N1968",Year),  # Adjust year, see note above
         Year = if_else(Year == "100","NC",Year))


# Determine characteristics of the soil carbon profile
soil_carbon_summary <- soil_carbon_data %>% group_by(Year,depth) %>%
  summarise(q_value = quantile(soilC, c(0.025, 0.5, 0.975),na.rm=TRUE),
            q_name = c("q0.025", "q0.5", "q0.975")) %>%
  ungroup() %>%
  group_by(Year,q_name) %>%
  mutate(q_prop = (q_value)/sum(q_value)) %>%
  ungroup()




### STEP 1b: the incubation data
incubation_data <- readxl::read_xlsx('data-raw/Canada data 2015.xlsx',sheet = 'combined') %>%
  select(1:8) %>%
  mutate(across(.cols=c(5:8),as.numeric)) %>%
  pivot_longer(cols=c(5:8),names_to="temperature",values_to="respiration_per_soil") %>%
  mutate(temperature = as.numeric(str_extract(temperature,"[[:digit:]]+"))) %>%
  rename(depth = 4,
         Year = area,
         Area = line,
         Plot = plot) %>%
  mutate(Year = if_else(Year == "N12","N2012",Year),
         Year = if_else(Year == "N90","N1990",Year),
         Year = if_else(Year == "N69","N1968",Year)) %>% # Adjust year, see note above
  mutate(respiration_per_soil_new = respiration_per_soil*1E-6 * (12 / 44) * 24) ### Change efflux from ug C02 g-1 h-1 to g C g-1 day-1

### STEP 1c: the microbe data
microbe_incubation_data <- readxl::read_xlsx('data-raw/MicrobeData_Canada.xlsx') %>%
  rename(site_code=1) %>%
  separate(site_code,into=c("area","line",NA),sep = "_") %>%
  separate(line,into=c("line","plot"),sep = 1,remove=FALSE) %>%
  select(1,3:5,9,12,13,16) %>%
  rename(depth = 4,
         Year = area,
         Area = line,
         Plot = plot,
         CS_per_soil = "soilC (mg g-1)",
         CM_per_soil="MBC (mg g-1)",
         CA_per_soil = "Soil available C (mg g-1)",
         fW = "AvMoi (%)") %>%
  mutate(Year = if_else(Year == "N12","N2012",Year),
         Year = if_else(Year == "N90","N1990",Year),
         Year = if_else(Year == "N69","N1968",Year),
         fW = fW/100,
         Plot = as.double(Plot)) %>%
  na.omit()





### STEP 2: Join the incubation data with the soil carbon data
joined_incubation <- incubation_data %>%
  inner_join(soil_carbon_data,by=c("Year","Area","Plot","depth")) %>%
  inner_join(microbe_incubation_data,by=c("Year","Area","Plot","depth")) %>%
  mutate(respiration = respiration_per_soil_new*soilC, # respiration is now g C m-2 day-1
         #CS = CS_per_soil*soilC/1000,
         CS = soilC,
         CM = CM_per_soil*soilC/1000,  # microbeC from mg g to is now g C m-2 day-1
         CA = CA_per_soil*soilC/1000)  # available C from mg g to is now g C m-2 day-1




# Save data files for later use
save(soil_carbon_data,
     soil_carbon_summary,
     incubation_data,
     joined_incubation,
     file="data-process/data-outputs/incubation-soil-data.Rda")

