# This script reads in the field data and processes it so we can apply the parameter estimation models

library(tidyverse)
library(broom)


### STEP 1: read in the incubation data and convert things
load('data-process/data-outputs/incubation-soil-proportion-year.Rda')

# Adjust the names on soil_proportions so we can match that up.
soil_proportions <- soil_proportions %>%
  mutate(Year = if_else(Year=="Control","NC",paste0("N",Year)))

### STEP 2: Load up the CA stats from the field to process
load('data-process/data-outputs/incubation-CA-stats.Rda')  # CA_stats

### STEP 3: Load in the microbe data - this is similar code to when we analyzed the incubation data.
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


### STEP 4: Read in the flux data, join to soil carbon data
field_data <- readxl::read_xlsx('data-raw/Modeling data Canada North.xlsx') %>%
  select(1,8,9,12,15:20) %>%
  separate(1,into=c("Year","Plot")) %>%
  separate(Plot,into=c("Area","Plot"),sep=1) %>%
  mutate(Year = if_else(Year=="N1969","N1968",Year)) %>%  # Change 1969 to 1968
  mutate(
    Plot = as.numeric(Plot),
    rSoil = `Soil Respiration, g/CO2/m2/h`* (12 / 44 ) * (24),  # Convert to g C / m2 day
    fW = `Soil moisture`/100,
    aboveground_biomass = `Total tree biomass, kg m2 (foliage, branches, stems)`*1000,
    CR = `Total tree biomass, kg m2 (foliage, branches, stems)`*1000*0.3, # Convert to gC and the 0.3 is just a guess - Harkonen 2011
    CS_top30 = `Soil C kg m2 in top 30 cm`*1000 )  %>%
  pivot_longer(cols = c("Soil temp 10cm","Soil temp 5cm","Soil temp 2cm"), names_to = "depth", values_to = "tSoil") %>%
  mutate(depth = as.numeric(str_extract(depth,"[[:digit:]]+"))) %>%
  select(Year,Area,Plot,depth,tSoil,aboveground_biomass,rSoil,fW,CR,CS_top30) %>%
  group_by(Year) %>%
  mutate(across(.cols=c("tSoil":"CS_top30"),.fns = ~if_else(is.na(.x),mean(.x,na.rm=TRUE),.x))) %>%
  ungroup()  # FOR NOW, we just take the mean if there is an NA


# join with the microbe data to get CM
joined_field <- field_data %>%
  inner_join(select(microbe_incubation_data,-fW,-CS_per_soil),by=c("Year","Area","Plot","depth")) %>%
  inner_join(soil_proportions,by=c("Year","depth")) %>%
  inner_join(select(CA_stats,Year,depth,lm_fit),by=c("Year","depth")) %>%
  mutate(CS = CS_top30*proportion,
         CM = CM_per_soil*CS/1000,
         CA = map2(.x=lm_fit,.y=CS,.f=~(augment(.x,newdata=tibble(CS=.y))$.fitted)  ),
         CA = as.numeric(CA)) %>%
  select(-lm_fit)# CM_per_soil is mg / g, then weighted by the amount in the soil



# Save the data, now we are ready for the field!
save(joined_field,file="data-process/data-outputs/soil-field-data.Rda")
