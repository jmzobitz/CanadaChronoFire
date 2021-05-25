# This code processes shapefile data for the fire maps in the figure, as well as the locations of each of the sites.

# NOTE: The chronosequence sites

library(tidyverse)
library(rgdal)


# Load up data on all the sites
sites <- read_csv('data-raw/appears_data.csv') %>% select(-Category) %>%
  rename(site_name = ID,
         lat = latitude,
         long = longitude)


# Save the data (both the sites sites together)
save(sites,
     file = "data-process/study-sites.Rda")



# read in fire shapefile data
# Downloaded from: https://cwfis.cfs.nrcan.gc.ca/datamart - select National Fire Database fire polygon data  (too large to store shapefiles on github)

# Read in the shapefile data and save it to file
fire_data <-  readOGR("data-raw/Fire-Shapefiles/NFDB_poly/NFDB_poly_20201005.shp")

fire_2012 <- subset(fire_data, fire_data$YEAR =="2012" & fire_data$SRC_AGENCY %in% c("YT","NT")) %>%
  spTransform("+init=epsg:4326")
fire_1990 <- subset(fire_data, fire_data$YEAR =="1990" & fire_data$SRC_AGENCY %in% c("YT","NT")) %>%
  spTransform("+init=epsg:4326")
fire_1968 <- subset(fire_data, fire_data$YEAR =="1968" & fire_data$SRC_AGENCY %in% c("YT","NT")) %>%
  spTransform("+init=epsg:4326")




save(file = "data-process/data-outputs/fire_2012.Rda",fire_2012)
save(file = "data-process/data-outputs/fire_1990.Rda",fire_1990)
save(file = "data-process/data-outputs/fire_1968.Rda",fire_1968)
