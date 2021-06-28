# This code produces the three maps for Figure 1: site locations

library(tidyverse)
library(leaflet.providers)
library(leaflet)


# Load up the locations for each of the site plots
load('data-process/data-outputs/study-sites.Rda')


### Now we want to get the other polygons imported in here

load('data-process/data-outputs/fire_1968.Rda')
load('data-process/data-outputs/fire_1990.Rda')
load('data-process/data-outputs/fire_2012.Rda')


# Get the sites loaded up in a way so they are easy to label.
my_sites <- sites %>%
  separate(site_name,into=c("site_name",NA),sep="-") %>%
  mutate(site_name = factor(site_name,levels=c("N2012","N1990","N1968","NC"),
                            labels=c("2012","1990","1968","Control")) )

my_colors <- c('#e41a1c','#377eb8','#4daf4a','#984ea3')

# Define the pallette
pal <- colorFactor(my_colors, my_sites$site_name,
                   na.color = "transparent")

# Base map:

base_map <- leaflet(options = leafletOptions(
  attributionControl=TRUE)) %>%
  addTiles(attribution = '© OpenStreetMap contributors 2021. Distributed under the Open Data Commons Open Database License (ODbL) v1.0.') %>%
  setView(lng = -136, lat=67,zoom=4) %>%
  addRectangles(
    lng1=-134.3, lat1=68.2,
    lng2=-133.1, lat2=67.8,
    color = 'blue',
    fillColor = "transparent"
  ) %>% leafem::addMouseCoordinates() %>%
  addRectangles(
    lng1=-137.7, lat1=66.4,
    lng2=-136.5, lat2=65.85,
    color='red',
    fillColor = "transparent"
  ) %>% addScaleBar()

base_map

# Map 1:
# zoom 8 -133.59, 68.025
# zoom 8, lat = -137, long = 66.15
map1 <- leaflet(options = leafletOptions(
  attributionControl=TRUE)) %>%
  addTiles(attribution = '© OpenStreetMap contributors 2021. Distributed under the Open Data Commons Open Database License (ODbL) v1.0.') %>%
  setView(lng = -136.94, lat=66.15,zoom=9) %>%
  addCircleMarkers(
    data = my_sites,
    lng = ~long,
    lat = ~lat,
    radius = 6,
    color = ~pal(site_name),
    stroke = FALSE, fillOpacity = 0.6
  )  %>%
  addPolygons(data=fire_1968,color = my_colors[1], weight = 1, smoothFactor = 0.5,opacity = 1.0, fillOpacity = 0.4,group="1968 Fires.") %>%
  addPolygons(data=fire_1990,color = my_colors[2], weight = 1, smoothFactor = 0.5,opacity = 1.0, fillOpacity = 0.4,group="1990 Fires") %>%
  addPolygons(data=fire_2012,color = my_colors[3], weight = 1, smoothFactor = 0.5,opacity = 1.0, fillOpacity = 0.4,group="2012 Fires") %>%
  addLegend(pal = pal,
            values = my_sites$site_name,
            title = "Chronosequence Site")  %>%
  addRectangles(
    lng1=-137.7, lat1=66.4,
    lng2=-136.5, lat2=65.85,
    color='red',
    fillColor = "transparent"
  ) %>% addScaleBar()



# Map 2:
# zoom 8 -133.59, 68.025
map2 <- leaflet(options = leafletOptions(
  attributionControl=TRUE)) %>%
  addTiles(attribution = '© OpenStreetMap contributors 2021. Distributed under the Open Data Commons Open Database License (ODbL) v1.0.') %>%
  setView(lng = -133.59, lat=68.025,zoom=9) %>%
  addCircleMarkers(
    data = my_sites,
    lng = ~long,
    lat = ~lat,
    radius = 6,
    color = ~pal(site_name),
    stroke = FALSE, fillOpacity = 0.6
  )  %>%
  addPolygons(data=fire_1968,color = my_colors[1], weight = 1, smoothFactor = 0.5,opacity = 1.0, fillOpacity = 0.4,group="1968 Fires.") %>%
  addPolygons(data=fire_1990,color = my_colors[2], weight = 1, smoothFactor = 0.5,opacity = 1.0, fillOpacity = 0.4,group="1990 Fires") %>%
  addPolygons(data=fire_2012,color = my_colors[3], weight = 1, smoothFactor = 0.5,opacity = 1.0, fillOpacity = 0.4,group="2012 Fires") %>%
  addLegend(pal = pal,
            values = my_sites$site_name,
            title = "Chronosequence Site") %>%
  addRectangles(
    lng1=-134.3, lat1=68.2,
    lng2=-133.1, lat2=67.8,
    color = 'blue',
    fillColor = "transparent"
  ) %>% addScaleBar()

# Now save all the maps together
mapview::mapshot(base_map, file = 'manuscript-figures/01-a-base-map.png',
                 remove_controls = c("zoomControl", "layersControl", "homeButton", "drawToolbar", "easyButton"))
mapview::mapshot(map2, file = 'manuscript-figures/01-b-north-sites.png',
                 remove_controls = c("zoomControl", "layersControl", "homeButton", "drawToolbar", "easyButton"))
mapview::mapshot(map1, file = 'manuscript-figures/01-c-south-sites.png',
                 remove_controls = c("zoomControl", "layersControl", "homeButton", "drawToolbar", "easyButton"))


