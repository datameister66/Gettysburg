library(magrittr)
library(leaflet)
library(htmlwidgets)
library(readxl)
mapnoon <- read_excel("C:/Users/ps324/OneDrive - Cummins/documents/mapnoon.xlsx")

# Create a leaflet map with default map tile using addTiles()
leaflet() %>%
  addTiles()

library(tidyverse)
providers
names(providers)
str_detect(names(providers), "CartoDB")
names(providers)[str_detect(names(providers), "CartoDB")]
leaflet() %>% 
  addProviderTiles(provider = "CartoDB.DarkMatter")
# Create a leaflet map that uses the CartoDB.PositronNoLabels provider tile
leaflet() %>%
  addProviderTiles("CartoDB.PositronNoLabels")
#library(ggmap)
data(quakes)
# Map with CartoDB tile centered on DataCamp's NYC office with zoom of 6
leaflet()  %>% 
  addProviderTiles("CartoDB")  %>% 
  setView(lng = -73.98575, lat = 40.74856, zoom = 6)

leaflet() %>% 
  addProviderTiles("Esri") %>% 
  setView(lng = mapnoon$long[2], lat = mapnoon$lat[2], zoom = 10)

# Plot all locations
leaflet() %>% 
  addProviderTiles("Esri") %>% 
  addMarkers(lng = mapnoon$long, lat = mapnoon$lat)

# Add circle markers with popups for college names
map <- leaflet() %>% 
  addProviderTiles("Esri")

map %>%
  addCircleMarkers(data = mapnoon, radius = 2, popup = ~popup)

# Add circle markers with popups that display both the institution name and sector
map %>% 
  addCircleMarkers(data = mapnoon, radius = 2, 
                   popup = ~paste0(Regiment_or_Battery, "<br/>", State))

# Make the institution name in each popup bold
map %>% 
  addCircleMarkers(data = mapnoon, radius = 3, 
                   popup = ~paste0("<b>", State, "</b>", "<br/>", Regiment_or_Battery))

# Add circle markers with labels identifying the name of each college
map %>% 
  addCircleMarkers(data = mapnoon, radius = 2, label = ~Brigade)

# color coding....
#OR <- ipeds %>% 
#  filter(state == "OR")

pal <- colorFactor(palette = c("red", "blue", "#9b4a11"), 
                   levels = c("Public", "Private", "For-Profit"))

oregon_colleges <- 
  OR %>% 
  leaflet() %>% 
  addProviderTiles("CartoDB") %>% 
  addCircleMarkers(radius = 2,
                   color = ~pal(sector_label),
                   label = ~name)

oregon_colleges %>% 
  addLegend(position = "bottomright",
            pal = pal, 
            values = c("Public", "Private", "For-Profit"))

admit <- admit %>% 
  filter(!is.na(rate),
         rate < 50, 
         rate > 0)

pal <- colorNumeric(palette = "Reds", domain = c(1:50), reverse = TRUE)

admit_map <- 
  admit %>% 
  leaflet() %>% 
  addProviderTiles("CartoDB")  %>% 
  addCircleMarkers(radius = 4, color = ~pal(rate), label = ~name)  %>%
  addLegend(title = "Admit Rate", pal = pal, values = c(1:50), 
            position = "bottomright")

# see http://colorbrewer2.org/ for interactive examples
library(RColorBrewer)
display.brewer.all()

library(leaflet.extras)
#leaflet extras
leaflet() %>%
  addTiles() %>%
  addSearchOSM()

leaflet() %>%
  addTiles() %>%
  addSearchOSM() %>% 
  addReverseSearchOSM()

leaflet() %>%
  addTiles() %>%
  addSearchOSM() %>% 
  addReverseSearchOSM() %>% 
  addResetMapButton()

# overlays
m <- 
  mapnoon %>% 
  leaflet() %>% 
  addProviderTiles("CartoDB") 

m %>% 
  addCircleMarkers(radius = 2,
                   label = ~Brigade)
# 
# ca_public <- ipeds  %>% 
#   filter(sector == "Public", 
#          state == "CA") 

m %>% 
  addCircleMarkers(
    data = ca_public,
    group = "Public")

m %>% 
  addCircleMarkers(
    data = ca_public,
    color = ~pal(sector_label),
    group = "Public")  %>%
  
  addCircleMarkers(
    data = ca_private,
    color = ~pal(sector_label),
    group = "Private")  %>%
  
  addCircleMarkers(
    data = ca_profit,
    color = ~pal(sector_label),
    group = "For-Profit")  %>%
  
  addLayersControl(
    overlayGroups = c("Public", 
                      "Private", 
                      "For-Profit"))

# Base Groups
# initialize leaflet map
leaflet() %>%
  # add basemaps with groups
  addTiles(group = "OSM") %>% 
  addProviderTiles("CartoDB", group = "Carto") %>% 
  addProviderTiles("Esri", group = "Esri") %>%
  # add marker layer for each sector with corresponding group name
  addCircleMarkers(data = public, radius = 2, label = ~htmlEscape(name),
                   color = ~pal(sector_label),   group = "Public") %>% 
  addCircleMarkers(data = private, radius = 2, label = ~htmlEscape(name),
                   color = ~pal(sector_label),   group = "Private") %>% 
  addCircleMarkers(data = profit, radius = 2, label = ~htmlEscape(name),
                   color = ~pal(sector_label),   group = "For-Profit") %>%
  # add layer controls for base and overlay groups
  addLayersControl(baseGroups = c("OSM", "Carto", "Esri"), 
                   overlayGroups = c("Public", "Private", "For-Profit"))

# Pieces of Flair
addSearchFeatures(options =
                    searchFeaturesOptions(zoom = 10),
                  targetGroups = 'Public')

ipeds  %>% 
  leaflet() %>% 
  addTiles() %>% 
  addCircleMarkers(radius = 2, color = ~pal(sector_label),
                   clusterOptions = markerClusterOptions())

# plot polygons
glimpse(shp@data)

shp@polygons[[1]] %>% 
  leaflet() %>% 
  addTiles() %>% 
  addPolygons()

shp %>% 
  leaflet() %>% 
  addTiles() %>% 
  addPolygons(weight = 1,
              color = "grey",
              label = ~paste0("Total Income: " dollar(income)),
              highlight = highlightOptions(weight = 3,
                                           color = "red",
                                           bringToFront = TRUE))

nc_pal <- colorNumeric("Blues", domain = high_inc@data$mean_income)
previewColors(pal = nc_pal, values = c(seq(100000, 600000, by = 100000)))
nc_pal <- colorNumeric(palette = "Blues", 
                       domain = shp@data$mean_income)
shp %>% 
  leaflet() %>% 
  # addTiles()  %>% 
  addPolygons(weight = 1,
              fillOpacity = 1,
              color =  ~nc_pal(mean_income),
              label = ~paste0("Mean Income: ", dollar(mean_income)),
              highlight = highlightOptions(weight = 3,
                                           color = "red",
                                           bringToFront = TRUE))

# final
leaflet() %>% 
  addTiles(group = "OSM") %>% 
  addProviderTiles("CartoDB", group = "Carto") %>% 
  addProviderTiles("Esri", group = "Esri") %>%
  addPolygons(data = shp, weight = 1, fillOpacity = .75,
              color =  ~nc_pal(log(mean_income)),
              label = ~paste0("Mean Income: ", dollar(mean_income)),
              group = "Mean Income") %>%
  addCircleMarkers(data = nc_public, radius = 2, label = ~htmlEscape(name),
                   color = ~pal(sector_label),   group = "Public") %>% 
  addCircleMarkers(data = nc_private, radius = 2, label = ~htmlEscape(name),
                   color = ~pal(sector_label),   group = "Private")  %>% 
  addCircleMarkers(data = nc_profit, radius = 2, label = ~htmlEscape(name),
                   color = ~pal(sector_label),   group = "For-Profit") %>%
  addLayersControl(baseGroups = c("OSM", "Carto", "Esri"), 
                   overlayGroups = c("Public", "Private",
                                     "For-Profit", "Mean Income"))

# Store leaflet map in object

m <-
  leaflet() %>% 
  addTiles() %>%
  addMarkers(
    data = ipeds,
    clusterOptions =
      markerClusterOptions())%>% 
  addPolygons(data = shp)
# save leaflet object as html file

library(htmlwidgets)

saveWidget(m, file="myMap.html")


# ggmap -------------------------------------------------------------------

library(ggmap)
corvallis <- c(lon = -123.2620, lat = 44.5646)
# Get map at zoom level 5: map_5
map_5 <- get_map(corvallis, zoom = 5, scale = 1)

# Plot map at zoom level 5
ggmap(map_5)
