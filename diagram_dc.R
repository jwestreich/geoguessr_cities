library(tibble)
library(dplyr)
library(readr)
library(sf)
library(httr)
library(jsonlite)
library(purrr)
library(tidyverse)
library(janitor)
library(googlesheets4)
library(stringr)
library(geosphere)
library(leaflet)
library(magick)
library(metro)

metro_stations<-rail_stations(LineCode = NULL, api_key = wmata_key(Sys.getenv("WMATA_API_KEY")))%>%
  filter(State=="DC" & StationName!="Friendship Heights" & StationName!="Deanwood" & StationName!="Takoma")%>%
  select(StationName,Lat,Lon)%>%
  distinct()

at_large_coords <- matrix(c(
  -77.119419, 38.934718,
  -77.040882, 38.995841,
  -76.909394, 38.892852,
  -77.029331, 38.799234, 
  -77.027054, 38.855403, 
  -77.059426, 38.899524, 
  -77.092116, 38.906666, 
  -77.119419, 38.934718
), ncol = 2, byrow = TRUE)

downtown_coords <- matrix(c(
  -77.045444, 38.911450, 
  -77.003304, 38.896285, 
  -77.003261, 38.885795, 
  -77.033344, 38.884692, 
  -77.039652, 38.887867, 
  -77.047162, 38.884158, 
  -77.056432, 38.893812, 
  -77.056861, 38.902398, 
  -77.045444, 38.911450 
), ncol = 2, byrow = TRUE)

greater_central_coords <- matrix(c(
  -77.071324, 38.904877,
  -77.071323, 38.942876,
  -77.027723, 38.945969,
  -77.000524, 38.931387,
  -76.983436, 38.899866,
  -76.988088, 38.877886,
  -77.020513, 38.875749,
  -77.050640, 38.886504,
  -77.057420, 38.901337,
  -77.071324, 38.904877
), ncol = 2, byrow = TRUE)

leaflet() %>%
  addTiles() %>%
  addPolygons(
    lng = at_large_coords[, 1],
    lat = at_large_coords[, 2],
    color = "yellow",
    weight = 2,
    fillColor = "yellow",
    fillOpacity = 0.5
  )%>%
  addPolygons(
    lng = downtown_coords[, 1],
    lat = downtown_coords[, 2],
    color = "green",
    weight = 2,
    fillColor = "green",
    fillOpacity = 0.5
  )%>%
  addPolygons(
    lng = greater_central_coords[, 1],
    lat = greater_central_coords[, 2],
    color = "blue",
    weight = 2,
    fillColor = "blue",
    fillOpacity = 0.5
  )%>%
  addCircleMarkers(
    lng = metro_stations$Lon,
    lat = metro_stations$Lat,
    radius = 4,
    color = "red",
    fillColor = "red",
    fillOpacity = .3
  )
