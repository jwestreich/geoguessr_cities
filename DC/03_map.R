library(leaflet)
library(dplyr)
library(tidyverse)
library(readr)
library(janitor)
library(googlesheets4)
library(stringr)
library(geosphere)

output_location<-"C:/Users/jwest/Documents/geoguessr_cities/DC/output/"
round_no<-1

score_by_location<-read_csv(paste0(output_location,"Round ",round_no,"/results by location.csv"))

seqnum_list <- unique(score_by_location$seqnum)

for (i in 1:length(seqnum_list)){
  filtered_data <- score_by_location %>% filter(seqnum == seqnum_list[i])
  
  # Create a leaflet map
  map <- leaflet() %>%
    addProviderTiles(providers$CartoDB.Positron) 
  
  # Add lines connecting each guess to its corresponding actual coordinate
  for (i in 1:nrow(filtered_data)) {
    map <- map %>%
      addPolylines(
        lng = c(filtered_data$longitude[i], filtered_data$longitude_guess[i]),
        lat = c(filtered_data$latitude[i], filtered_data$latitude_guess[i]),
        color = "black", opacity = 1, weight=1
      )
  }
  
  # Add points for actual coordinates
  map <- map %>%
    addCircleMarkers(
      data = filtered_data,
      lng = ~longitude, lat = ~latitude, 
      color = "yellow", stroke = TRUE, weight = 4,
      fillColor = "black", fillOpacity = 1, radius = 5,
      popup = ~paste("Actual location ", max(filtered_data$seqnum))
    )
  
  # Add points for guessed coordinates
  map <- map %>%
    addCircleMarkers(
      data = filtered_data,
      lng = ~longitude_guess, lat = ~latitude_guess, 
      color = "black", stroke = TRUE, weight = 2,
      fillColor = "red", fillOpacity = 1, radius = 5,
      popup = ~paste(team_name,"\nScore: ",score)
    )%>%
    setView(lng = -77.0369, lat = 38.9072, zoom = 11)
  
  # Render the map
  print(max(filtered_data$seqnum))
  print(map)
}