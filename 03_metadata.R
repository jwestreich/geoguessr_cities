locations <- locations_draft_spaced %>%
  rowwise() %>%
  mutate(
    valid = {
      coords <- str_split(location, ",")[[1]]
      lat <- coords[1]
      lng <- coords[2]
      res <- fromJSON(GET(
        paste0(
          "https://maps.googleapis.com/maps/api/streetview/metadata?location=",
          location, "&key=", Sys.getenv("GOOGLE_MAPS_API_KEY")
        )
      ) %>% content("text"))
      res$status == "OK"
    },
    latitude = {
      if (valid) {
        coords <- str_split(location, ",")[[1]]
        res <- fromJSON(GET(
          paste0(
            "https://maps.googleapis.com/maps/api/streetview/metadata?location=",
            location, "&key=", Sys.getenv("GOOGLE_MAPS_API_KEY")
          )
        ) %>% content("text"))
        res$location$lat
      } else {
        NA
      }
    },
    longitude = {
      if (valid) {
        coords <- str_split(location, ",")[[1]]
        res <- fromJSON(GET(
          paste0(
            "https://maps.googleapis.com/maps/api/streetview/metadata?location=",
            location, "&key=", Sys.getenv("GOOGLE_MAPS_API_KEY")
          )
        ) %>% content("text"))
        res$location$lng
      } else {
        NA
      }
    }
  ) %>%
  ungroup() %>%
  filter(valid == TRUE)%>%
  mutate(seqnum=row_number())%>%
  filter(seqnum<=5)

if (nrow(locations) < 5) {
  stop("Less than 5 valid locations found")
}

#locations$location[1]<-"40.716847,-73.936363"

write_csv(locations, paste0(output_location,"Round ",round_no,"/locations.csv"))


# leaflet(locations) %>%
#   addProviderTiles(providers$CartoDB.Positron) %>%
#   addCircleMarkers(~longitude_draft, ~latitude_draft, fillColor = "blue", fillOpacity = 1, color = "black", stroke = TRUE,weight = 2, radius = 5, label = ~paste("Lat:", latitude_draft, "Lon:", longitude_draft)) %>%
#   addCircleMarkers(~longitude, ~latitude, fillColor = "red", fillOpacity = 1, color = "black", stroke = TRUE,weight = 2, radius = 5, label = ~paste("Exact Lat:", latitude, "Exact Lon:", longitude))
