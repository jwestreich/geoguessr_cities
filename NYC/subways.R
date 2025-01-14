library(leaflet)
library(gtfstools)

temp_file <- tempfile(fileext = ".zip")
download.file("http://web.mta.info/developers/data/nyct/subway/google_transit.zip", temp_file)

gtfs_data <- read_gtfs(temp_file)

stations <- gtfs_data$stops%>%
  filter(location_type==1)%>%
  filter(stop_lon>-74.051977)%>%
  select(stop_name,longitude=stop_lon,latitude=stop_lat)%>%
  distinct()


leaflet(data = stations) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addCircles(
    lng = ~longitude, lat = ~latitude,
    radius = 100, color = "blue", fillOpacity = 0.5, stroke = FALSE
  ) %>%
  addCircleMarkers(
    ~longitude, ~latitude,
    radius = 1,
    color = "black",
    fillColor = "black",
    fillOpacity = 1,
    weight = 0
  )
