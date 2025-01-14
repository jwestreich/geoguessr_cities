locations_draft<-data.frame()

for (j in 1:10){

  borough_picker<-runif(n=1)
  
  if (city=="DC"){
    borough <- case_when(
      city == "DC" & borough_picker <= at_large ~ "At Large",
      city == "DC" & borough_picker <= downtown ~ "Downtown",
      city == "DC" & borough_picker <= greater_central ~ "Greater Central",
      city == "DC" & borough_picker <= metro ~ "Metro"
    )
  }
  
  if (city=="NYC"){
    borough <- case_when(
      city == "NYC" & borough_picker <= manhattan ~ "Manhattan",
      city == "NYC" & borough_picker <= brooklyn ~ "Brooklyn",
      city == "NYC" & borough_picker <= queens ~ "Queens",
      city == "NYC" & borough_picker <= bronx ~ "Bronx",
      city == "NYC" & borough_picker <= subway ~ "Subway"
    )
  }
  
  if (borough == "At Large") {
    polygon_coords<-at_large_coords
  }
  if (borough == "Downtown") {
    polygon_coords<-downtown_coords
  }
  if (borough == "Metro") {
    metro_station<-rail_stations(LineCode = NULL, api_key = wmata_key(Sys.getenv("WMATA_API_KEY")))%>%
      filter(State=="DC" & StationName!="Friendship Heights" & StationName!="Deanwood" & StationName!="Takoma")%>%
      select(StationName,Lat,Lon)%>%
      distinct()%>%
      mutate(rand=runif(n()))%>%
      filter(rand==min(rand))

    # Extract latitude and longitude from the dataframe
    lat <- metro_station$Lat
    lon <- metro_station$Lon
    
    # Function to calculate the new coordinates given distance and bearing
    get_new_coords <- function(lat, lon, distance_ft, bearing) {
      distance_m <- distance_ft * 0.3048 # Convert feet to meters
      dest_point <- destPoint(c(lon, lat), b = bearing, d = distance_m)
      return(dest_point)
    }
    
    # Calculate the new coordinates for the 4 points
    nw_point <- get_new_coords(lat, lon, 100, 315) # Northwest (bearing 315 degrees)
    ne_point <- get_new_coords(lat, lon, 100, 45)  # Northeast (bearing 45 degrees)
    se_point <- get_new_coords(lat, lon, 100, 135) # Southeast (bearing 135 degrees)
    sw_point <- get_new_coords(lat, lon, 100, 225) # Southwest (bearing 225 degrees)
    
    # Create the matrix with the coordinates
    metro_coords <- matrix(c(
      nw_point[1], nw_point[2],
      ne_point[1], ne_point[2],
      se_point[1], se_point[2],
      sw_point[1], sw_point[2],
      nw_point[1], nw_point[2] # Close the loop
    ), ncol = 2, byrow = TRUE)
    
    polygon_coords<-metro_coords
  }
  if (borough == "Manhattan") {
    polygon_coords<-manhattan_coords
  }
  if (borough == "Brooklyn") {
    polygon_coords<-brooklyn_coords
  }
  if (borough == "Queens") {
    polygon_coords<-queens_coords
  }
  if (borough == "Bronx") {
    polygon_coords<-bronx_coords
  }
  if (borough == "Subway") {

    subway_station <- gtfs_data$stops%>%
      filter(location_type==1)%>%
      filter(stop_lon>-74.051977)%>%
      select(stop_name,Lon=stop_lon,Lat=stop_lat)%>%
      distinct()%>%
      mutate(rand=runif(n()))%>%
      filter(rand==min(rand))
    
    # Extract latitude and longitude from the dataframe
    lat <- subway_station$Lat
    lon <- subway_station$Lon
    
    # Function to calculate the new coordinates given distance and bearing
    get_new_coords <- function(lat, lon, distance_ft, bearing) {
      distance_m <- distance_ft * 0.3048 # Convert feet to meters
      dest_point <- destPoint(c(lon, lat), b = bearing, d = distance_m)
      return(dest_point)
    }
    
    # Calculate the new coordinates for the 4 points
    nw_point <- get_new_coords(lat, lon, 100, 315) # Northwest (bearing 315 degrees)
    ne_point <- get_new_coords(lat, lon, 100, 45)  # Northeast (bearing 45 degrees)
    se_point <- get_new_coords(lat, lon, 100, 135) # Southeast (bearing 135 degrees)
    sw_point <- get_new_coords(lat, lon, 100, 225) # Southwest (bearing 225 degrees)
    
    # Create the matrix with the coordinates
    subway_coords <- matrix(c(
      nw_point[1], nw_point[2],
      ne_point[1], ne_point[2],
      se_point[1], se_point[2],
      sw_point[1], sw_point[2],
      nw_point[1], nw_point[2] # Close the loop
    ), ncol = 2, byrow = TRUE)
    
    polygon_coords<-subway_coords
  }
  
  
  # Create an sf polygon
  polygon_sf <- st_sfc(st_polygon(list(polygon_coords)), crs = 4326)

  # Generate random points within the polygon
  generate_random_points <- function(polygon, n = 1) {
    bbox <- st_bbox(polygon)
    points <- tibble(
      lon = runif(n * 5, bbox["xmin"], bbox["xmax"]), # Generate extra points for filtering
      lat = runif(n * 5, bbox["ymin"], bbox["ymax"])
    ) %>%
      st_as_sf(coords = c("lon", "lat"), crs = 4326)
    
    points_within <- points[st_intersects(points, polygon, sparse = FALSE), ]
    if (nrow(points_within) < n) {
      # Generate more points if needed
      rbind(points_within, generate_random_points(polygon, n - nrow(points_within)))
    } else {
      points_within[1:n, ]
    }
  }
  
  # Convert to dataframe with latitude and longitude
  location_single <- st_coordinates(generate_random_points(polygon_sf, n = 1)) %>%
    as_tibble() %>%
    rename(longitude_draft = X, latitude_draft = Y)%>%
    mutate(location=paste0(latitude_draft,",",longitude_draft),
           borough=borough)
  
  locations_draft<-bind_rows(locations_draft, location_single)
}
