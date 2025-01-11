locations_draft<-data.frame()

for (j in 1:10){

  borough_picker<-runif(n=1)
  
  if (city=="DC"){
    borough <- case_when(
      city == "DC" & borough_picker <= at_large ~ "At Large",
      city == "DC" & borough_picker <= downtown ~ "Downtown"
    )
  }
  
  if (city=="NYC"){
    borough <- case_when(
      city == "NYC" & borough_picker <= manhattan ~ "Manhattan",
      city == "NYC" & borough_picker <= brooklyn ~ "Brooklyn",
      city == "NYC" & borough_picker <= queens ~ "Queens",
      city == "NYC" & borough_picker <= bronx ~ "Bronx"
    )
  }
  
  if (borough == "At Large") {
    polygon_coords<-at_large_coords
  }
  if (borough == "Downtown") {
    polygon_coords<-downtown_coords
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
    rename(longitude = X, latitude = Y)%>%
    mutate(location=paste0(latitude,",",longitude),
           borough=borough)
  
  locations_draft<-bind_rows(locations_draft, location_single)
}
