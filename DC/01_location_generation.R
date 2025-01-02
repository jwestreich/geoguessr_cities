library(tibble)
library(dplyr)
library(readr)
library(sf)
library(httr)

code_base<-"C:/Users/jwest/Documents/geoguessr_cities/DC/"
output_location<-"C:/Users/jwest/Documents/geoguessr_cities/DC/output/"
round_no<-3

# Define the polygon using coordinates
polygon_coords <- matrix(c(
  -77.119419, 38.934718,
  -77.040882, 38.995841,
  -76.909394, 38.892852,
  -77.029331, 38.799234, 
  -77.027054, 38.855403, 
  -77.059426, 38.899524, 
  -77.119419, 38.934718 # Close the polygon
), ncol = 2, byrow = TRUE)

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
locations <- st_coordinates(generate_random_points(polygon_sf, n = 10)) %>%
  as_tibble() %>%
  rename(longitude = X, latitude = Y)%>%
  mutate(location=paste0(latitude,",",longitude),
         seqnum=row_number())

write_csv(locations, paste0(output_location,"Round ",round_no,"/locations.csv"))

camera_angles<-c("0","72","144","216","288")

for (i in 1:nrow(locations)){
  for (j in 1:length(camera_angles)){
    # Parameters
    api_key <- Sys.getenv("GOOGLE_MAPS_API_KEY")
    location <- locations$location[i] # Latitude,Longitude
    size <- "2048x2048"                 # Image size in pixels
    heading <- camera_angles[j]                    # Direction of the camera in degrees
    pitch <- "0"                      # Angle of the camera
    output_file <- paste0(output_location,"Round ",round_no,"/location_",i,"_image_",j,".jpg")  # Name of the output file
    
    # Construct the URL
    url <- paste0(
      "https://maps.googleapis.com/maps/api/streetview?",
      "size=", size, "&",
      "location=", location, "&",
      "heading=", heading, "&",
      "pitch=", pitch, "&",
      "key=", api_key
    )
    
    # Download and save the image
    response <- GET(url, write_disk(output_file, overwrite = TRUE))
    
    # Check if the download was successful
    if (response$status_code == 200) {
      message("Image downloaded successfully!")
    } else {
      message("Failed to download image: ", response$status_code)
    }
    Sys.sleep(1)
  }
}
