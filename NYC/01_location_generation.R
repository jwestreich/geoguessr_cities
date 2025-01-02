library(tibble)
library(dplyr)
library(readr)
library(sf)
library(httr)

code_base<-"C:/Users/jwest/Documents/geoguessr_cities/NYC/"
output_location<-"C:/Users/jwest/Documents/geoguessr_cities/NYC/output/"
round_no<-1

locations<-data.frame()


for (j in 1:10){
  borough_picker<-runif(n=1)
  borough <- ifelse(borough_picker < 0.5, "Manhattan",
                    ifelse(borough_picker < 0.8, "Brooklyn",
                           ifelse(borough_picker < 0.95,"Queens", 
                                  "Bronx")))
  if (borough == "Manhattan") {
    polygon_coords <- matrix(c(
      -74.013875, 40.700163, 
      -74.024023, 40.704360, 
      -74.014279, 40.755175, 
      -73.927928, 40.878489, 
      -73.910015, 40.872770, 
      -73.934220, 40.834756, 
      -73.933159, 40.808281, 
      -73.927902, 40.796740, 
      -73.942641, 40.782089, 
      -73.940642, 40.775750, 
      -73.970457, 40.742866, 
      -73.969927, 40.728005, 
      -73.975405, 40.710730, 
      -74.013875, 40.700163 
    ), ncol = 2, byrow = TRUE)
  }
  
  if (borough == "Brooklyn") {
    polygon_coords <- matrix(c(
      -73.962692, 40.737428, 
      -73.929043, 40.727534, 
      -73.921881, 40.709394, 
      -73.895402, 40.681704, 
      -73.868821, 40.694847, 
      -73.855515, 40.642089, 
      -73.911528, 40.577762, 
      -74.013005, 40.570018, 
      -74.013976, 40.583109, 
      -74.001595, 40.582741, 
      -74.006693, 40.596383, 
      -74.031212, 40.603387, 
      -74.045535, 40.620895, 
      -74.018345, 40.663263, 
      -74.021016, 40.679282, 
      -73.996496, 40.704684, 
      -73.971977, 40.709469, 
      -73.962692, 40.737428 
    ), ncol = 2, byrow = TRUE)
  }
  
  if (borough == "Queens") {
    polygon_coords <- matrix(c(
      -73.967564, 40.736458,
      -73.931842, 40.779854,
      -73.909949, 40.792220,
      -73.888406, 40.781261,
      -73.887799, 40.767105,
      -73.873233, 40.771977,
      -73.850899, 40.759014,
      -73.855754, 40.781445,
      -73.861581, 40.786224,
      -73.837061, 40.801662,
      -73.771879, 40.798630,
      -73.755856, 40.768024,
      -73.751608, 40.783926,
      -73.701719, 40.752578,
      -73.700263, 40.739427,
      -73.707789, 40.728022,
      -73.730123, 40.722135,
      -73.725025, 40.652367,
      -73.743839, 40.643618,
      -73.821883, 40.666261,
      -73.822597, 40.647909,
      -73.853239, 40.643285,
      -73.869289, 40.694845,
      -73.896493, 40.682682,
      -73.922375, 40.709145,
      -73.928582, 40.728072,
      -73.967564, 40.736458
    ), ncol = 2, byrow = TRUE)
  }
  
  if (borough == "Bronx") {
    polygon_coords <- matrix(c(
      -73.910848, 40.915267,
      -73.819450, 40.888649,
      -73.790025, 40.804987,
      -73.912296, 40.796159,
      -73.928312, 40.802131,
      -73.933776, 40.810322,
      -73.934095, 40.834624,
      -73.909793, 40.872164,
      -73.925643, 40.878535,
      -73.910848, 40.915267
    ), ncol = 2, byrow = TRUE)
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
           seqnum=j,
           borough=borough)
  
  locations<-bind_rows(locations, location_single)
}

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
