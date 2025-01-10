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
      "source=outdoor", "&",
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

if (pano=="on"){
  for (i in 1:nrow(locations)){
  
    file_paths <- c(paste0(output_location,"Round ",round_no,"/location_",i,"_image_1.jpg"),
                    paste0(output_location,"Round ",round_no,"/location_",i,"_image_2.jpg"),
                    paste0(output_location,"Round ",round_no,"/location_",i,"_image_3.jpg"),
                    paste0(output_location,"Round ",round_no,"/location_",i,"_image_4.jpg"),
                    paste0(output_location,"Round ",round_no,"/location_",i,"_image_5.jpg"))
  
    images <- lapply(file_paths, image_read)
  
    stitched_image <- image_append(image_join(images), stack = FALSE)
  
    image_write(stitched_image, paste0(output_location,"Round ",round_no,"/location_",i,"_pano.jpg"))
  }
}