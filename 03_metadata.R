locations <- locations_draft %>%
  rowwise() %>%
  mutate(valid = {
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
  }) %>%
  ungroup()%>%
  filter(valid==TRUE)%>%
  mutate(seqnum=row_number())%>%
  filter(seqnum<=5)

if (nrow(locations) < 5) {
  stop("Less than 5 valid locations found")
}


write_csv(locations, paste0(output_location,"Round ",round_no,"/locations.csv"))

