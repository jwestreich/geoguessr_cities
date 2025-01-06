if (city=="DC"){
  max_dist<-73000  
}
if (city=="NYC"){
  max_dist<-130000  
}
  
answers<-read_csv(paste0(output_location,"Round ",round_no,"/locations.csv"))

guesses <- read_sheet(google_sheet_link, sheet=paste0("Sheet",round_no))%>%
  clean_names()%>%
  pivot_longer(cols = starts_with("guess_"), 
               names_to = "seqnum", 
               values_to = "link")%>%
  mutate(seqnum=as.numeric(str_replace(seqnum,"guess_","")))

guesses_processed<-guesses%>%
  filter(!is.na(link))%>%
  separate(link, into = c("latitude_guess", "longitude_guess"), sep = ",", convert = TRUE)

score_by_location<-answers%>%
  right_join(guesses_processed,by=c("seqnum"))%>%
  mutate(distance = distHaversine(cbind(longitude, latitude), cbind(longitude_guess, latitude_guess)) * 3.28084)%>%
  mutate(score=round(5000 * exp(-10 * distance / max_dist),0))

score_by_team<-score_by_location%>%
  group_by(team_name)%>%
  summarise(score=sum(score,na.rm=T))

write_csv(score_by_location, paste0(output_location,"Round ",round_no,"/results by location.csv"))
write_csv(score_by_team, paste0(output_location,"Round ",round_no,"/results by team.csv"))
