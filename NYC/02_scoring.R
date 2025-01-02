library(dplyr)
library(tidyverse)
library(readr)
library(janitor)
library(googlesheets4)
library(stringr)
library(geosphere)

output_location<-"C:/Users/jwest/Documents/geoguessr_cities/NYC/output/"
round_no<-1

answers<-read_csv(paste0(output_location,"Round ",round_no,"/locations.csv"))

guesses <- read_sheet("https://docs.google.com/spreadsheets/d/1V-mcfBj6rsc7EGLMdp2UXJCPZ22lUruc1g8JlZjil4s/edit?usp=sharing", sheet=paste0("Sheet",round_no))%>%
  clean_names()%>%
  pivot_longer(cols = starts_with("guess_"), 
               names_to = "seqnum", 
               values_to = "link")%>%
  mutate(seqnum=as.numeric(str_replace(seqnum,"guess_","")))

guesses_processed<-guesses%>%
  filter(!is.na(link))%>%
  mutate(location_guess = sub(".*!8m2!3d", "", link))%>%
  mutate(location_guess = sub("!16.*", "", location_guess))%>%
  mutate(location_guess = sub("!5m1.*", "", location_guess))%>%
  mutate(location_guess=str_replace(location_guess,"!4d",","))%>%
  separate(location_guess, into = c("latitude_guess", "longitude_guess"), sep = ",", convert = TRUE)

score_by_location<-answers%>%
  right_join(guesses_processed,by=c("seqnum"))%>%
  mutate(distance = distHaversine(cbind(longitude, latitude), cbind(longitude_guess, latitude_guess)) * 3.28084)%>%
  mutate(score=round(5000 * exp(-10 * distance / 73000),0))

score_by_team<-score_by_location%>%
  group_by(team_name)%>%
  summarise(score=sum(score,na.rm=T))

write_csv(score_by_location, paste0(output_location,"Round ",round_no,"/results by location.csv"))
write_csv(score_by_team, paste0(output_location,"Round ",round_no,"/results by team.csv"))
