library(dplyr)
library(tidyverse)
library(readr)
library(janitor)
library(googlesheets4)
library(stringr)
library(geosphere)

output_location<-"C:/Users/jwest/github/geoguessr_cities/DC/output/"
round_no<-3

answers<-read_csv(paste0(output_location,"Round ",round_no,"/locations.csv"))

guesses <- read_sheet("https://docs.google.com/spreadsheets/d/1FnSqet68G3hWA0wMUM0Ti-W-PRbS-DMRt4g8zYX5t5s/edit?usp=sharing", sheet=paste0("Sheet",round_no))%>%
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
  mutate(score=round(5000 * exp(-10 * distance / 73000),0))

score_by_team<-score_by_location%>%
  group_by(team_name)%>%
  summarise(score=sum(score,na.rm=T))

write_csv(score_by_location, paste0(output_location,"Round ",round_no,"/results by location.csv"))
write_csv(score_by_team, paste0(output_location,"Round ",round_no,"/results by team.csv"))
