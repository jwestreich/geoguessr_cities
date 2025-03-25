if (city=="DC"){
  max_dist<-73000  
}
if (city=="NYC"){
  max_dist<-130000  
}
  
answers<-read_csv(paste0(output_location,"Round ",round_no,"/locations.csv"))

guesses <- read_sheet(google_sheet_link)%>%
  filter(round==round_no)%>%
  group_by(team_name,location)%>%
  filter(datetime==max(datetime))%>%
  ungroup()

score_by_location<-answers%>%
  right_join(guesses,by=c("seqnum"="location"))%>%
  mutate(distance = distHaversine(cbind(longitude, latitude), cbind(longitude_guess, latitude_guess)) * 3.28084)%>%
  mutate(score=5000 * exp(-10 * distance / max_dist))%>%
  mutate(score=ifelse(distance<5280,score*1.02,score))%>%
  mutate(score=ifelse(distance<160,5000,score))%>%
  mutate(score=ifelse(score>5000,5000,score))%>%
  mutate(score=round(score,0))%>%
  mutate(distance_label=ifelse(
    distance<5280,paste0(round(distance,0)," ft"),
                  paste0(round(distance/5280,1)," mi")))

score_by_team<-score_by_location%>%
  group_by(team_name)%>%
  summarise(score=sum(score,na.rm=T))

write_csv(score_by_location, paste0(output_location,"Round ",round_no,"/results by location.csv"))
write_csv(score_by_team, paste0(output_location,"Round ",round_no,"/results by team.csv"))

if (round_no==1){
  scores_cumulative<-score_by_team%>%
    group_by(team_name)%>%
    summarise(round1_score=sum(score,na.rm=T),
              cumulative_score=sum(score,na.rm=T)
    )%>%
    arrange(desc(cumulative_score))
}

if (round_no==2){
  score_round1<-read_csv(paste0(output_location,"Round ","1","/results by team.csv"))%>%
    rename(round1_score=score)
  
  scores_cumulative<-score_by_team%>%
    full_join(score_round1,by=c("team_name"))%>%
    rename(round2_score=score)%>%
    mutate(round1_score=ifelse(is.na(round1_score),0,round1_score),
           round2_score=ifelse(is.na(round2_score),0,round2_score)
    )%>%
    group_by(team_name)%>%
    mutate(cumulative_score=sum(round1_score+round2_score,na.rm=T))%>%
    select(team_name,round1_score,round2_score,cumulative_score)%>%
    arrange(desc(cumulative_score))
}

if (round_no==3){
  score_round1<-read_csv(paste0(output_location,"Round ","1","/results by team.csv"))
  score_round2<-read_csv(paste0(output_location,"Round ","2","/results by team.csv"))
  
  scores_cumulative<-score_by_team%>%
    rename(round3_score=score)%>%
    full_join(score_round1,by=c("team_name"))%>%
    rename(round1_score=score)%>%
    full_join(score_round2,by=c("team_name"))%>%
    rename(round2_score=score)%>%
    mutate(round1_score=ifelse(is.na(round1_score),0,round1_score),
           round2_score=ifelse(is.na(round2_score),0,round2_score),
           round3_score=ifelse(is.na(round3_score),0,round3_score)
    )%>%
    group_by(team_name)%>%
    mutate(cumulative_score=sum(round1_score+round2_score+round3_score,na.rm=T))%>%
    select(team_name,round1_score,round2_score,round3_score,cumulative_score)%>%
    arrange(desc(cumulative_score))
}
