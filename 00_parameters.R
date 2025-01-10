library(tibble)
library(dplyr)
library(readr)
library(sf)
library(httr)
library(jsonlite)
library(purrr)
library(tidyverse)
library(janitor)
library(googlesheets4)
library(stringr)
library(geosphere)
library(leaflet)
library(magick)

city <- "DC"        #DC or NYC
round_no <- 1
google_sheet_link<-"https://docs.google.com/spreadsheets/d/188wl-XhzJ0fz0TCU9zHMOKmARJes0_Is-VX1b9NZgAs/edit?usp=sharing"
pano<-"off" #on or off to save panorama pictures of locations

manhattan_percent<-.45
brooklyn_percent<-.35
queens_percent<-.15
bronx_percent<-.05

code_base<-paste0("C:/Users/jwest/github/geoguessr_cities/",city)
output_location<-paste0("C:/Users/jwest/github/geoguessr_cities/",city,"/output/")