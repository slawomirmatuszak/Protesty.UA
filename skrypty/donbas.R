library(tidyverse)
library(sf)
library(lubridate)
library(leaflet)
library(tmap)

dane <- read_csv("https://data.humdata.org/dataset/1999c553-d901-4630-b150-33b20af754c3/resource/9a5a8298-1c63-486c-8ef4-353b336f5932/download/conflict_data_ukr.csv")

load("./dane/dane.Rda")

donbas <- dane %>%
  slice(-1)%>%
  mutate(location = if_else(latitude == "47.8917", "Luhanske1", location),
         across(c(latitude, longitude, fatalities), as.numeric),
         #across(c(longitude, latitude), ~jitter(.,10)),
         event_date = ymd(event_date))%>%
  filter(event_type %in% c("Battles", "Explosions/Remote violence"),
         #event_date > "2020-01-01",
         longitude > 37.2,
         latitude < 49&latitude>47.023878)%>%
  mutate(notes = gsub("((?:[^ ]+ ){3}[^ ]+) ", "\\1\n", notes))%>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)%>%
  st_jitter(geometry, amount = 0.005)
#separate(geometry, into=c("longitude", "latitude"), sep = ",")

donbas <- donbas %>%
  mutate(latitude = unlist(map(donbas$geometry,2)),
         longitude = unlist(map(donbas$geometry,1)))

st_geometry(donbas) <- NULL

save(donbas, file="./dane/donbas.Rda")

mapa <- donbas  %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

tmap_mode("view")

tm_basemap(leaflet::providers$CartoDB.Positron)+
  tm_shape(mapa)+
  tm_dots(col="red",alpha=0.5,scale=1.5,# jitter = 0.1, 
          id="location", popup.vars = "notes")

