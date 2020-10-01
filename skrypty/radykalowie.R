library(tidyverse)
library(sf)
library(lubridate)
library(leaflet)
library(tmap)

dane <- read_csv("https://data.humdata.org/dataset/1999c553-d901-4630-b150-33b20af754c3/resource/9a5a8298-1c63-486c-8ef4-353b336f5932/download/conflict_data_ukr.csv")

save(dane, file="./dane/dane.Rda")

# protesty - organizacje ekstremistyczne ----------------------------------

load("./dane/dane.Rda")

# próba loopu

nazwy <- c("Right Sector", "Democratic Axe", "Svoboda", "C14", "Traditions and Order", "National Corps", "Sokil", "CUN", "Azov", "Freikorps", "Karpatska Sich", "Former Military Forces of Ukraine")

nazi <- dane %>%
  slice(-1)%>%
  mutate(across(c(latitude, longitude, fatalities), as.numeric),
         #across(c(longitude, latitude), ~jitter(.,10)),
         event_date = ymd(event_date),
         nazwa = "całość")%>%
  filter(event_type %in% c("Protests", "Riots")) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)%>%
  st_jitter(geometry, amount = 0.005) %>%
  filter(grepl(paste(nazwy, collapse="|"),assoc_actor_1))%>%
  mutate(liczba = sub(".*\\[([^][]+)].*", "\\1", notes),
         liczba = gsub("size=", "", liczba),
         liczba = gsub("about ", "", liczba),
         liczba = gsub("more than ", "", liczba),
         liczba = gsub("at least ", "", liczba),
         liczba = gsub("a ", "", liczba),
         liczba = gsub("over ", "", liczba), 
         liczba = gsub("around ", "", liczba), 
         liczba = gsub("up to ", "", liczba),
         liczba = sub(".*to ", "", liczba),
         liczba = sub(".*-", "", liczba), 
         liczba = case_when(liczba == "no report" ~ "NA", 
                            liczba == "dozens" ~ "100",
                            liczba == "several hundred" ~ "500",
                            liczba == "dozen" ~ "12",
                            liczba == "several dozen" ~ "100",
                            liczba == "hundreds" ~ "500",
                            liczba == "hundred" ~ "100",
                            liczba == "several" ~ "10",
                            liczba == "two dozen" ~ "25",
                            liczba == "several thousand" ~ "5000",
                            liczba == "ten thousand" ~ "10000",
                            liczba == "thousands" ~ "5000",
                            liczba == "thousand" ~ "1000",
                            TRUE ~ liczba),
         liczba = gsub(" ", "", liczba),
         liczba = gsub(",", "", liczba),
         liczba = as.numeric(liczba))

nazi <- nazi %>%
  mutate(latitude = unlist(map(nazi$geometry,2)),
         longitude = unlist(map(nazi$geometry,1)))

st_geometry(nazi) <- NULL  
         
for(i in nazwy){
  assign(paste(i), filter(nazi, grepl(i,assoc_actor_1)==T)%>%
           mutate(nazwa=i))
}
rm(dane, i, nazwy)

radykalni <- lapply(ls(), get)

radykalni <- bind_rows(radykalni, .id = "id")

save(radykalni, file="./dane/radykalni.Rda")


# LGTB --------------------------------------------------------------------

load("./dane/dane.Rda")

lgbt <- dane %>%
  slice(-1)%>%
  mutate(across(c(latitude, longitude, fatalities), as.numeric),
         #across(c(longitude, latitude), ~jitter(.,10)),
         event_date = ymd(event_date),
         nazwa = "całość")%>%
  filter(event_type %in% c("Protests", "Riots")) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)%>%
  st_jitter(geometry, amount = 0.005)%>%
  filter_at(vars(assoc_actor_1:assoc_actor_2, notes), any_vars(grepl("LGBT", ., ignore.case = T)))

ggplot(lgbt, aes(event_date))+
  geom_histogram(binwidth = 60)

tmap_mode("view")

tm_shape(lgbt)+
  tm_dots(size=1, col = "red", alpha = 0.5)
