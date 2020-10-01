library(tidyverse)
library(sf)
library(lubridate)
library(leaflet)
library(tmap)

dane <- read_csv("https://data.humdata.org/dataset/96267a9e-c628-4a54-8aa5-e17d022c2573/resource/185fae7c-ce67-4b11-ab80-8e31d8760de3/download/conflict_data_blr.csv")

BY <- dane %>%
  slice(-1)%>%
  mutate(across(c(latitude, longitude, fatalities), as.numeric),
         #across(c(longitude, latitude), ~jitter(.,10)),
         event_date = ymd(event_date))%>%
  filter(event_date>"2020-07-14",
         event_type %in% c("Protests", "Riots"))%>%
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
                            liczba == "tens" ~ "50",
                            liczba == "several hundred" ~ "500",
                            liczba == "several tens" ~ "50",
                            liczba == "dozen" ~ "12",
                            liczba == "several dozen" ~ "100",
                            liczba == "hundreds" ~ "500",
                            liczba == "hundred" ~ "100",
                            liczba == "several" ~ "10",
                            liczba == "two dozen" ~ "25",
                            liczba == "several thousand" ~ "5000",
                            liczba == "several thousands" ~ "5000",
                            liczba == "tens of thousands" ~ "50000",
                            liczba == "ten thousand" ~ "10000",
                            liczba == "thousands" ~ "5000",
                            liczba == "thousand" ~ "1000",
                            TRUE ~ liczba),
         liczba = gsub(" ", "", liczba),
         liczba = gsub(",", "", liczba),
         liczba = as.numeric(liczba)) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)%>%
  st_jitter(geometry, amount = 0.005)

BY <- BY %>%
mutate(latitude = unlist(map(BY$geometry,2)),
       longitude = unlist(map(BY$geometry,1)))

st_geometry(BY) <- NULL

save(BY, file="./dane/BY.Rda")


# wizualizacje ------------------------------------------------------------


ggplot(BY, aes(x=event_date, y=liczba))+
  geom_col()

mapa <- BY  %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
  filter(event_date == "2020-08-23")

tmap_mode("view")

tm_basemap(leaflet::providers$CartoDB.Positron)+
  tm_shape(mapa)+
  tm_dots(col="red",alpha=0.5,scale=1.5, # jitter = 0.1, 
          id="location", popup.vars = "notes")

wykres <- BY %>%
  filter(event_date>"2020-07-31",
         grepl("Minsk", location)==F)

library(scales)
library(gridExtra)
p1 <- ggplot(wykres, aes(x=event_date, y=liczba/1e3))+
  geom_col(fill="blue")+
  scale_y_continuous(label=unit_format(unit = "tys."))+
  geom_vline(xintercept=ymd("2020-08-09"), colour="red") +
  annotate("text", x=ymd("2020-08-09"),y=100, angle=90, label="wybory", color="red", hjust = 0, vjust=-0.5)+
  geom_vline(xintercept=ymd("2020-09-23"), colour="red") +
  annotate("text", x=ymd("2020-09-23"),y=60, angle=90, label="inauguracja Łukaszenki", color="red", hjust = 0, vjust=-0.5)+
  labs(x=NULL, 
       y="liczba protestujących",
       caption = "na podstawie danych ACLED")+
  theme_bw()

p2 <- ggplot(wykres, aes(x=event_date))+
  geom_bar(fill="blue")+
  geom_vline(xintercept=ymd("2020-08-09"), colour="red") +
  annotate("text", x=ymd("2020-08-09"),y=20, angle=90, label="wybory", color="red", hjust = 0, vjust=-0.5)+
  geom_vline(xintercept=ymd("2020-09-23"), colour="red") +
  annotate("text", x=ymd("2020-09-23"),y=17, angle=90, label="inauguracja Łukaszenki", color="red", hjust = 0, vjust=-0.5)+
  labs(x=NULL, 
       y="liczba protestów",
       caption = "",
       title = "Liczba protestów i protestujących bez uwzględnienia Mińska")+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))

png(paste0("./wykresy/białoruś.",Sys.Date(), ".png"), units="in", width=9, height=9, res=300)
grid.arrange(p2,p1, ncol=1)
dev.off()

zrodla <- BY %>%
  select(source)%>%
  separate(col = source, into = letters[1:9], sep = ";")%>%
  pivot_longer(cols = c(a:i), names_to = "dupa", values_to= "źródło") %>%
  filter(!is.na(źródło))%>%
  group_by(źródło)%>%
  mutate(źródło = str_trim(źródło, side = "both")) %>%
  summarise(
    źródło = unique(źródło),
    liczba = n()
  )%>%
  arrange(desc(liczba))%>%
  head(15)

png(paste0("./wykresy/białoruś.zrodla.",Sys.Date(), ".png"), units="in", width=7, height=9, res=300)
ggplot(zrodla, aes(x=reorder(źródło, liczba), y=liczba))+
  geom_col(fill="blue")+
  coord_flip()+
  labs(x=NULL,
       y="liczba cytowań",
       title = "Źródła informacji ACLED")+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))
dev.off()

