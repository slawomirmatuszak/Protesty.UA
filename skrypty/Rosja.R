library(tidyverse)
library(sf)
library(lubridate)
library(leaflet)
library(tmap)

dane.ru <- read_csv("https://data.humdata.org/dataset/c170291e-adf5-489d-ace7-80f3c603dfb3/resource/92404ea2-bd82-4e0b-a77e-2a238aa560c9/download/conflict_data_rus.csv")


slowa <- list(" ", ",", "2015", "2016","2017","2018","2019","2020", "2009", "1990")

# in REGEX, | also represents "OR"
find.string <- paste(unlist(slowa), collapse = "|")

dane1 <- dane.ru %>%
  slice(-1)%>%
  mutate(across(c(latitude, longitude, fatalities), as.numeric),
         event_date = ymd(event_date))%>%
  filter(event_type %in% c("Protests", "Riots")) %>%
  filter(str_detect(notes, "\\[size")==F)%>%
  mutate(notes2 = gsub(":", ",", notes),
         notes2 = gsub("28 Russian", "", notes2),
         notes2 = gsub("more than 500 000 Dollars", "", notes2),
         notes2 = gsub("Caucasian War (1763-1864)", "", notes2),
         notes2 = sub("^.*?,", "", notes2),
         notes2 = gsub(find.string, "", notes2))%>%
  mutate(liczba = regmatches(notes2, gregexpr("[[:digit:]]+", notes2)))%>%
  mutate(liczba2=liczba)%>%
  unnest_wider(col = liczba2)

ru1 <- dane1 %>%
  mutate(across(34:length(dane1), as.numeric))%>%
  mutate(liczba2 = pmax(...1,...2,...3,...4,...5,...6,...7, na.rm = T))%>%
  select(liczba2, everything())%>%
  select(1:iso3)%>%
  select(2:32, liczba2)%>%
  rename(liczba = liczba2)

ru2 <- dane.ru %>%
  slice(-1)%>%
  mutate(across(c(latitude, longitude, fatalities), as.numeric),
         event_date = ymd(event_date))%>%
  filter(event_type %in% c("Protests", "Riots")) %>%
  filter(str_detect(notes, "\\[size")==T)%>%
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
                            liczba == "dozen" ~ "12",
                            liczba == "several dozen" ~ "100",
                            liczba == "hundreds" ~ "500",
                            liczba == "hundred" ~ "100",
                            liczba == "several" ~ "10",
                            liczba == "two dozen" ~ "25",
                            liczba == "couple dozen" ~ "25",
                            liczba == "several hundred" ~ "500",
                            liczba == "several thousand" ~ "5000",
                            liczba == "several thousands" ~ "5000",
                            liczba == "tens of thousands" ~ "50000",
                            liczba == "ten thousand" ~ "10000",
                            liczba == "thousand fivehundred" ~ "1500",
                            liczba == "thousands" ~ "5000",
                            liczba == "thousand" ~ "1000",
                            TRUE ~ liczba),
         liczba = gsub("between .. and ", "", liczba),
         liczba = gsub("between ... and ", "", liczba),
         liczba = gsub("between .... and ", "", liczba),
         liczba = gsub("between ..... and ", "", liczba),
         liczba = gsub("between ...... and ", "", liczba),
         liczba = gsub("under", "", liczba),
         liczba = gsub("fewer than", "", liczba),
         liczba = gsub("people", "", liczba),
         liczba = gsub(" ", "", liczba),
         liczba = gsub(",", "", liczba),
         liczba = gsub("[.]", "", liczba),
         liczba = str_extract(liczba, '\\b[^=]+$'),
         liczba = regmatches(liczba, gregexpr("[[:digit:]]+", liczba)))%>%
  unnest(liczba)%>%
  mutate(liczba = as.numeric(liczba))

ru <- bind_rows(ru2,ru1) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)%>%
  st_jitter(geometry, amount = 0.005)

ru <- ru %>%
  mutate(latitude = unlist(map(ru$geometry,2)),
         longitude = unlist(map(ru$geometry,1)))

st_geometry(ru) <- NULL

org <- ru %>%
  separate(col = assoc_actor_1, into = letters[1:10], sep = ";", remove = F)%>%
  pivot_longer(cols = c(a:j), names_to = NULL, values_to = "organizator")%>%
  filter(!is.na(organizator))

nazwy <- org %>%
  select(organizator)%>%
  mutate(organizator = str_trim(organizator, side = "both")) %>%
  group_by(organizator)%>%
  summarise(
    organizator = unique(organizator),
    liczba = n()
  )%>%
  arrange(desc(liczba))%>%
  filter(organizator!="Navalny HQ")%>%
  head(11) %>%
  select(organizator)%>%
  pull()

org <- org %>%
  mutate(organizator = str_trim(organizator, side = "both")) %>%
  filter(organizator %in% nazwy)

ru <- ru %>%
  mutate(organizator = "suma")%>%
  bind_rows(org)%>%
  mutate(organizator = gsub("RF: Russia of the Future", "Russia of the Future (Navalny)",organizator),
         organizator = gsub("LDPR: Liberal Democratic Party of Russia", "LDPR",organizator),
         organizator = gsub("KPRF: Communist Party of the Russian Federation", "KPRF",organizator),
         organizator = gsub("Yabloko: Russian United Democratic Party", "Yabloko",organizator))

save(ru, file = "./dane/rosja.Rda")
