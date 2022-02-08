library(tidyverse)
library(lubridate)
library(sf)
library(mapview)
library(here)

# massachusetts counties
massco <- st_read('https://earthworks.stanford.edu/download/file/tufts-macounties10-geojson.json')

# tmp <- read.csv(url('https://data.cocorahs.org/export/exportreports.aspx?ReportType=Daily&Format=csv&State=MA&ReportDateType=reportdate&StartDate=1/1/2021&EndDate=12/31/2021'))

# get all stations in massachusetts
cocometarw <- read.csv(url('https://data.cocorahs.org/export/exportstations.aspx?Format=csv&state=ma'))

cocometa <- cocometarw %>% 
  dplyr::filter(Latitude > 35) %>% 
  dplyr::filter(grepl('Reporting', StationStatus)) %>% 
  st_as_sf(coords = c('Longitude', 'Latitude'), crs = 4326) %>% 
  mutate(
    CreationDate = ymd_hm(CreationDate, tz = 'EST'),
    DateTimeStamp = ymd_hm(DateTimeStamp, tz = 'EST')
  )

rngs <- st_coordinates(cocometa) %>% 
  apply(2, range)

# arbitrary location
loc <- tibble(
  lat = runif(1, min = rngs[1, 2], max = rngs[2, 2]), #42.53284, 
  lng = runif(1, min = rngs[1, 1], max = rngs[2, 1]), #-72.17468
  ) %>% 
  st_as_sf(coords = c('lng', 'lat'), crs = 4326)


# find nearest cocorahs station
nearpt <- loc %>% 
  st_nearest_feature(cocometa) %>% 
  cocometa[., ] %>% 
  mutate(
    dist = st_distance(., loc),
    dist = units::set_units(dist, 'km')
    )

mapview(cocometa) + mapview(loc, col.regions = 'red') + mapview(nearpt, col.regions = 'green')

# get data for the nearest station
# build query for url

