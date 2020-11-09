

#install.packages("devtools")
install.packages("mapview")
library(devtools)

devtools::install_github("PublicHealthDataGeek/CycleInfraLnd")

library(CycleInfraLnd)
library(dplyr)
library(sf)
library(leaflet)
library(tmap) #thematic mapping package
library(mapview)
library(tidyverse)
library(here)
here::here()


CLT <- get_cid_lines(type = "cycle_lane_track")
IMD <- st_read(here::here("proj_data", "IMD_2019","IMD_2019.shp"))
#depidx <- read_csv("proj_data/indicesofdeprivation.csv")

head(IMD)

# create new object that selects all cycle lanes 
# and tracks that allow segrgegated lanes
segreg_cl = CLT %>%
  filter(CLT_SEGREG == TRUE | CLT_STEPP == TRUE |
         CLT_PARSEG == TRUE)

imd_map <- IMD %>%
  filter(str_detect(LADcd, "^E09"))

# Looking at Islington segregated lanes 
# to check their list

# islington_segreg = segreg_cl %>%
#  filter(BOROUGH == "Islington")

# mapview(islington_segreg, legend = FALSE, map.types = "OpenStreetMap", color = 'red')
# islington_OSM = mapview(islington_segreg, legend = FALSE, map.types = "OpenStreetMap", color = 'red')
# mapshot(islington_OSM)

# mapview(islington_segreg)
#  islingtonBW = mapview(islington_segreg, legend = FALSE, color = 'red')


mapview(segreg_cl)
segregBW = mapview(segreg_cl, legend = FALSE, color = 'red')

tmap_mode("plot")
qtm(imd_map,
    fill = "IMDScore")


'''''''''''''''''''''''''''''''''''''''''''''''''''''''''
# Adding feature id to map
mapview(sutton_contra, legend = FALSE, map.types = "OpenStreetMap", color = 'red')
l1 = addStaticLabels(sutton_OSM, label = sutton_contra$FEATURE_ID)
l1 # show L1

# Examining the Sutton data of 26 observations that only cover 11 roads
Sutton_camdenroad = sutton_contra %>%
  filter(str_detect(FEATURE_ID, "RWG12411"))

mapview(Sutton_camdenroad)
photo_camden_road = Sutton_camdenroad$PHOTO1_URL
browseURL(Sutton_camdenroad$PHOTO1_URL[1])

# get stats19 data nearby -------------------------------------------------

sf::st_crs(Sutton_camdenroad)
buffer_size = 30
Sutton_camdenroad_buffer = stplanr::geo_projected(Sutton_camdenroad, st_buffer, dist = buffer_size)
mapview(Sutton_camdenroad_buffer)
buffer_union = st_union(Sutton_camdenroad_buffer)
mapview(buffer_union)

y = 2013:2018
a = purrr::map_dfr(y, stats19::get_stats19, type = "accidents")
casualties = purrr::map_dfr(y, stats19::get_stats19, type = "cas")
cas_cycling = casualties %>% filter(casualty_type == "Cyclist")
a_cyclist = a %>% filter(accident_index %in% cas_cycling$accident_index)
a = left_join(a_cyclist, cas_cycling)
a_sf = stats19::format_sf(a, lonlat = TRUE)
a_buffer = a_sf[buffer_union, ]
mapview(a_buffer) + mapview(buffer_union)

Sutton_buffer = stplanr::geo_projected(sutton_contra, st_buffer, dist = buffer_size)
buffer_union = st_union(Sutton_buffer)
a_buffer = a_sf[buffer_union, ]
mapview(a_buffer) + mapview(buffer_union)


# join on cid data to crashes ---------------------------------------------

a_buffer

nearest_features = st_nearest_feature(x = a_buffer, y = sutton_contra)
a_buffer$segregated = sutton_contra$CLT_SEGREG[nearest_features]

tmap_mode("view")
tm_shape(a_buffer) + tm_dots("segregated")
mapview(sutton_contra[23, ])
browseURL(sutton_contra$PHOTO1_URL[23])