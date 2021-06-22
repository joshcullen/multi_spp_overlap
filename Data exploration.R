
library(tidyverse)
library(lubridate)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)

green<- read.csv("~/Documents/Multi-Species FL Project/181790-Argos.csv", as.is = T)
terrapin<- read.csv("~/Documents/Multi-Species FL Project/176034 Bertha all locations.csv",
                    as.is = T)


# Check and summarize green turtle data
glimpse(green)
summary(green)

# Check and summarize terrapin data
glimpse(terrapin)
summary(terrapin)


# Create sf track objects
green.sf<- st_as_sf(green %>% 
                      drop_na(Longitude),
                    coords = c("Longitude","Latitude"), crs = 4326, agr = "constant")
terrapin.sf<- st_as_sf(terrapin, coords = c("Longitude","Latitude"), crs = 4326, agr = "constant")


## Map the data
states<- ne_states(country = "United States of America", returnclass = "sf")


# pal<- c(wes_palette('Darjeeling1', n = 5),
#         wes_palette('Darjeeling2', n = 2)[2])
# pal<- pal[c(1,4,3,2,5,6)]  #reorder
# 
# bounds<- st_sfc(st_point(c(-83,24.5)),
#                 st_point(c(-80,30.5)),
#                 crs = 4326) %>% 
#   st_transform(crs = 32617)

ggplot() +
  geom_sf(data = states) +
  geom_path(data = green, aes(Longitude, Latitude), color = "navy") +
  geom_path(data = terrapin, aes(Longitude, Latitude), color = "chartreuse") +
  # geom_sf(data = green.sf, color = "navy") +
  coord_sf(xlim = c(-88,-86), ylim = c(30, 31)) +
  theme_bw() +
  theme(panel.grid = element_blank())


ggplot() +
  geom_sf(data = states) +
  geom_path(data = terrapin, aes(Longitude, Latitude)) +
  geom_sf(data = terrapin.sf, color = "chartreuse") +
  coord_sf(xlim = c(-88,-86), ylim = c(30, 31)) +
  theme_bw() +
  theme(panel.grid = element_blank())
