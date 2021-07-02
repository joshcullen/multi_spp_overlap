
library(tidyverse)
library(lubridate)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)

green<- read.csv("../181790-Argos.csv", as.is = T)
terrapin<- read.csv("../176034-Argos.csv", as.is = T)


# Check and summarize green turtle data
glimpse(green)
summary(green)

# Check and summarize terrapin data
glimpse(terrapin)
summary(terrapin)

# Remove rows with missing coordinates
green<- green %>% 
  drop_na(Longitude)
terrapin<- terrapin %>% 
  drop_na(Longitude)

# Change dates to proper time format
green$Date<- as.POSIXlt(strptime(green$Date, format = "%H:%M:%S %d-%b-%Y", tz = "US/Central"))
terrapin$Date<- as.POSIXlt(strptime(terrapin$Date, format = "%H:%M:%S %d-%b-%Y",
                                    tz = "US/Central"))
tracks<- rbind(green[,c("Ptt","Date","Longitude","Latitude")],
               terrapin[,c("Ptt","Date","Longitude","Latitude")])
tracks$Ptt<- as.character(tracks$Ptt)

# Create sf track objects
green.sf<- st_as_sf(green, coords = c("Longitude","Latitude"), crs = 4326, agr = "constant")
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
  geom_path(data = tracks, aes(Longitude, Latitude, color = Ptt, group = Ptt), alpha = 0.65) +
  scale_color_brewer("ID", palette = "Dark2", labels = c("Terrapin","Green")) +
  coord_sf(xlim = c(-88,-86), ylim = c(30, 31)) +
  theme_bw() +
  theme(panel.grid = element_blank())
# ggsave("Green_Terrapin tracks.png", width = 6, height = 4, units = "in", dpi = 300)


ggplot() +
  geom_sf(data = states) +
  geom_path(data = tracks, aes(Longitude, Latitude, group = Ptt, color = as_date(Date))) +
  scale_color_viridis_c("Date", trans = "date", option = "plasma", direction = -1,
                        labels = as.Date) +
  coord_sf(xlim = c(-88,-86), ylim = c(30, 31)) +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  guides(color = guide_colorbar(reverse = TRUE))
# ggsave("Green_Terrapin tracks time.png", width = 6, height = 4, units = "in", dpi = 300)



table(green$LocationQuality)
table(terrapin$LocationQuality)


# Inspect dt for primary time interval
green2<- bayesmove::prep_data(green %>% 
                                rename(date = Date), c('Longitude', 'Latitude'), 'Ptt')
table(green2$dt)
ggplot(green2) +
  geom_histogram(aes(dt/60), binwidth = 10) +
  theme_bw()
## appears to be 30 min


terrapin2<- bayesmove::prep_data(terrapin %>% 
                                rename(date = Date), c('Longitude', 'Latitude'), 'Ptt')
table(terrapin2$dt)
ggplot(terrapin2) +
  geom_histogram(aes(dt/3600), binwidth = 1) +
  theme_bw()
## not sure

### Try using foieGras to model tracks