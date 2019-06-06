library(data.table)
library(DT)
library(magrittr)
library(sf)
library(shiny)
library(tmap)

# Import tankstation data and make brands toupper()
stations <- fread("data/stations/stations.csv")
stations[, brand := toupper(brand)]

# Filter out bad coords and add 2-digit zip code
stations <- stations[longitude > 0 & latitude > 0]
stations[, plz := substring(post_code, 1, 2)]

# Extract Top 5 Brands
top_5 <- stations[, .(count = .N), by=brand][order(-count)][1:5, brand]

station_map <- stations[brand %in% top_5] %>%
    sf::st_as_sf(coords = c("longitude", "latitude"), crs=4326) %>% 
    sf::st_transform(crs=4326)

tmap_mode("view")

station_map %>% 
    tm_shape() + 
    tm_dots("plz", style="cont") +
    tm_view(basemaps = "OpenStreetMap")



