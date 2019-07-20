library(data.table)
library(dygraphs)
library(furrr)
library(future)
library(ggplot2)
library(lubridate)
library(purrr)
library(magrittr)

# Read in files in parallel
future::plan(multiprocess)

# Read in price data in parallel
csv_files <- list.files("data/prices/oil_crisis/", recursive = TRUE)
oil_crisis <- csv_files %>% 
  furrr::future_map_dfr(
    ~ fread(paste0("data/prices/oil_crisis/", .x)),
    .progress = TRUE
  )

# Read in stations data and extract top 5 brands
stations <- fread("data/stations/stations.csv")
stations[, brand := toupper(brand)]
top_5_brands <- stations[, .(count = .N), by=brand][order(-count)][1:5, brand]
top_5_brand_stores <- stations[
  brand %in% top_5_brands, 
  .(uuid, brand, post_code, city, latitude, longitude)
]

# Price data for only stores in top 5 brands
DT <- oil_crisis[station_uuid %in% top_5_brand_stores[, uuid]]
setindexv(DT, c("station_uuid", "date"))
rm(oil_crisis)
gc()


# Daily Data --------------------------------------------------------------

# Calculate average price per station and day
DT[, date := as.Date(date)]
daily <- DT[, .(avg_diesel = round(mean(diesel, na.rm = TRUE), 3),
                avg_e5     = round(mean(e5, na.rm = TRUE), 3),
                avg_e10    = round(mean(e10, na.rm = TRUE), 3)),
            by = .(station_uuid, date)]


# Merge daily data with store information
daily <- merge(daily, top_5_brand_stores, all.x = TRUE, 
               by.x = "station_uuid", by.y = "uuid")

# Calculate average price per brand and month
daily_brand <- daily[, .(avg_diesel = round(mean(avg_diesel, na.rm = TRUE), 3),
                         avg_e5     = round(mean(avg_e5, na.rm = TRUE), 3),
                         avg_e10    = round(mean(avg_e10, na.rm = TRUE), 3)),
                     by = .(brand, date)]

# Monthly Data ------------------------------------------------------------

# Floor dates to month
DT[, month := lubridate::floor_date(date, "month")]

# Calculate average price per station and month
monthly <- DT[, .(avg_diesel = round(mean(diesel, na.rm = TRUE), 3),
                  avg_e5     = round(mean(e5, na.rm = TRUE), 3),
                  avg_e10    = round(mean(e10, na.rm = TRUE), 3)),
              by = .(station_uuid, month)]

monthly <- merge(monthly, top_5_brand_stores, all.x = TRUE, 
                 by.x = "station_uuid", by.y = "uuid")


# Calculate average price per brand and month
monthly_brand <- monthly[, .(avg_diesel = round(mean(avg_diesel, na.rm = TRUE), 3),
                             avg_e5     = round(mean(avg_e5, na.rm = TRUE), 3),
                             avg_e10    = round(mean(avg_e10, na.rm = TRUE), 3)),
                         by = .(brand, month)]


# Visualize ---------------------------------------------------------------

# Write out results of script
fwrite("data/prices/daily_brand_2014.csv")
daily_brand[, .(date, brand, avg_diesel)] %>% 
  tidyr::spread(brand, avg_diesel) %>% 
  timetk::tk_xts(silent = TRUE) %>% 
  dygraph(main = "Average Daily Price for Diesel", ylab = "Average Price (€)") 


