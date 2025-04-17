library(dplyr)
library(reshape)
library(zoo)
library(stringi)
library(stringr)
library(lubridate)

data <- NULL
for (i in 2000:2024) {
  filerdata <- sprintf("br-asos-rdata/asos%d.rdata", i)
  load(filerdata)
  asos <- asos |> select(station, date = station_date, time = station_hour)
  data <- rbind(data, asos)
}

load("airports/br-airports.rdata")
stations <- br_airports |> select(station = stid, station_name) |> arrange(station_name, station)

dates <- data |> distinct(date, time) |> arrange(date, time)

load("airports/airport_status.rdata")

airport_status$delay_perc <- airport_status$delays/airport_status$flights

data_city_flights <- dates
data_city_delays <- dates
data_city_delay_perc <- dates

for (i in 1:nrow(stations)) {
  station_id <- stations$station[i]
  station_name <- gsub(" ", "_", stations$station_name[i])
  station_name <- gsub("-", "_", station_name)
  station_name <- gsub("/", "_", station_name)
  station_name <- gsub("\\.", "", station_name)
  station_name <- stringi::stri_trans_general(station_name, "Latin-ASCII")
  station_name <- str_to_title(station_name)
  station_name <- sprintf("%s_%s", station_name, station_id)
  print(station_name)
  
  datafilter <- airport_status |> filter(station == station_id) |> select (date, time, flights, delays, delay_perc) 
  if (nrow(datafilter) >= 365) {
    datafilter <- merge(x = dates, y = datafilter, by.x = c("date", "time"), by.y = c("date", "time"), all.x = TRUE) |> arrange(date, time)
    data_city_flights[,station_name] <- datafilter$flights
    data_city_delays[,station_name] <- datafilter$delays
    data_city_delay_perc[,station_name] <- datafilter$delay_perc
  }
}

fill_attribute <- function(data_city) {
  for (i in 3:ncol(data_city)) {
    NonNAindex <- which(!is.na(data_city[,i]))
    if (length(NonNAindex) > 0) {
      firstNonNA <- min(NonNAindex)
      lastNonNA <- max(NonNAindex)
      data_city[1,i] <- data_city[firstNonNA,i]
      data_city[nrow(data_city),i] <- data_city[lastNonNA,i]
      data_city[,i] <- na.approx(data_city[,i])
    }
  }
  return(data_city)
}

data_city <- data_city_flights
save(data_city, file="asos-airports/br-flights-raw.rdata")
data_city <- fill_attribute(data_city)
save(data_city, file="asos-airports/br-flights.rdata")

data_city <- data_city_delays
save(data_city, file="asos-airports/br-delays-raw.rdata")
data_city <- fill_attribute(data_city)
save(data_city, file="asos-airports/br-delays.rdata")

data_city <- data_city_delay_perc
save(data_city, file="asos-airports/br-delay_perc-raw.rdata")
data_city <- fill_attribute(data_city)
save(data_city, file="asos-airports/br-delay_perc.rdata")
