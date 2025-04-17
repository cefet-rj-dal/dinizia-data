library(dplyr)
library(reshape)
library(zoo)
library(stringi)
library(stringr)
library(lubridate)

asos_airports <- function(airports, asos) {
  data <- merge(airports, asos, by.x = "stid", by.y = "station") |> arrange(station_date, station_hour, stid)
  names(data)[names(data) == "stid"] <- "station"
  names(data)[names(data) == "station_date"] <- "date"
  names(data)[names(data) == "station_hour"] <- "time"
  
  data <- as_tibble(data)
  
  return(data)
}

process_attribute <- function(stations, dates, data, attribute) {
  names(data)[names(data) == attribute] <- "value"
  data <- data |> select(station, station_name, date, time, value)
  data_city <- dates
  
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
    
    datafilter <- data |> filter(station == station_id) |> select (date, time, value) |> arrange(date, time)
    datafilter <- as_tibble(datafilter)
    if (nrow(datafilter) >= 365) {
      datafilter <- merge(x = dates, y = datafilter, by.x = c("date", "time"), by.y = c("date", "time"), all.x = TRUE) |> arrange(date, time)
      data_city[,station_name] <- datafilter$value
    }
  }
  save(data_city, file=sprintf("asos-airports/br-%s-raw.rdata", attribute))
  
  return(data_city)
}

fill_attribute <- function(data_city,attribute) {
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
  
  save(data_city, file=sprintf("asos-airports/br-%s.rdata", attribute))  
  
  return(data_city)
}

load("airports/br-airports.rdata")
br_airports <- br_airports |> select(stid, station_name)

fil <- list.files("br-asos-rdata")
data <- NULL
for (f in fil) {
  filerdata <- sprintf("br-asos-rdata/%s", f)
  print(filerdata)
  load(filerdata)
  asos <- asos_airports(br_airports, asos)
  data <- rbind(data, asos)
}

dates <- data |> distinct(date, time) |> arrange(date, time)
stations <- (data |> distinct(station, station_name) |> arrange(station_name))

data_city <- process_attribute(stations, dates, data, "air_temperature")
data_city <- fill_attribute(data_city, "air_temperature")

data_city <- process_attribute(stations, dates, data, "dew_point")
data_city <- fill_attribute(data_city, "dew_point")

data_city <- process_attribute(stations, dates, data, "relative_humidity")
data_city <- fill_attribute(data_city, "relative_humidity")

data_city <- process_attribute(stations, dates, data, "wind_direction")
data_city <- fill_attribute(data_city, "wind_direction")

data_city <- process_attribute(stations, dates, data, "wind_speed")
data_city <- fill_attribute(data_city, "wind_speed")

data_city <- process_attribute(stations, dates, data, "pressure")
data_city <- fill_attribute(data_city, "pressure")

data_city <- process_attribute(stations, dates, data, "visibility")
data_city <- fill_attribute(data_city, "visibility")

data_city <- process_attribute(stations, dates, data, "apparent_temperature")
data_city <- fill_attribute(data_city, "apparent_temperature")


