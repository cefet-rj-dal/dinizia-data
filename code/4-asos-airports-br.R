library(dplyr)
library(reshape)
library(zoo)
library(stringi)
library(stringr)

asos_airports <- function(airports, asos) {
  asos <- asos |> dplyr::select(station = station, date = station_date, time = station_hour, air_temperature)
  
  data <- merge(airports, asos, by.x = "stid", by.y = "station") |> select(station = stid, station_name, date, time, air_temperature) |> arrange(date, time, station)
  
  return(data)
}

load("airports/br-airports.rdata")
br_airports <- br_airports |> select(stid, station_name)

fil <- list.files("br-asos-rdata")
data <- NULL
for (f in fil) {
  filerdata <- sprintf("br-asos-rdata/%s", f)
  print(filerdata)
  load(filerdata)
  asos <- as_tibble(asos)
  asos <- asos_airports(br_airports, asos)
  data <- rbind(data, asos)
}
save(data, file="data.rdata")

dates <- data |> distinct(date, time) |> arrange(date, time)
stations <- (data |> distinct(station, station_name) |> arrange(station_name))
data_city <- dates

for (i in 1:nrow(stations)) {
  station_id <- stations$station[i]
  station_name <- gsub(" ", "_", stations$station_name[i])
  station_name <- gsub("-", "_", station_name)
  station_name <- gsub("/", "_", station_name)
  station_name <- gsub(".", "", station_name)
  station_name <- stringi::stri_trans_general(station_name, "Latin-ASCII")
  station_name <- str_to_title(station_name)
  station_name <- sprintf("%s_%s", station_name, station_id)
  
  datafilter <- data |> filter(station == station_id) |> select(date, time, air_temperature) |> arrange(date, time)
  datafilter <- merge(x = dates, y = datafilter, by.x = c("date", "time"), by.y = c("date", "time"), all.x = TRUE) |> arrange(date, time)
  data_city[,station_name] <- datafilter$air_temperature
  print(station_name)
}

save(data_city, file="asos-airports/br-data.rdata")

for (i in 3:ncol(data_city)) {
  x <- data_city[,i]
  NonNAindex <- which(!is.na(x))
  firstNonNA <- min(NonNAindex)
  lastNonNA <- max(NonNAindex)
  data_city[1,i] <- x[firstNonNA]
  data_city[nrow(data_city),i] <- x[lastNonNA]
  data_city[,i] <- na.approx(data_city[,i])
}

save(data_city, file="asos-airports/br-data-inter.rdata")

