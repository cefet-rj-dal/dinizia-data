library(dplyr)
library(reshape)
library(zoo)
library(stringi)
library(stringr)

asos_airports <- function(airports, asos) {
  data <- merge(airports, asos, by.x = "stid", by.y = "station") |> arrange(stid, station_date, station_hour)
  colnames(data)[c(1,3,4)] <- c("station", "date", "time")
  data$lon <- NULL
  data$lat <- NULL
  data$elevation <- NULL
  data$valid <- NULL
  return(data)
}

load("airports/br-airports.rdata")
airports <- br_airports |> select(stid, station_name) |> arrange(station_name)

fil <- list.files("br-asos-rdata")
data <- NULL
for (f in fil) {
  filerdata <- sprintf("br-asos-rdata/%s", f)
  print(filerdata)
  load(filerdata)
  asos <- asos_airports(airports, asos)
  data <- rbind(data, asos)
}

for (i in 1:nrow(airports)) {
  station_id <- airports$stid[i]
  station_name <- gsub(" ", "_", airports$station_name[i])
  station_name <- gsub("-", "_", station_name)
  station_name <- gsub("/", "_", station_name)
  station_name <- gsub("\\.", "", station_name)
  station_name <- stringi::stri_trans_general(station_name, "Latin-ASCII")
  station_name <- str_to_title(station_name)
  station_name <- sprintf("%s_%s", station_name, station_id)
  
  airport <- data |> filter(station == station_id) |> arrange(date, time)
  airport$station <- NULL
  airport$station_name <- NULL
  
  filename <- sprintf("br-airports/%s.rdata", station_name)
  
  if (nrow(airport) >= 365) {
    save(airport, file=filename)
  }
  else 
    file.remove(filename)
  
  
  print(station_name)
}

