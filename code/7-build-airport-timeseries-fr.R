library(dplyr)
library(reshape)
library(zoo)
library(stringi)
library(stringr)

# Purpose: Build per-airport time series (FR) from merged ASOS data.
# Inputs:
#   - airports/fr-airports.rdata (object: fr_airports)
#   - fr-asos-rdata/*.rdata (object: asos)
# Outputs:
#   - fr-airports/<Station_Name_ID>.rdata (object: airport)

asos_airports <- function(airports, asos) {
  asos <- asos |> filter(!is.na(air_temperature))
  merged <- merge(airports, asos, by.x = "stid", by.y = "station") |>
    arrange(stid, station_date, station_hour)
  colnames(merged)[c(1, 3, 4)] <- c("station", "date", "time")
  merged$lon <- NULL
  merged$lat <- NULL
  merged$elevation <- NULL
  merged$valid <- NULL
  return(merged)
}

load("airports/fr-airports.rdata")
airports <- fr_airports |> select(stid, station_name) |> arrange(station_name)

input_files <- list.files("fr-asos-rdata")
merged_data <- NULL
for (file_name in input_files) {
  rdata_path <- sprintf("fr-asos-rdata/%s", file_name)
  print(rdata_path)
  load(rdata_path)
  asos <- asos_airports(airports, asos)
  merged_data <- rbind(merged_data, asos)
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

  airport <- merged_data |> filter(station == station_id) |> arrange(date, time)
  airport$station <- NULL
  airport$station_name <- NULL

  filename <- sprintf("fr-airports/%s.rdata", station_name)

  if (nrow(airport) >= 365) {
    save(airport, file = filename)
  } else {
    file.remove(filename)
  }

  print(station_name)
}


