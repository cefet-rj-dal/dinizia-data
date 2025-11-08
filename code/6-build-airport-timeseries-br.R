library(dplyr)
library(reshape)
library(zoo)
library(stringi)
library(stringr)

# Purpose: Build per-airport time series (BR) from merged ASOS data.
# Inputs:
#   - data/intermediate/airports/br-airports.rdata
#   - data/intermediate/asos/br/*.rdata
# Outputs:
#   - data/final/airports/br/<Station_Name_ID>.rdata (object: airport)

ensure_dir_for <- function(path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
}

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

airports_rdata_path <- "data/intermediate/airports/br-airports.rdata"
load(airports_rdata_path)
airports <- br_airports |> select(stid, station_name) |> arrange(station_name)

asos_intermediate_dir <- "data/intermediate/asos/br"
input_files <- list.files(asos_intermediate_dir)
merged_data <- NULL
for (file_name in input_files) {
  rdata_path <- file.path(asos_intermediate_dir, file_name)
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

  filename <- file.path("data/final/airports/br", sprintf("%s.rdata", station_name))
  ensure_dir_for(filename)

  if (nrow(airport) >= 365) {
    save(airport, file = filename)
  } else {
    file.remove(filename)
  }

  print(station_name)
}

