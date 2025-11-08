library(dplyr)
library(reshape)
library(zoo)
library(stringi)
library(stringr)
library(lubridate)

# Purpose: Build BR-wide airport delay/flight matrices aligned to ASOS timeline.
# Inputs:
#   - data/intermediate/asos/br/asos<year>.rdata
#   - data/intermediate/airports/br-airports.rdata
#   - data/source/airports/airport_status.rdata
# Outputs:
#   - data/final/asos-airports/br-{flights|delays|delay_perc}-raw.rdata
#   - data/final/asos-airports/br-{flights|delays|delay_perc}.rdata (interpolated)

ensure_dir_for <- function(path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
}

# Build a reference timeline from all ASOS yearly indices
asos_intermediate_dir <- "data/intermediate/asos/br"
asos_index <- NULL
for (i in 2000:2024) {
  asos_path <- file.path(asos_intermediate_dir, sprintf("asos%d.rdata", i))
  load(asos_path)
  asos <- asos |> select(station, date = station_date, time = station_hour)
  asos_index <- rbind(asos_index, asos)
}

airports_rdata_path <- "data/intermediate/airports/br-airports.rdata"
load(airports_rdata_path)
stations_df <- br_airports |> select(station = stid, station_name) |> arrange(station_name, station)

timestamps_df <- asos_index |> distinct(date, time) |> arrange(date, time)

airport_status_path <- "data/source/airports/airport_status.rdata"
load(airport_status_path)

airport_status$delay_perc <- airport_status$delays / airport_status$flights

city_data_flights <- timestamps_df
city_data_delays <- timestamps_df
city_data_delay_perc <- timestamps_df

for (i in 1:nrow(stations_df)) {
  station_id <- stations_df$station[i]
  station_name <- gsub(" ", "_", stations_df$station_name[i])
  station_name <- gsub("-", "_", station_name)
  station_name <- gsub("/", "_", station_name)
  station_name <- gsub("\\.", "", station_name)
  station_name <- stringi::stri_trans_general(station_name, "Latin-ASCII")
  station_name <- str_to_title(station_name)
  station_name <- sprintf("%s_%s", station_name, station_id)
  print(station_name)

  filtered_data <- airport_status |>
    filter(station == station_id) |>
    select(date, time, flights, delays, delay_perc)
  if (nrow(filtered_data) >= 365) {
    filtered_data <- merge(
      x = timestamps_df, y = filtered_data,
      by.x = c("date", "time"), by.y = c("date", "time"), all.x = TRUE
    ) |>
      arrange(date, time)
    city_data_flights[, station_name] <- filtered_data$flights
    city_data_delays[, station_name] <- filtered_data$delays
    city_data_delay_perc[, station_name] <- filtered_data$delay_perc
  }
}

fill_attribute <- function(city_data) {
  for (i in 3:ncol(city_data)) {
    NonNAindex <- which(!is.na(city_data[, i]))
    if (length(NonNAindex) > 0) {
      firstNonNA <- min(NonNAindex)
      lastNonNA <- max(NonNAindex)
      city_data[1, i] <- city_data[firstNonNA, i]
      city_data[nrow(city_data), i] <- city_data[lastNonNA, i]
      city_data[, i] <- na.approx(city_data[, i])
    }
  }
  return(city_data)
}

asos_final_dir <- "data/final/asos-airports"
city_data <- city_data_flights
out_raw <- file.path(asos_final_dir, "br-flights-raw.rdata")
ensure_dir_for(out_raw)
save(city_data, file = out_raw)
city_data <- fill_attribute(city_data)
save(city_data, file = file.path(asos_final_dir, "br-flights.rdata"))

city_data <- city_data_delays
out_raw <- file.path(asos_final_dir, "br-delays-raw.rdata")
ensure_dir_for(out_raw)
save(city_data, file = out_raw)
city_data <- fill_attribute(city_data)
save(city_data, file = file.path(asos_final_dir, "br-delays.rdata"))

city_data <- city_data_delay_perc
out_raw <- file.path(asos_final_dir, "br-delay_perc-raw.rdata")
ensure_dir_for(out_raw)
save(city_data, file = out_raw)
city_data <- fill_attribute(city_data)
save(city_data, file = file.path(asos_final_dir, "br-delay_perc.rdata"))
