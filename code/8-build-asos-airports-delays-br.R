library(dplyr)
library(reshape)
library(zoo)
library(stringi)
library(stringr)
library(lubridate)

# Purpose: Build BR-wide airport delay/flight matrices aligned to ASOS timeline.
# Inputs:
#   - br-asos-rdata/asos<year>.rdata (object: asos)
#   - airports/br-airports.rdata (object: br_airports)
#   - airports/airport_status.rdata (object: airport_status with flights/delays)
# Outputs:
#   - asos-airports/br-{flights|delays|delay_perc}-raw.rdata
#   - asos-airports/br-{flights|delays|delay_perc}.rdata (interpolated)

# Build a reference timeline from all ASOS yearly indices
asos_index <- NULL
for (i in 2000:2024) {
  asos_path <- sprintf("br-asos-rdata/asos%d.rdata", i)
  load(asos_path)
  asos <- asos |> select(station, date = station_date, time = station_hour)
  asos_index <- rbind(asos_index, asos)
}

load("airports/br-airports.rdata")
stations_df <- br_airports |> select(station = stid, station_name) |> arrange(station_name, station)

timestamps_df <- asos_index |> distinct(date, time) |> arrange(date, time)

load("airports/airport_status.rdata")

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

city_data <- city_data_flights
save(city_data, file = "asos-airports/br-flights-raw.rdata")
city_data <- fill_attribute(city_data)
save(city_data, file = "asos-airports/br-flights.rdata")

city_data <- city_data_delays
save(city_data, file = "asos-airports/br-delays-raw.rdata")
city_data <- fill_attribute(city_data)
save(city_data, file = "asos-airports/br-delays.rdata")

city_data <- city_data_delay_perc
save(city_data, file = "asos-airports/br-delay_perc-raw.rdata")
city_data <- fill_attribute(city_data)
save(city_data, file = "asos-airports/br-delay_perc.rdata")
