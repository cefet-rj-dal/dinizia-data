library(dplyr)
library(reshape)
library(zoo)
library(stringi)
library(stringr)
library(lubridate)

# Purpose: Build BR-wide airport-by-time matrices for each ASOS attribute.
# Inputs:
#   - airports/br-airports.rdata (object: br_airports)
#   - br-asos-rdata/*.rdata (object: asos)
# Outputs:
#   - asos-airports/br-<attribute>-raw.rdata (wide matrix with NAs)
#   - asos-airports/br-<attribute>.rdata (wide matrix, NA-filled by interpolation)

# Join airport metadata with ASOS, align to date/time fields
asos_airports <- function(airports, asos) {
  merged <- merge(airports, asos, by.x = "stid", by.y = "station") |>
    arrange(station_date, station_hour, stid)
  names(merged)[names(merged) == "stid"] <- "station"
  names(merged)[names(merged) == "station_date"] <- "date"
  names(merged)[names(merged) == "station_hour"] <- "time"

  merged <- as_tibble(merged)

  return(merged)
}

# For a given attribute, build a city-by-time matrix aligned to the union timeline
process_attribute <- function(stations_df, timestamps_df, merged_data, attribute) {
  names(merged_data)[names(merged_data) == attribute] <- "value"
  merged_data <- merged_data |>
    select(station, station_name, date, time, value)
  city_data <- timestamps_df

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

    filtered_data <- merged_data |>
      filter(station == station_id) |>
      select(date, time, value) |>
      arrange(date, time)
    filtered_data <- as_tibble(filtered_data)
    if (nrow(filtered_data) >= 365) {
      filtered_data <- merge(
        x = timestamps_df, y = filtered_data,
        by.x = c("date", "time"), by.y = c("date", "time"), all.x = TRUE
      ) |>
        arrange(date, time)
      city_data[, station_name] <- filtered_data$value
    }
  }
  save(city_data, file = sprintf("asos-airports/br-%s-raw.rdata", attribute))

  return(city_data)
}

# Fill NAs forward/back then interpolate within; save final output
fill_attribute <- function(city_data, attribute) {
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

  save(city_data, file = sprintf("asos-airports/br-%s.rdata", attribute))

  return(city_data)
}

load("airports/br-airports.rdata")
br_airports <- br_airports |> select(stid, station_name)

input_files <- list.files("br-asos-rdata")
merged_data <- NULL
for (file_name in input_files) {
  rdata_path <- sprintf("br-asos-rdata/%s", file_name)
  print(rdata_path)
  load(rdata_path)
  asos <- asos_airports(br_airports, asos)
  merged_data <- rbind(merged_data, asos)
}

timestamps_df <- merged_data |> distinct(date, time) |> arrange(date, time)
stations_df <- merged_data |> distinct(station, station_name) |> arrange(station_name)

city_data <- process_attribute(stations_df, timestamps_df, merged_data, "air_temperature")
city_data <- fill_attribute(city_data, "air_temperature")

city_data <- process_attribute(stations_df, timestamps_df, merged_data, "dew_point")
city_data <- fill_attribute(city_data, "dew_point")

city_data <- process_attribute(stations_df, timestamps_df, merged_data, "relative_humidity")
city_data <- fill_attribute(city_data, "relative_humidity")

city_data <- process_attribute(stations_df, timestamps_df, merged_data, "wind_direction")
city_data <- fill_attribute(city_data, "wind_direction")

city_data <- process_attribute(stations_df, timestamps_df, merged_data, "wind_speed")
city_data <- fill_attribute(city_data, "wind_speed")

city_data <- process_attribute(stations_df, timestamps_df, merged_data, "pressure")
city_data <- fill_attribute(city_data, "pressure")

city_data <- process_attribute(stations_df, timestamps_df, merged_data, "visibility")
city_data <- fill_attribute(city_data, "visibility")

city_data <- process_attribute(stations_df, timestamps_df, merged_data, "apparent_temperature")
city_data <- fill_attribute(city_data, "apparent_temperature")
