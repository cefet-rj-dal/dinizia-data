library(readr)
library(stringr)
library(lubridate)
library(dplyr)

# Purpose: Process BR ASOS hourly data files into cleaned per-hour observations.
# Inputs: zip/CSV files under 'br-asos' (already unzipped or readable by readr)
# Outputs: RData files under 'br-asos-rdata' with object 'asos'
# Notes:
#   - Only keep on-the-hour records (minute == 0)
#   - Convert imperial temperatures (F) to Celsius
#   - Drop high-NA and unused columns
#   - Source reference: https://mesonet.agron.iastate.edu/ASOS/

# Columns to remove prior to selecting final set
columns_to_remove <- c(
  "mslp", "gust", "skyc2", "skyc3", "skyc4", "skyl1", "skyl2", "skyl3", "skyl4", "p01i",
  "wxcodes", "ice_accretion_1hr", "ice_accretion_3hr", "ice_accretion_6hr", "peak_wind_gust", "peak_wind_drct",
  "peak_wind_time", "snowdepth", "metar"
)

# Compute share of NAs per column and report those above threshold
validate_attributes <- function(df) {
  na_eval <- function(x) {
    y <- is.na(x)
    y <- y[y == TRUE]
    return(round(length(y) / length(x), digits = 2))
  }

  result <- sapply(df, na_eval)
  result <- result[result > 0.05]
  return(result)
}

# Add derived date/time fields, filter to hourly, select and convert units
enrich_asos <- function(asos) {
  asos$station_date <- date(asos$valid)
  asos$station_hour <- hour(asos$valid)
  asos$station_minute <- minute(asos$valid)

  asos <- asos |>
    filter(station_minute == 0) |>
    select(
      station, station_date, station_hour, valid,
      air_temperature = tmpf, dew_point = dwpf, relative_humidity = relh, wind_direction = drct,
      wind_speed = sknt, sky_coverage = skyc1, pressure = alti, visibility = vsby, apparent_temperature = feel
    ) |>
    distinct()

  # Convert Fahrenheit to Celsius for temperature metrics
  asos$air_temperature <- (asos$air_temperature - 32) * 5 / 9
  asos$dew_point <- (asos$dew_point - 32) * 5 / 9
  asos$apparent_temperature <- (asos$apparent_temperature - 32) * 5 / 9

  asos <- as_tibble(asos)

  return(asos)
}

# Iterate over BR ASOS files and generate cleaned RData outputs
input_files <- list.files("br-asos")
processing_log <- NULL
for (file_name in input_files) {
  input_path <- sprintf("br-asos/%s", file_name)
  print(input_path)
  output_path <- sprintf("br-asos-rdata/%s", str_replace(file_name, ".zip", ".rdata"))
  raw_data <- read_csv(input_path, col_types = cols(valid = col_character()))
  raw_data$valid <- strptime(raw_data$valid, "%Y-%m-%d %H:%M", tz = "GMT")
  na_stats <- validate_attributes(raw_data)
  print(na_stats)
  columns_to_keep <- colnames(raw_data)[is.na(pmatch(colnames(raw_data), columns_to_remove))]
  asos <- raw_data[, columns_to_keep]

  asos <- enrich_asos(asos)

  save(asos, file = output_path)
  processing_log <- rbind(processing_log, data.frame(file = file_name, col = ncol(asos)))
}





