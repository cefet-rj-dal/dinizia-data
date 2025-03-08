library(readr)
library(stringr) 
library(lubridate)
library(dplyr)

#unzip asos2000.zip
#https://mesonet.agron.iastate.edu/ASOS/


cols_rm <- c("mslp", "gust", "skyc2", "skyc3", "skyc4", "skyl1", "skyl2", "skyl3", "skyl4", "p01i",
             "wxcodes", "ice_accretion_1hr", "ice_accretion_3hr", "ice_accretion_6hr", "peak_wind_gust", "peak_wind_drct",
             "peak_wind_time", "snowdepth", "metar")

validate_attributes <- function(data) {
  na.eval <- function(x) {
    y <- is.na(x)
    y <- y[y==TRUE]
    return(round(length(y)/length(x), digits = 2))
  }
  
  result <- sapply(data, na.eval)
  result <- result[result > 0.05]
  return(result)
}

enrich_asos <- function(asos) {
  asos$station_date <- date(asos$valid)
  asos$station_hour <- hour(asos$valid)
  
  asos <- asos |> select(station, station_date, station_hour, valid, lon, lat, 
                         elevation, tmpf, dwpf, relh, drct, sknt, skyc1, alti, vsby, feel) |>
    group_by(station, station_date, station_hour) |> 
    summarise(valid = min(valid), lon = max(lon), lat = max(lat), elevation = max(elevation),
              air_temperature = max(tmpf), dew_point = max(dwpf), relative_humidity = max(relh),
              wind_direction = max(drct), wind_speed = max(sknt), sky_coverage = max(skyc1),
              pressure = max(alti), visibility = max(vsby), apparent_temperature = max(feel))
  
  asos$air_temperature <- (asos$air_temperature - 32) * 5/9
  asos$dew_point <- (asos$dew_point - 32) * 5/9
  asos$apparent_temperature <- (asos$apparent_temperature - 32) * 5/9
  
  asos <- as_tibble(asos)  
  
  return(asos)
}

fil <- list.files("br-asos")
process <- NULL
for (f in fil) {
  filecsv <- sprintf("br-asos/%s", f)
  print(filecsv)
  filerdata <- sprintf("br-asos-rdata/%s", str_replace(f, ".zip", ".rdata"))
  data <- read_csv(filecsv, col_types = cols(valid = col_character()))
  data$valid <- strptime(data$valid,"%Y-%m-%d %H:%M", tz="GMT")
  result <- validate_attributes(data)
  print(result)
  res <- colnames(data)[is.na(pmatch(colnames(data),cols_rm))]
  asos <- data[,res]
  
  asos <- enrich_asos(asos)
  
  save(asos, file=filerdata)
  process <- rbind(process, data.frame(file = f, col = ncol(asos)))
}





