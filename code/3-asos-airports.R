library(dplyr)
library(reshape)
library(zoo)
library(stringi)


asos_airports <- function(airports, asos) {
  asos <- asos |> dplyr::select(station = station, date = station_date, time = station_hour, air_temperature)
  
  data <- merge(airports, asos, by.x = "icao", by.y = "station") |> select(city = statecity, date, time, air_temperature) |> arrange(date, time, city)
  
  return(data)
}

load("airports/airports.rdata")
airports <- airports |> filter(country == "BRASIL") |> select(icao, statecity)

fil <- list.files("asos_rdata")
data <- NULL
for (f in fil) {
  filerdata <- sprintf("asos_rdata/%s", f)
  print(filerdata)
  load(filerdata)
  asos <- as_tibble(asos)
  asos <- asos_airports(airports, asos)
  data <- rbind(data, asos)
}

data_city <- cast(data, date+time ~ city, mean)

colnames(data_city) <- gsub(" ", "_", colnames(data_city))
colnames(data_city) <- gsub("-", "_", colnames(data_city))
colnames(data_city) <- stringi::stri_trans_general(colnames(data_city), "Latin-ASCII")

save(data_city, file="asos_airports/data_city.rdata")

for (i in 3:ncol(data_city)) {
  x <- data_city[,i]
  NonNAindex <- which(!is.na(x))
  firstNonNA <- min(NonNAindex)
  lastNonNA <- max(NonNAindex)
  data_city[1,i] <- x[firstNonNA]
  data_city[nrow(data_city),i] <- x[lastNonNA]
  data_city[,i] <- na.approx(data_city[,i])
}

save(data_city, file="asos_airports/data_city_inter.rdata")
