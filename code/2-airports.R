library(readr)


#https://siros.anac.gov.br/siros/registros/

airports <- read_delim("airports/airports.csv", delim = ";", escape_double = FALSE, 
                       locale = locale(decimal_mark = ",", grouping_mark = "."), trim_ws = TRUE)
colnames(airports) <- c("icao", "iata", "name", "city", "state", 
                        "country", "criticalairplane", "latitude", "longitude")
airports$statecity <- sprintf("%s-%s", airports$state, airports$city)
save(airports, file="airports/airports.rdata")
