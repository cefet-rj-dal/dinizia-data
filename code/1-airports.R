library(readr)

library(stringr)

#https://mesonet.agron.iastate.edu/sites/networks.php?network=BR__ASOS
#https://siros.anac.gov.br/siros/registros/

br_airports <- read_csv("airports/br-airports.csv")
br_airports$station_name <- str_to_title(br_airports$station_name)
save(br_airports, file="airports/br-airports.rdata")


fr_airports <- read_csv("airports/fr-airports.csv")
fr_airports$station_name <- str_to_title(fr_airports$station_name)
save(fr_airports, file="airports/fr-airports.rdata")


