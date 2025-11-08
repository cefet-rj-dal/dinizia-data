library(readr)
library(stringr)

# Purpose: Load airport metadata (BR and FR) from CSV and persist as RData.
# Inputs:
#   - data/source/airports/br-airports.csv
#   - data/source/airports/fr-airports.csv
# Outputs:
#   - data/intermediate/airports/br-airports.rdata: tibble 'br_airports'
#   - data/intermediate/airports/fr-airports.rdata: tibble 'fr_airports'
# Notes:
#   - Title-case the 'station_name' for consistency.
#   - Data sources:
#       https://mesonet.agron.iastate.edu/sites/networks.php?network=BR__ASOS
#       https://siros.anac.gov.br/siros/registros/

ensure_dir_for <- function(path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
}

airports_source_dir <- "data/source/airports"
airports_intermediate_dir <- "data/intermediate/airports"

# Load Brazil airports and normalize station names
br_csv <- file.path(airports_source_dir, "br-airports.csv")
br_airports <- read_csv(br_csv)
br_airports$station_name <- str_to_title(br_airports$station_name)
br_rdata <- file.path(airports_intermediate_dir, "br-airports.rdata")
ensure_dir_for(br_rdata)
save(br_airports, file = br_rdata)

# Load France airports and normalize station names
fr_csv <- file.path(airports_source_dir, "fr-airports.csv")
fr_airports <- read_csv(fr_csv)
fr_airports$station_name <- str_to_title(fr_airports$station_name)
fr_rdata <- file.path(airports_intermediate_dir, "fr-airports.rdata")
ensure_dir_for(fr_rdata)
save(fr_airports, file = fr_rdata)
