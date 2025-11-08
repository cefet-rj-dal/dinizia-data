library(readr)
library(stringr)

# Purpose: Load airport metadata (BR and FR) from CSV and persist as RData.
# Inputs:
#   - airports/br-airports.csv: Brazilian airport station list
#   - airports/fr-airports.csv: French airport station list
# Outputs:
#   - airports/br-airports.rdata: tibble 'br_airports'
#   - airports/fr-airports.rdata: tibble 'fr_airports'
# Notes:
#   - Title-case the 'station_name' for consistency.
#   - Data sources:
#       https://mesonet.agron.iastate.edu/sites/networks.php?network=BR__ASOS
#       https://siros.anac.gov.br/siros/registros/

# Load Brazil airports and normalize station names
br_airports <- read_csv("airports/br-airports.csv")
br_airports$station_name <- str_to_title(br_airports$station_name)
save(br_airports, file = "airports/br-airports.rdata")

# Load France airports and normalize station names
fr_airports <- read_csv("airports/fr-airports.csv")
fr_airports$station_name <- str_to_title(fr_airports$station_name)
save(fr_airports, file = "airports/fr-airports.rdata")
