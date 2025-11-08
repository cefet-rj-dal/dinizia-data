# dinizia-data

**Overview**
- This repository builds hourly airport-level weather and delay datasets for Brazil (BR) and France (FR) from ASOS observations and flight status inputs.
- Outputs include per-airport time series and country-wide “airport x time” matrices for key meteorological attributes and delays.

**Final Datasets**
- data/final/asos-airports
  - br-air_temperature(.rdata | -raw.rdata): wide matrix [date, time, <Station_Name_ID>]
  - br-dew_point(.rdata | -raw.rdata): wide matrix [date, time, <Station_Name_ID>]
  - br-relative_humidity(.rdata | -raw.rdata): wide matrix [date, time, <Station_Name_ID>]
  - br-wind_direction(.rdata | -raw.rdata): wide matrix [date, time, <Station_Name_ID>]
  - br-wind_speed(.rdata | -raw.rdata): wide matrix [date, time, <Station_Name_ID>]
  - br-pressure(.rdata | -raw.rdata): wide matrix [date, time, <Station_Name_ID>]
  - br-visibility(.rdata | -raw.rdata): wide matrix [date, time, <Station_Name_ID>]
  - br-apparent_temperature(.rdata | -raw.rdata): wide matrix [date, time, <Station_Name_ID>]
  - br-flights(.rdata | -raw.rdata): wide matrix [date, time, <Station_Name_ID>]
  - br-delays(.rdata | -raw.rdata): wide matrix [date, time, <Station_Name_ID>]
  - br-delay_perc(.rdata | -raw.rdata): wide matrix [date, time, <Station_Name_ID>]
- data/final/airports/br
  - <Station_Name_ID>.rdata: per-airport hourly time series [date, time, attributes]
- data/final/airports/fr
  - <Station_Name_ID>.rdata: per-airport hourly time series [date, time, attributes]

**Schema (Common Fields)**
- date: Date (UTC) of observation.
- time: Integer hour of day (0–23, UTC).
- Station columns (wide matrices in data/final/asos-airports): One column per station named as TitleCase_StationName_<ID>.

**Meteorological Attributes and Units**
- air_temperature: Celsius (converted from ASOS tmpf).
- dew_point: Celsius (converted from ASOS dwpf).
- apparent_temperature: Celsius (converted from ASOS feel).
- relative_humidity: Percent (ASOS relh).
- wind_direction: Degrees (0–360, ASOS drct).
- wind_speed: Knots (ASOS sknt).
- pressure: Inches of mercury (ASOS alti).
- visibility: Statute miles (ASOS vsby).
- sky_coverage: ASOS sky condition code (skyc1); present only in per‑airport series.

**Delays/Flights Fields**
- flights: Number of flights in the hour.
- delays: Number of delayed flights in the hour.
- delay_perc: delays / flights (0–1); computed during ETL.

**Data Sources**
- ASOS hourly observations (Zip/CSV): Iowa Environmental Mesonet (IEM) ASOS archive
  - https://mesonet.agron.iastate.edu/ASOS/
  - Station networks reference for BR (and other networks): https://mesonet.agron.iastate.edu/sites/networks.php?network=BR__ASOS
- Airport station lists (CSV): Provided in this repository under source data; station names normalized to Title Case.
- Airport flight status (RData): Provided in this repository as `airport_status.rdata` with hourly flights/delays per station.

**Data Locations**
- Source Data
  - data/source/airports/br-airports.csv
  - data/source/airports/fr-airports.csv
  - data/source/airports/airport_status.rdata
  - data/source/asos/br
  - data/source/asos/fr
- Intermediate Data
  - data/intermediate/airports/br-airports.rdata
  - data/intermediate/airports/fr-airports.rdata
  - data/intermediate/asos/br
  - data/intermediate/asos/fr
- Final Data
  - data/final/asos-airports
  - data/final/airports/br
  - data/final/airports/fr

**ETL Overview**
- Hourly ASOS zip/CSV files are parsed, keeping only on-the-hour records (minute == 0).
- Temperature fields (tmpf, dwpf, feel) are converted from Fahrenheit to Celsius.
- Attributes are selected and renamed to consistent English names.
- Airport metadata is joined to ASOS, then:
  - Per-airport hourly time series files are generated (one file per station).
  - Country-wide attribute matrices are generated (date/time rows, station columns) and saved as raw and interpolated variants.
- Delay/flight data is merged on the same date/time grid to generate flights, delays, and delay_perc matrices.

**Missing Data Handling**
- For wide matrices, each station series is:
  - Back/forward-filled at the endpoints to the first/last observed value.
  - Linearly interpolated for internal gaps (using `zoo::na.approx`).

**ETL Code and Run Order**
- code/1-load-airports.R: Load airport CSVs, save RData metadata.
- code/2-process-asos-br.R: Parse/clean BR ASOS into hourly RData per year.
- code/3-process-asos-fr.R: Parse/clean FR ASOS into hourly RData per year.
- code/4-build-asos-airports-br.R: Build BR attribute matrices (raw + interpolated).
- code/5-build-asos-airports-fr.R: Build FR attribute matrices (raw + interpolated).
- code/6-build-airport-timeseries-br.R: Build per-airport hourly series for BR.
- code/7-build-airport-timeseries-fr.R: Build per-airport hourly series for FR.
- code/8-build-asos-airports-delays-br.R: Build BR flights/delays/delay_perc matrices.

Note: Only the new `data/source`, `data/intermediate`, and `data/final` directory structure is supported.
