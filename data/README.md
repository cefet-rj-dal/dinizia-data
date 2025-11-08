# Data Directory

Overview
- data/source: Original/raw inputs used by the ETL.
- data/intermediate: Processed working datasets used between ETL stages.
- data/final: Final deliverables ready for analysis and consumption.

data/source
- airports
  - br-airports.csv: Brazilian airport station list (CSV).
  - fr-airports.csv: French airport station list (CSV).
  - airport_status.rdata: Hourly flights/delays per station (object: airport_status).
- asos/br
  - Raw ASOS files for Brazil. Typically zipped CSVs as provided by IEM ASOS.
- asos/fr
  - Raw ASOS files for France. Typically zipped CSVs as provided by IEM ASOS.

data/intermediate
- airports
  - br-airports.rdata: Cleaned airport metadata (object: br_airports).
  - fr-airports.rdata: Cleaned airport metadata (object: fr_airports).
- asos/br
  - asosYYYY.rdata: Hourly ASOS observations for year YYYY in Brazil (object: asos).
    - Contains: station (ID), valid (POSIXct), station_date, station_hour, and selected attributes
      such as air_temperature, dew_point, relative_humidity, wind_direction, wind_speed,
      sky_coverage, pressure, visibility, apparent_temperature. Only minute == 0 records retained.
- asos/fr
  - asosYYYY.rdata: Hourly ASOS observations for year YYYY in France (object: asos). Same structure as BR.

data/final
- airports/br
  - <Station_Name_ID>.rdata: Per‑airport hourly time series for Brazil (object: airport),
    with columns [date, time, air_temperature, dew_point, relative_humidity, wind_direction,
    wind_speed, sky_coverage, pressure, visibility, apparent_temperature].
- airports/fr
  - <Station_Name_ID>.rdata: Per‑airport hourly time series for France (object: airport), same structure as BR.
- asos-airports
  - br-<attribute>-raw.rdata: Wide matrix [date, time, <one column per station>] before gap filling.
  - br-<attribute>.rdata: Same matrix after endpoint filling and linear interpolation of gaps.
  - fr-<attribute>-raw.rdata: Wide matrix for France before gap filling.
  - fr-<attribute>.rdata: Same matrix after interpolation.
  - Attributes include: air_temperature, dew_point, relative_humidity, wind_direction,
    wind_speed, pressure, visibility, apparent_temperature, and (BR only) flights, delays, delay_perc.

Conventions
- Station columns in wide matrices are named TitleCase_StationName_<ID> (ASCII only).
- Temperature fields are converted to Celsius; only records at exact hours (minute == 0) are kept.
- “-raw.rdata” files contain original gaps (NA); non-raw versions are filled using endpoint carry and
  `zoo::na.approx` for internal gaps.

