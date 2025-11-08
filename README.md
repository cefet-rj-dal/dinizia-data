# dinizia-data

Data directory layout
- data/source: original/raw inputs (e.g., ASOS zips, airport CSVs, airport_status)
- data/intermediate: processed working datasets (e.g., airports RData, per-year ASOS RData)
- data/final: final deliverables (e.g., airport time series and attribute matrices)

Note: Only the new data directory structure above is supported.

ETL order
1. code/1-load-airports.R
2. code/2-process-asos-br.R
3. code/3-process-asos-fr.R
4. code/4-build-asos-airports-br.R
5. code/5-build-asos-airports-fr.R
6. code/6-build-airport-timeseries-br.R
7. code/7-build-airport-timeseries-fr.R
8. code/8-build-asos-airports-delays-br.R
