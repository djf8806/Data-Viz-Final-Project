Q procedure
-	Import LODES data from R
-	Import ferry landings (delete non-NYC Ferry landings manually: Whitehall, Governor’s Island, and Battery Maritime)
-	Import subway stations
-	Import boro boundaries

Clip LODES to boro boundaries

Calculate 0.5 mi buffer, dissolved around subway stations, clip to boro boundaries, calculate area
-	129.365 sq mi within 0.5 mi of subway stations.
Calculate isochrones, 15 min walk from ferry landings, dissolve, calculate area
-	10.844 sq mi within walking distance from ferry landings
Calculate area within 15min walk from ferry landings NOT covered by subway
-	Difference: input 15min_walk, overlay subway_buffer
-	3.649 sq mi within walking distance from ferry landings NOT within 0.5 mi of subway

Identify census tracts that have areas within 15 mins walk from ferry
-	Select by location census_lodes_clipped: intersect with 15min_walk_ferry, export as a new layer
o	405,301 resident workers
o	985,695 jobs
o	878,451 overall population

Identify census tracts that have areas within 15 mins walk from ferry NOT covered by subway
-	Select by location census_lodes_clipped: intersect with difference_15min_walk_ferry, export as a new layer
o	189,761 resident workers
o	376,755 jobs
o	435,873 overall population

Identify census tracts that have areas covered by subway
-	Select by location census_lodes_clipped: intersect with subway_buffer, export as a new layer
o	2,811,393 resident workers
o	3,492,441 jobs
o	6,954,163 overall population

Identify LODES statistics on a landing-by-landing basis
-	Calculate 15-minute walking isochrones for ferry landings. 
-	Join by location census_lodes_clipped: intersect to landings_isochrones. 
-	Concatenate landings_isochrones: group by isochrone ID, sum census data. 
-	Manually rename isochrones to match ferry landing names.

Identify the subway stations that are within 15 mins of ferry landings 
-	Select By Location: vector research tools  select by location 
-	Select features from: subway_stations ; where the features intersect ; by comparing to the features from: 15min_walk_ferry; create new selection. 
-	Export selection as new shapefile layer: subway_stations_15min_walk_ferry
-	Export selection as new csv to use to clean subway ridership data: subway_stations_15min_walk_ferry.csv

Import cleaned [in R] Ferry Ridership data into QGIS 
-	Layer  add layer  add delimited text layer

Use Field Calculator to rename ferry landing stop names to match the ridership data 

Join ferry ridership to ferry landings 

Calculate AM to PM ratio
-	Field Calculator  Create new decimal field  hourly_am / hourly_pm
-	Save as duplicate layer for easy symbolization

Note: 
The subway ridership dataset included in this repository is cleaned to significantly reduce size. 

R code to clean MTA Subway Hourly Ridership: 2020-2024, which was downloaded after the data was queried on data.ny.gov: filtered to 2022, subway only, removed unnecessary columns: transit_mode, payment_method, fare_class_category.  

# Clean subway ridership data 
MTA_Subway_Hourly_Ridership_2022 <- read_csv("r_data/MTA_Subway_Hourly_Ridership__2022.csv") %>% 
  clean_names() 

#Import QGIS output: subway stations within 15 mins of ferry landings
subway_stations_15min_walk_ferry <- read_csv("q_output/subway_stations_15min_walk_ferry/subway_stations_15min_walk_ferry.csv") %>% 
  clean_names()

# Filter ridership to ONLY stations within 15-min walk of ferries
subway_ridership_15min_walk_ferry <- MTA_Subway_Hourly_Ridership_2022 %>% 
  semi_join(
    subway_stations_15min_walk_ferry,
    by = c("station_complex_id" = "complex_id")
  )

#Save as CSV
write_csv(subway_ridership_15min_walk_ferry, "r_output/subway_ridership_15min_walk_ferry_2022.csv")
