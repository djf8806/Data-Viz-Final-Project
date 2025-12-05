library(tidyverse)
library(janitor)
library(lehdr)
library(tidycensus)
library(sf)

# Download LODES data (origin-destination) for New York
lodes <- grab_lodes(
  state = "ny",
  year = 2022,
  version = "LODES8",
  lodes_type = "od",
  job_type = "JT00",
  segment = "S000",
  state_part = "main",
  agg_geo = "tract"
) %>% 
  clean_names()

# Summarize by home tract, sum number of total workers in each tract
workers_home <- lodes %>% 
  group_by(h_tract) %>% 
  summarize(residents = sum(s000))

# Summarize by work tract, sum total number of workers in each tract
workers_work <-  lodes %>% 
  group_by(w_tract) %>% 
  summarize(workers = sum(s000))

# Join residents and workers
lodes_tracts <- workers_home %>% 
  full_join(workers_work,
            by = c("h_tract" = "w_tract")) %>% 
  rename(tract = h_tract)

# Download census tract geometry, population data, and household income data
nyc_census <- get_acs(
  state = "ny",
  geography = "tract",
  year = 2022,
  variables = c(population = "B01003_001",
                househ_income = "B19061_001"),
  output = "wide",
  geometry = TRUE
) %>% 
  clean_names()

# Merge census data to LODES data by tract, filter for NYC
census_lodes <- lodes_tracts %>%  
  left_join(nyc_census,
            by = c("tract" = "geoid"))%>% 
  filter(str_detect(name, "Bronx County|Queens County|Kings County|New York County|Richmond County"))

# Save as SHP
st_write(census_lodes, "r_output/census_lodes.shp")
