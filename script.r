library(tidyverse)
library(janitor)
library(lehdr)
library(tidycensus)

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

# Merge census data to LODES data by home tracts, filter for NYC
nyc_census_lodes <- lodes %>%  
  left_join(nyc_census,
            by = c("h_tract" = "geoid"))%>% 
  filter(str_detect(name, "Bronx County|Queens County|Kings County|New York County|Richmond County"))
