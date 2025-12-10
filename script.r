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

## ---------------------------------------------------------------
  
library(lubridate)

# clean ferry ridership data 
ferry <- read_csv("r_data/NYC_Ferry_Ridership_2022.csv") %>% 
  clean_names() 

# columns now: date, hour, route, direction, stop, boardings, type_day

#categorize data using mutate
ferry <- ferry %>% 
  mutate(
    date      = mdy(date),              
    hour      = as.integer(hour),       # 0–23
    month     = floor_date(date, "month"),
    weekday   = wday(date, label = TRUE),     
    is_weekend = weekday %in% c("Sat", "Sun"))

#Create peak, midday, and evening travel categories 
ferry <- ferry %>% 
  mutate(
    time_period = case_when(
      hour >= 6  & hour < 10 ~ "AM peak",
      hour >= 16 & hour < 19 ~ "PM peak",
      hour >= 10 & hour < 16 ~ "Midday",
      hour >= 19 & hour < 23 ~ "Evening",
      TRUE                  ~ "Overnight"),
    period_hrs = case_when(
      time_period == "AM peak" ~ 4,
      time_period == "PM peak" ~ 3,
      time_period == "Midday" ~ 6,
      time_period == "Evening" ~ 4,
      time_period == "Overnight" ~ 7
      )
    )

# Classify “commute vs non-commute” times 
ferry <- ferry %>% 
  mutate(
    commute_period = time_period %in% c("AM peak", "PM peak"),
    commute_type = if_else(!is_weekend & commute_period,
                           "Commute",
                           "Non-commute"))

# Summarize annual totals per stop
ferry_stop_totals <- ferry %>% 
  group_by(stop) %>% 
  summarize(
    total_boardings = sum(boardings, na.rm = TRUE),
    commute_boardings = sum(boardings[commute_type == "Commute"], na.rm = TRUE),
    noncommute_boardings = sum(boardings[commute_type == "Non-commute"], na.rm = TRUE),
    weekday_boardings = sum(boardings[!is_weekend], na.rm = TRUE),
    weekend_boardings = sum(boardings[is_weekend], na.rm = TRUE),
    .groups = "drop")

write_csv(ferry_stop_totals, "r_output/ferry_stop_totals_2022.csv")

# Summarize stop x time_period (To see which stops are commuter vs midday)
ferry_stop_period <- ferry %>% 
  group_by(stop, time_period, period_hrs) %>% 
  summarize(
    boardings = sum(boardings, na.rm = TRUE),
    .groups = "drop") %>% 
  mutate(avg_hourly_boardings = boardings/period_hrs)

#Save as CSV
write_csv(ferry_stop_period, "r_output/ferry_stop_timeperiod_2022.csv")


#--------------

#Import previously-cleaned subway ridership data (see Q procedure file for R code)
subway <- read_csv("r_data/subway_ridership_15min_walk_ferry_2022.csv") %>% 
  clean_names() 

#categorize data using mutate
subway <- subway %>%
mutate(
  transit_timestamp = mdy_hms(transit_timestamp),
  
  date = as_date(transit_timestamp),
  hour = hour(transit_timestamp),
  month = floor_date(date, "month"),
  weekday = wday(date, label = TRUE),
  is_weekend = weekday %in% c("Sat", "Sun"))

#Create peak, midday, and evening travel categories and classify “commute vs non-commute” times 
subway <- subway %>%
  mutate(
    time_period = case_when(
      hour >= 6  & hour < 10 ~ "AM peak",
      hour >= 16 & hour < 19 ~ "PM peak",
      hour >= 10 & hour < 16 ~ "Midday",
      hour >= 19 & hour < 23 ~ "Evening",
      TRUE                  ~ "Overnight" ),
    period_hrs = case_when(
      time_period == "AM peak" ~ 4,
      time_period == "PM peak" ~ 3,
      time_period == "Midday" ~ 6,
      time_period == "Evening" ~ 4,
      time_period == "Overnight" ~ 7
    ),
    commute_period = time_period %in% c("AM peak", "PM peak"),
    commute_type = if_else(!is_weekend & commute_period,
                           "Commute", "Non-commute") )

# Summarize annual totals per stop
subway_station_totals <- subway %>% 
  group_by(station_complex_id, station_complex) %>% 
  summarize(
    total_ridership       = sum(ridership, na.rm = TRUE),
    commute_ridership     = sum(ridership[commute_type == "Commute"], na.rm = TRUE),
    noncommute_ridership  = sum(ridership[commute_type == "Non-commute"], na.rm = TRUE),
    weekday_ridership     = sum(ridership[!is_weekend], na.rm = TRUE),
    weekend_ridership     = sum(ridership[is_weekend], na.rm = TRUE),
    .groups = "drop")

write_csv(subway_station_totals, "r_output/subway_station_totals_2022.csv")

# Summarize stop x time_period (To see which stops are commuter vs midday)
subway_stop_period <- subway %>%
  group_by(station_complex, time_period, period_hrs) %>%
  summarize(
    ridership = sum(ridership, na.rm = TRUE),
    .groups = "drop") %>% 
  mutate(avg_hourly_ridership = ridership/period_hrs)

#Save as CSV
write_csv(subway_stop_period, "r_output/subway_stop_timeperiod_2022.csv")

#---- summaries, analysis & charts 

# Ferry summary (by time period, weekday/weekend)
ferry_summary_period <- ferry %>%
  group_by(time_period, period_hrs) %>%
  summarize(
    total = sum(boardings, na.rm = TRUE)) %>%
  mutate(mode = "Ferry",
         avg_hourly_ridership = total/period_hrs
         )

ferry_summary_weektype <- ferry %>%
  group_by(is_weekend) %>%
  summarize(
    total = sum(boardings, na.rm = TRUE) ) %>%
  mutate(mode = "Ferry")

#Average ridership per ferry landing by time period
ferry_avg_by_period <- ferry %>%
  group_by(stop, time_period) %>%
  summarise(total = sum(boardings, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(time_period) %>%
  summarise(avg_ferry = mean(total)) 

#Average ridership per ferry landing by commute vs non-commute
ferry_avg_commute <- ferry %>%
  group_by(stop, commute_type) %>%
  summarise(total = sum(boardings, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(commute_type) %>%
  summarise(avg_ferry = mean(total))

# Subway summary (by time period, weekday/weekend)
subway_summary_period <- subway %>%
  group_by(time_period, period_hrs) %>%
  summarize(
    total = sum(ridership, na.rm = TRUE) ) %>%
  mutate(mode = "Subway",
         avg_hourly_ridership = total/period_hrs
  )

subway_summary_weektype <- subway %>%
  group_by(is_weekend) %>%
  summarize(
    total = sum(ridership, na.rm = TRUE)) %>%
  mutate(mode = "Subway")

#Average ridership per subway station by time period
  #per station, per period totals
subway_period_station_totals <- subway %>%
  group_by(station_complex_id, time_period, period_hrs) %>%
  summarize(total = sum(ridership, na.rm = TRUE)) %>% 
  mutate(avg_hourly_ridership = total/period_hrs)
  
  #avg across stations
subway_avg_by_period <- subway_period_station_totals %>%
  group_by(time_period) %>%
  summarize(avg_subway = mean(total))

#Average ridership per subway station by commute vs non-commute
  #per station totals
subway_commute_station_totals <- subway %>%
  group_by(station_complex_id, commute_type) %>%
  summarize(total = sum(ridership, na.rm = TRUE))
  
  #avg across stations
subway_avg_commute <- subway_commute_station_totals %>%
  group_by(commute_type) %>%
  summarize(avg_subway = mean(total))

#Combine Ferry + Subway for Comparison Charts
  #time period comparison table 
comparison_period <- ferry_avg_by_period %>%
  left_join(subway_avg_by_period, by = "time_period") %>%
  pivot_longer(cols = c(avg_ferry, avg_subway),
               names_to = "mode",
               values_to = "avg_ridership") %>% 
  mutate(
    hrs = case_when(
    time_period == "AM peak" ~ 4,
    time_period == "PM peak" ~ 3,
    time_period == "Midday" ~ 6,
    time_period == "Evening" ~ 4,
    time_period == "Overnight" ~ 7
    ),
    avg_hourly_ridership = avg_ridership/hrs
  )

#Save as CSV
write_csv(comparison_period, "r_output/average_ridership_comparison.csv")

  #commute vs non-commute table
comparison_commute <- ferry_avg_commute %>%
  left_join(subway_avg_commute, by = "commute_type") %>%
  pivot_longer(cols = c(avg_ferry, avg_subway),
               names_to = "mode",
               values_to = "avg_ridership") %>% 
  mutate(
    hrs = case_when(
      commute_type == "Commute" ~ 7,
      commute_type == "Non-commute" ~ 17
      ),
    avg_hourly_ridership = avg_ridership/hrs
  )

#Plot: Average Ridership per Location by Time of Day
ggplot(comparison_period,
       aes(x = time_period, y = avg_hourly_ridership, fill = mode)) +
  geom_col(position = "dodge") +
  labs(
    title = "Average Ridership per Landing/Station by Time of Day",
    x = "Time Period",
    y = "Average Ridership",
    fill = "Mode"
  ) +
  theme_minimal()+
  scale_y_continuous()

#Plot: Average Ridership per Location (Commute vs Non-commute)
ggplot(comparison_commute,
       aes(x = commute_type, y = avg_hourly_ridership, fill = mode)) +
  geom_col(position = "dodge") +
  labs(
    title = "Average Ridership per Landing/Station: Commute vs Non-Commute",
    x = "",
    y = "Average Ridership"
  ) +
  theme_minimal()


#Combining summaries 
summary_period_combined <- bind_rows(
  ferry_summary_period,
  subway_summary_period)

summary_weektype_combined <- bind_rows(
  ferry_summary_weektype,
  subway_summary_weektype)


#Summarize subway ridership in ferry neighborhoods
subway_total_near_ferry <- subway %>%
  summarize(total = sum(ridership, na.rm = TRUE)) %>%
  mutate(mode = "Subway (near ferry)")

#Summarize ferry ridership by landing
ferry_by_landing <- ferry %>%
  group_by(stop) %>%      # or `landing` depending on your column name
  summarise(
    ferry_total = sum(boardings, na.rm = TRUE),
    ferry_commute = sum(boardings[commute_type == "Commute"], na.rm = TRUE),
    ferry_noncommute = sum(boardings[commute_type == "Non-commute"], na.rm = TRUE),
    .groups = "drop"
  )

#----------------

