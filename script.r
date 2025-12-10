library(tidyverse)
library(janitor)
library(lehdr)
library(tidycensus)
library(sf)
library(lubridate)
library(scales)
library(ggplot2)

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
    avg_hourly_ridership = avg_ridership/hrs)

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
      commute_type == "Non-commute" ~ 17 ),
    avg_hourly_ridership = avg_ridership/hrs)


comparison_period_fixed <- comparison_period %>%
  mutate(
    # put time periods in logical order
    time_period = factor(
      time_period,
      levels = c("AM peak", "Midday", "PM peak", "Evening", "Overnight")),
    # turn ugly codes into nice labels
    mode = recode(
      mode,
      "avg_ferry"  = "Ferry",
      "avg_subway" = "Subway" ))


ggplot(comparison_period_fixed,
       aes(x = time_period,
           y = avg_hourly_ridership,
           fill = mode)) +
  
  geom_col(position = position_dodge(width = 0.75), width = 0.65) +
  
  geom_text(
    aes(label = comma(round(avg_hourly_ridership))),
    position = position_dodge(width = 0.75),
    vjust = -0.25,
    size = 3,
    color = "black"
  ) +
  
  # --- COLOR PALETTE ---
  scale_fill_manual(
    values = c(
      "Ferry" = "#AD1AAC",   # Rockaway Ferry magenta/purple
      "Subway" = "#0039A6"   # MTA blue
    ) ) +
  
  scale_y_continuous(
    labels = scales::comma,
    expand = expansion(mult = c(0, 0.15)) ) +
  
  labs(
      title = "Average Hourly Ridership for \nNYC Ferry Landings and nearby Subway Stations",
      subtitle = "Ferries attract little commuter traffic compared to \nnearby subway stations, limiting their role in peak-hour travel.",
    x = "",
    y = "Average hourly ridership",
    fill = NULL,
    caption = "Sources: NYC Ferry Ridership (2022); MTA Subway Hourly Ridership (2022)"
    ) +
  coord_cartesian(clip = "off") + 
  
  theme_minimal(base_size = 14) +
  theme(
    # remove gridlines
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    
    # thinner, lighter axis lines + ticks
    axis.line.x = element_line(color = "gray60", linewidth = 0.3),
    axis.line.y = element_line(color = "gray60", linewidth = 0.3),
    axis.ticks = element_line(color = "gray60", linewidth = 0.03),
    axis.ticks.length = unit(0.12, "cm"),
    
    # black text everywhere
    text = element_text(color = "black"),
    axis.title = element_text(color = "black"),
    axis.text = element_text(color = "black"),
    
    # alignment
    plot.title.position = "plot",
    plot.title = element_text(color = "black", face = "bold", hjust = 0.5, lineheight = 1.1),
    plot.subtitle = element_text(color = "black", hjust = 0.5, lineheight = 1.1),
    plot.caption = element_text(size = 9, color = "gray30", hjust = 1.0),
    
    # legend alignment
    legend.position = c(0, 1),
    legend.justification = c(0, 1),
    legend.text = element_text(size = 12, color = "black"),
    legend.box.margin = margin(5, 0, 10, 0),
    
    # more space so nothing feels cramped
    plot.margin = margin(t = 30, r = 20, b = 20, l = 20)
  )




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

# table to show all numbers 

# 1) Annual ferry totals per landing
ferry_totals <- read_csv("r_output/ferry_stop_totals_2022.csv") %>%
  clean_names()
# columns (from your script): stop, total_boardings, commute_boardings, noncommute_boardings,
#                             weekday_boardings, weekend_boardings

# 2) Ferry ridership by time period per landing
ferry_period <- read_csv("r_output/ferry_stop_timeperiod_2022.csv") %>%
  clean_names()
# columns: stop, time_period, period_hrs, boardings, avg_hourly_boardings

# 3) LODES jobs + workers per landing
lodes_by_landing <- read_csv("q_output//LODES_by_landing/LODES_by_landing.csv") %>%
  clean_names()
lodes_by_landing_clean <- lodes_by_landing %>%
  rename(
    stop            = new_field,
    residents_15m   = resdnts,
    workers_15m     = workers,
    population_15m  = popltn,    # note the underscore
    hh_income       = hosh_ncm   # note the underscore
  ) %>%
  select(stop, residents_15m, workers_15m, population_15m, hh_income)


# pivot time-period data wide to get one column per period
ferry_hourly_wide <- ferry_period %>%
  select(stop, time_period, avg_hourly_boardings) %>%
  mutate(
    time_period = factor(
      time_period,
      levels = c("AM peak", "Midday", "PM peak", "Evening", "Overnight")
    )
  ) %>%
  pivot_wider(
    names_from  = time_period,
    values_from = avg_hourly_boardings,
    names_prefix = "hourly_"
  )%>%
  clean_names()


#clean / rename LODES by landing 
lodes_by_landing_clean <- lodes_by_landing %>%
  rename(
    stop            = new_field,   # landing name
    residents_15m   = resdnts,     # residents within 15-minute walk
    workers_15m     = workers,     # people who WORK within 15-minute walk (jobs)
    population_15m  = popltn,      # total population (if different from residents)
    hh_income       = hosh_ncm     # household income
  ) %>%
  select(stop, residents_15m, workers_15m, population_15m, hh_income)


landing_summary <- ferry_totals %>%
  left_join(ferry_hourly_wide, by = "stop") %>%
  left_join(lodes_by_landing_clean, by = "stop") %>%
  mutate(
    commute_share        = commute_boardings / total_boardings,
    weekend_share        = weekend_boardings / total_boardings,
    peak_hourly          = pmax(hourly_am_peak, hourly_pm_peak, na.rm = TRUE),
    midday_hourly        = hourly_midday,
    peak_to_midday_ratio = peak_hourly / midday_hourly
  )


write_csv(landing_summary, "r_output/landing_level_summary_2022.csv")

landing_summary %>%
  select(
    stop,
    total_boardings,
    hourly_am_peak,
    hourly_midday,
    commute_share,
    workers_15m
  ) %>%
  arrange(desc(total_boardings)) %>%
  write_csv("r_output/landing_summary_for_slides.csv")


#----------------

