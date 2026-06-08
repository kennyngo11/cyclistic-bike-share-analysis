# ============================================================
# Cyclistic Bike-Share Analysis
# Google Data Analytics Capstone - Case Study 1
# Analyst: Kenny Ngo
# Data: July 2025 - June 2026 (12 months)
# ============================================================

# STEP 1: LOAD PACKAGES
install.packages("tidyverse")
install.packages("lubridate")
install.packages("janitor")

library(tidyverse)
library(lubridate)
library(janitor)

# ============================================================
# STEP 2: LOAD AND COMBINE ALL 12 CSV FILES
# ============================================================

setwd("C:/Users/kenny/OneDrive/Desktop/cyclistic-case-study/raw-data")

all_trips <- list.files(pattern = "*.csv") %>%
  lapply(read_csv) %>%
  bind_rows()

glimpse(all_trips)
# Result: 5,848,703 rows, 13 columns

# ============================================================
# STEP 3: CLEAN AND PROCESS DATA
# ============================================================

# add ride_length in minutes
all_trips$ride_length <- as.numeric(difftime(all_trips$ended_at, all_trips$started_at, units = "mins"))

# add day_of_week (Sun = 1, Sat = 7)
all_trips$day_of_week <- wday(all_trips$started_at, label = TRUE)

# add month and year columns
all_trips$month <- format(all_trips$started_at, "%B")
all_trips$year  <- format(all_trips$started_at, "%Y")

# remove bad data (negative or zero ride lengths, missing end coordinates)
all_trips_clean <- all_trips %>%
  filter(ride_length > 0) %>%
  drop_na(end_lat, end_lng)

# check how many rows were removed
nrow(all_trips) - nrow(all_trips_clean)
# Result: 5,925 rows removed

glimpse(all_trips_clean)
# Result: 5,842,778 rows, 17 columns

# ============================================================
# STEP 4: ANALYZE
# ============================================================

# average ride length by member type
avg_ride <- all_trips_clean %>%
  group_by(member_casual) %>%
  summarise(
    avg_ride_length    = mean(ride_length),
    median_ride_length = median(ride_length),
    max_ride_length    = max(ride_length),
    total_rides        = n()
  )

print(avg_ride)

# rides by day of week
rides_by_day <- all_trips_clean %>%
  group_by(member_casual, day_of_week) %>%
  summarise(
    total_rides     = n(),
    avg_ride_length = mean(ride_length)
  ) %>%
  arrange(member_casual, day_of_week)

print(rides_by_day)

# rides by month
rides_by_month <- all_trips_clean %>%
  group_by(member_casual, month) %>%
  summarise(
    total_rides     = n(),
    avg_ride_length = mean(ride_length)
  )

print(rides_by_month)

# bike type usage
bike_type <- all_trips_clean %>%
  group_by(member_casual, rideable_type) %>%
  summarise(total_rides = n())

print(bike_type)

# ============================================================
# STEP 5: EXPORT SUMMARY FILES FOR TABLEAU
# ============================================================

write_csv(avg_ride,       "C:/Users/kenny/OneDrive/Desktop/cyclistic-case-study/avg_ride.csv")
write_csv(rides_by_day,   "C:/Users/kenny/OneDrive/Desktop/cyclistic-case-study/rides_by_day.csv")
write_csv(rides_by_month, "C:/Users/kenny/OneDrive/Desktop/cyclistic-case-study/rides_by_month.csv")
write_csv(bike_type,      "C:/Users/kenny/OneDrive/Desktop/cyclistic-case-study/bike_type.csv")

# ============================================================
# KEY FINDINGS
# ============================================================

# 1. casual riders avg 18.9 mins per ride vs 12.1 mins for members
# 2. casual riders peak on weekends (leisure), members peak on weekdays (commuting)
# 3. both groups peak in summer, casual riders drop off sharply in winter
# 4. both groups prefer electric bikes over classic bikes

# tableau dashboard:
# https://public.tableau.com/app/profile/kenny.ngo6285/viz/CyclisticBikeShareAnalysis_17808909860820/CyclisticAnalysisDashboard
