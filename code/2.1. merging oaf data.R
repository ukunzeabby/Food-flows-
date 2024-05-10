library(tidyverse)
library(sf)
library(leaflet)
library(diagram)
library(lubridate)
library(readxl)
library(dplyr)
library(stringr)
library(readxl)
library(dplyr)

# Set working directory
setwd("C:/Users/HP/Desktop/IITA/IITA 2024/LFSM/food_flows/Data/")

# Read maintainable sheet from Vendor_information 11
main <- read_excel("OAF/vendor_information_11_ by 02 04 2024.xlsx", sheet = "maintable")
str(main)
# Selecting relevant columns from main sheet
main_df <- main %>%
  select("survey_start","province","district","market","age","sex")

# Date and time formatting
main_df$survey_start <- as.POSIXlt(main_df$survey_start, format="%Y-%m-%d %H:%M:%S")

# Create new columns for date and time
main_df <- main_df %>%
  mutate(
    date = as.Date(survey_start),
    time = format(survey_start, "%H:%M:%S")
  ) %>%
  select(date, time, everything()) %>%
  select(-survey_start)

# Rename columns
main_df <- main_df %>%
  rename(
    Gender = sex,
    Province = province,
    District = district
  )

# Read items_sold_repeat sheet from Vendor_information 11
repeatd <- read_excel("OAF/vendor_information_11_ by 02 04 2024.xlsx", sheet = "items_sold_repeat")
str(repeatd)
# Selecting relevant columns from items_sold_repeat sheet
repeat_df <- repeatd %>%
  select("survey_start","items_sold_label","source_channel","source_location",
         "produced_location","market_transport","othmarket_current","othmarket_location")

# Date and time formatting
repeat_df$survey_start <- as.POSIXlt(repeat_df$survey_start, format="%Y-%m-%d %H:%M:%S")

# Create new columns for date and time
repeat_df <- repeat_df %>%
  mutate(
    date = as.Date(survey_start),
    time = format(survey_start, "%H:%M:%S")
  ) %>%
  select(date, time, everything()) %>%
  select(-survey_start)

# Merging items_sold_repeat sheet with maintable sheet and changing case sensitivity of some columns
rpt_main_df <- merge(repeat_df, main_df, by = c("date", "time"))

# Read dismark_route_rep sheet from Vendor_information 11
dismark <- read_excel("OAF/vendor_information_11_ by 02 04 2024.xlsx", sheet = "dismark_route_rep")

# Selecting relevant columns from dismark_route_rep sheet
dismark_df <- dismark %>%
  select("survey_start", "dismark_route_label", "dismark_transport")

# Date and time formatting
dismark_df$survey_start <- as.POSIXlt(dismark_df$survey_start, format="%Y-%m-%d %H:%M:%S")

# Create new columns for date and time
dismark_df <- dismark_df %>%
  mutate(
    date = as.Date(survey_start),
    time = format(survey_start, "%H:%M:%S")
  ) %>%
  select(date, time, everything()) %>%
  select(-survey_start)

# Remove duplicate rows from dismark_df
dismark_df <- dismark_df[!duplicated(dismark_df[c("date", "time")]), ]

# Merging dismark_df with rpt_main_df
rpt_main_dsk_df <- merge(rpt_main_df, dismark_df, by = c("date", "time"), all.x = TRUE)

# dismark_route_label has the same information as othmarket_location
rpt_main_dsk_df <- rpt_main_dsk_df %>%
  mutate(
    othmarket_location = tools::toTitleCase(othmarket_location),
    othmarket_location = ifelse(!is.na(dismark_route_label), dismark_route_label, othmarket_location)
  )

# Arrange columns
rpt_main_dsk_df <- rpt_main_dsk_df %>%
  select(
    date, time, age, Gender, Province, District,items_sold_label, market,
    source_channel, source_location, produced_location, market_transport,
    othmarket_current, othmarket_location, dismark_transport
  )


colnames(rpt_main_dsk_df)
unique(rpt_main_dsk_df$items_sold_label)
unique(rpt_main_df$date)

# Write final data set(all OAF data) to CSV

write.csv(rpt_main_dsk_df, "OAF/megered_OAF_March2024.csv")

