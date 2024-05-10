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



##################################################################################################
#######################################Combining OAF + VIAMO data####################################
#####################################################################################################

# Filter data for the specified months
data <- rpt_main_dsk_df %>%
  filter(format(date, "%Y-%m") %in% c("2023-09", "2023-09"))

# Output unique dates and column names
unique(data$date)
colnames(data)

# Select relevant columns for further analysis
data <- data %>%
  select("date", "time", "Province", "District", "items_sold_label", "source_channel","source_location", "market_transport")

# Output column names
colnames(data)

# Read additional data from an Excel file
df <- read_excel("VIAMO/RW IITA USAID Market Monitoring Round 1.xlsx")
colnames(df)
# Remove rows with specific delivery status
df <- df[!(df$`Delivery Status` %in% c("Finished (incomplete)", "Failed (error)")), ]

# sOURCE _LOCATION 
df <- df %>%
  mutate(
    source_location = case_when(
      is.na(`From where did you source the food item? (Multiple Choice Question)`) & !is.na(`District Item Is Sold From (Expression Result) - Raw result of block`) ~ `District Item Is Sold From (Expression Result) - Raw result of block`,
      `From where did you source the food item? (Multiple Choice Question)` == "District" ~ `District Item Is Sold From (Expression Result) - Raw result of block`,
      `From where did you source the food item? (Multiple Choice Question)` == "Other District" ~ `From where did you source the food item? (Multiple Choice Question)`,
      `From where did you source the food item? (Multiple Choice Question)` == "International" & !is.na(`Which country did you source the food item? ( write the country name (Open-ended question) - Text`) ~ `Which country did you source the food item? ( write the country name (Open-ended question) - Text`,
      !is.na(`District Item Is Sold From (Expression Result) - Raw result of block`) ~ `District Item Is Sold From (Expression Result) - Raw result of block`,
      `From where did you source the food item? (Multiple Choice Question)` == "International" & 
        (`From whom did you source the food item? (Multiple Choice Question)` %in% c("Neighbour farm", "Own a farm")) ~ `From where did you source the food item? (Multiple Choice Question)`,
      !is.na(`From where did you source the food item? (Multiple Choice Question)`) ~ `From where did you source the food item? (Multiple Choice Question)`,
      #`From where did you source the food item? (Multiple Choice Question)` == "International" ~ `Which country did you source the food item? ( write the country name (Open-ended question) - Text`,
      `From where did you source the food item? (Multiple Choice Question)` == "Don't know" ~ "Don't know",
      TRUE ~ NA_character_
    )
  )
unique(df$`Which country did you source the food item? ( write the country name (Open-ended question) - Text`)
# filtering rows 
df <- df %>%
  filter(!(`From where did you source the food item? (Multiple Choice Question)` == "International" &
             (`Which country did you source the food item? ( write the country name (Open-ended question) - Text` %in% c("Murwanda","murwanda","2","KAMONYI","cyu rwanda",  "Urwanda", "Mu Rwanda","MURWANDA","1","1.0"))))

unique(df$`From whom did you source the food item? (Multiple Choice Question)`)

# Date and time formatting
df$Started <- as.POSIXlt(df$Started, format="%Y-%m-%d %H:%M:%S")

# Create new columns for date and time
df <- df %>%
  mutate(
    date = as.Date(Started),
    time = format(Started, "%H:%M:%S")
  ) %>%
  select(date, time, everything(), -c(Started))


# Filter data for the specified months
df <- df %>%
  filter(format(as.Date(date), "%Y-%m") %in% c("2023-09", "2023-09"))

# Output unique dates and column names
unique(df$date)
colnames(df)

# Select relevant columns from the additional data
df <- df %>%
  select(
    "date",
    "time",
    "In which province are you selling? (Multiple Choice Question)",
    "District Item Is Sold From (Expression Result) - Raw result of block",
    "Food Item - Kinyarwanda",
    "From whom did you source the food item? (Multiple Choice Question)",
    "source_location",
    "How did you transport the food to market? (Multiple Choice Question)",
    "Food Item - English"
  )


### Naming changing ######
# Standardize Food Item
data$items_sold_label <- tolower(data$items_sold_label)  # Convert to lowercase
data$items_sold_label <- gsub("\\s", "", data$items_sold_label)  # Remove spaces
data$items_sold_label <- gsub(",", ",", data$items_sold_label)  # Ensure commas as separators

# Sort the items before translation
sorted_items <- sort(unique(data$items_sold_label))
# Define a translation dictionary in alphabetical order
translation_dict <- c(
  "amafi" = "Fish",
  "amagi" = "Eggs",
  "amapera" = "Guava",
  "amasaka" = "Sorghum",
  "amashaza" = "Green Peas",
  "amashu" = "Cabbage",
  "amatay`inka" = "milk",
  "amateke" = "Taro",
  "amatunda" = "Passionfruit",
  "beterave" = "Beetroot",
  "biringanya" = "Aubergine",
  "dodo" = "Amaranth Leaves",
  "ibigori" = "Maize",
  "ibihazabitobyumuhondo" = "Pumpkin",
  "ibihazaimyungu" = "Pumpkin",
  "ibijumbaby`umuhondo" = "Sweet Potatoes Orange",
  "ibijumbaby`umweru" = "Sweet Potatoes White",
  "ibinyomoro" = "Tree Tomato",
  "ibirayi" = "Potato",
  "ibisheke" = "Sugarcane",
  "ibishyimbo" = "Field Beans",
  "ibitoki(by`imineke" = "Banana Beer",
  "ibitoki(by`inzoga)" = "Banana Beer",
  "ibitoki(byoguteka)" = "Banana Cooking",
  "ibitunguru" = "Onion",
  "imiteja" = "Faba Beans",
  "imyumbati" = "Cassava Root",
  "inanasi" = "Pineapple",
  "ingano" = "Barley",
  "inyamaz`ihene" = "Goat Meat",
  "inyamaz`inka" = "Cow Meat",
  "inyanya" = "Tomato",
  "ipapayi" = "Papaya",
  "isombe" = "Cassava Leaves",
  "karoti" = "Carrot",
  "soya" = "Soya Beans",
  "sunflower" = "Sunflower",
  "ubunyobwa" = "Ground Nuts",
  "uburo" = "Millet",
  "umuceri" = "Rice",
  "urusenda" = "Chilli",
  "voka" = "Avocado"
)

# Add a new column with translated items to your data frame
data$`Food Item - English` <- translation_dict[data$items_sold_label]




# Rename columns to match the original data
colnames(df) <- colnames(data)

# Merge the result with main_df based on survey_start
final_df <- bind_rows(data, df)

# Arrange final_df based on date
final_df <- final_df %>%
  arrange(as.Date(date))
unique(final_df$market_transport)
# Standardize market_transport
final_df$market_transport <- tolower(final_df$market_transport)  # Convert to lowercase
final_df$market_transport <- gsub("\\s", "", final_df$market_transport)  # Remove spaces
final_df$market_transport <- gsub(",", ",", final_df$market_transport)  # Ensure commas as separators


unique(final_df$Province)
# Standardize Province
final_df$Province <- sub("\\s*Sourhern\\s*", "Southern", final_df$Province)
final_df$Province <- sub("\\s*Nothern\\s*", "Northern", final_df$Province)
final_df$Province <- tools::toTitleCase(final_df$Province)


unique(final_df$District)
# Standardize district
final_df$District <- tools::toTitleCase(final_df$District)
final_df$District <- gsub("\\s", "_", final_df$District)

unique(final_df$source_channel)
# Standardize source_channel values
final_df$source_channel <- tolower(final_df$source_channel)
final_df$source_channel <- gsub("\\s", "_", final_df$source_channel)
final_df$source_channel <- sub("\\s*own_a_farm\\s*", "own_farm", final_df$source_channel)


unique(final_df$source_location)
# Standardize Source location
final_df$source_location <- tools::toTitleCase(final_df$source_location) 
final_df$source_location <- sub("\\s*UGANDA\\s*", "Uganda", final_df$source_location)
final_df$source_location <- sub("\\s*Tanzaniya\\s*", "Tanzania", final_df$source_location)
final_df$source_location <- sub("\\s*TANZANIA\\s*", "Tanzania", final_df$source_location)
final_df$source_location <- sub("\\s*NotKnow\\s*", "Don't know", final_df$source_location)
final_df$source_location <- sub("\\s*Kongo\\s*", "DRCongo", final_df$source_location)
final_df$source_location <- sub("\\s*Congo\\s*", "DRCongo", final_df$source_location)
final_df$source_location <- sub("\\s*muri kongo\\s*", "DRCongo", final_df$source_location)
final_df$source_location <- sub("\\s*Other DistrictDistrict\\s*", "Other District", final_df$source_location)
final_df$source_location <- sub("\\s*Other\\s*", "Other District", final_df$source_location)
final_df$source_location <- sub("\\s*DRDRCongo\\s*", "DRCongo", final_df$source_location)
final_df$source_location <- sub("\\s*DRDRCongo\\s*", "DRCongo", final_df$source_location)

# Standardize items_sold
final_df$items_sold_label <- trimws(tolower(final_df$items_sold_label))
# Rearrange columns
final_df <- final_df %>%
  relocate("Food Item - English", .after = "items_sold_label")
final_df<- na.omit(final_df)
# Write the final dataset(combination of VIAM +OAF DATA) to CSV
write.csv(final_df, "C:/Users/HP/Desktop/IITA/IITA 2024/LFSM/food_flows/Result/Final Data/Market_Monitoring_Round 2.csv")

colnames(final_df)











