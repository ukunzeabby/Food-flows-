library(dplyr)
library(stringr)
library(readxl)
library(dplyr)



# Set working directory
setwd("C:/Users/HP/Desktop/IITA/IITA 2024/LFSM/food_flows/Data/")

##################################################################################################
#######################################Combining OAF + VIAMO data####################################
#####################################################################################################

data<-read.csv("OAF/megered_OAF_March2024.csv")

# Filter data for the specified months
data$date <- as.Date(data$date)

data <- data %>%
  filter(format(date, "%Y-%m") %in% c("2024-03", "2024-03"))

# Output unique dates and column names
unique(data$date)
colnames(data)

# Select relevant columns for further analysis
data <- data %>%
  select("date", "time", "Province", "District", "items_sold_label", "source_channel","source_location", "market_transport")

# Output column names
colnames(data)

# Read additional data from an Excel file
df <- read_excel("VIAMO/RW IITA USAID Market Monitoring Round 5_26 March 2024.xlsx")
colnames(df)
# Remove rows with specific delivery status
df <- df[!(df$`Delivery Status` %in% c("Finished (incomplete)", "Failed (error)")), ]

# sOURCE _LOCATION 
df <- df %>%
  mutate(
    source_location = case_when(
      is.na(`From where did you source the food item? (Multiple Choice Question)`) & !is.na(`District_MM_source food other`) ~ `District_MM_source food other`,
      is.na(`From where did you source the food item? (Multiple Choice Question)`) & !is.na(`District Item Is Sold From (Expression Result) - Raw result of block`) ~ `District Item Is Sold From (Expression Result) - Raw result of block`,
      `From where did you source the food item? (Multiple Choice Question)` == "District" ~ `District Item Is Sold From (Expression Result) - Raw result of block`,
      `From where did you source the food item? (Multiple Choice Question)` == "Other District" ~ `District_MM_source food other`,
      `From where did you source the food item? (Multiple Choice Question)` == "International" & !is.na(`Which country did you source the food item? ( write the country name (Open-ended question) - Text`) ~ `Which country did you source the food item? ( write the country name (Open-ended question) - Text`,
      !is.na(`District_MM_source food other`) ~ `District_MM_source food other`,
      `From where did you source the food item? (Multiple Choice Question)` == "International" & 
        (`From whom did you source the food item? (Multiple Choice Question)` %in% c("Neighbour farm", "Own a farm")) ~ `District_MM_source food other`,
      !is.na(`District_MM_source food other`) ~ `District_MM_source food other`,
      #`From where did you source the food item? (Multiple Choice Question)` == "International" ~ `Which country did you source the food item? ( write the country name (Open-ended question) - Text`,
      `From where did you source the food item? (Multiple Choice Question)` == "Don't know" ~ "Don't know",
      TRUE ~ NA_character_
    )
  )
unique(df$`Which country did you source the food item? ( write the country name (Open-ended question) - Text`)
# filtering rows 
df <- df %>%
  filter(!(`From where did you source the food item? (Multiple Choice Question)` == "International" &
             (`Which country did you source the food item? ( write the country name (Open-ended question) - Text` %in% c("Nibeshye","Rwanda","MUHANGA","5.0","MURWNDA","cyu rwanda", "Kogokamanyura","RWANDA","MURWANDA","1","1.0","Murwnda","Kenya\ntanzaniya","Murwanda", "Murwada","RWAND"))))

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
#df <- df %>%
  #filter(format(as.Date(date), "%Y-%m") %in% c("2024-01", "2024-01"))

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

#### Naming changing ######
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

# Standardize market_transport
final_df$market_transport <- tolower(final_df$market_transport)  # Convert to lowercase
final_df$market_transport <- gsub("\\s", "", final_df$market_transport)  # Remove spaces
final_df$market_transport <- gsub(",", ",", final_df$market_transport)  # Ensure commas as separators
unique(final_df$market_transport)

unique(final_df$Province)
# Standardize Province
final_df$Province <- sub("\\s*Sourhern\\s*", "Southern", final_df$Province)
final_df$Province <- sub("\\s*Nothern\\s*", "Northern", final_df$Province)
final_df$Province <- tools::toTitleCase(final_df$Province)
unique(final_df$Province)

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
final_df$source_location <- sub("\\s*Muzambike\\s*", "Mozambique", final_df$source_location)
final_df$source_location <- sub("\\s*Tanzaniya\\s*", "Tanzania", final_df$source_location)
final_df$source_location <- sub("\\s*TANZANIA\\s*", "Tanzania", final_df$source_location)
final_df$source_location <- sub("\\s*NotKnow\\s*", "Don't know", final_df$source_location)
final_df$source_location <- sub("\\s*Kongo\\s*", "DRCongo", final_df$source_location)
final_df$source_location <- sub("\\s*Congo\\s*", "DRCongo", final_df$source_location)
final_df$source_location <- sub("\\s*muri kongo\\s*", "DRCongo", final_df$source_location)
final_df$source_location <- sub("\\s*Umuhinde\\s*", "India", final_df$source_location)
final_df$source_location <- sub("\\s*umuhinde\\s*", "India", final_df$source_location)
final_df$source_location <- sub("\\s*MuriDRDRDRCongo\\s*", "DRCongo", final_df$source_location)
final_df$source_location <- sub("\\s*DRDRCongo\\s*", "DRCongo", final_df$source_location)
final_df$source_location <- sub("\\s*Don't know\\s*", "Don't Know", final_df$source_location)
unique(final_df$`Food Item - English`)
# Standardize items_sold
final_df$items_sold_label <- trimws(tolower(final_df$items_sold_label))
unique(final_df$date)


# Rearrange columns
final_df <- final_df %>%
  relocate("Food Item - English", .after = "items_sold_label")

final_df<- na.omit(final_df)
# Write the final dataset(combination of VIAM +OAF DATA) to CSV
write.csv(final_df, "C:/Users/HP/Desktop/IITA/IITA 2024/LFSM/food_flows/Result/Final Data/Market_Monitoring_Round 12.csv")

