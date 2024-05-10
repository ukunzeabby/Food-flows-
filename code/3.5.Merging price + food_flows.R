library(dplyr)
library(stringr)
library(readr)
library(readxl)
library(sf)
library(dplyr)
library(ggplot2)
library(readr)
library(Hmisc)
library(stringr)

file_names <- paste0("C:/Users/HP/Desktop/IITA/IITA 2024/LFSM/food_flows/Result/Final Data/Market_Monitoring_Round ", 1:12, ".csv")

combined_data <- data.frame()

# Loop through each file and append its contents to the data frame
for (file_path in file_names) {
  # Read the CSV file
  file_data <- read.csv(file_path, header = TRUE)
  
  # Append the data to the combined data frame
  combined_data <- rbind(combined_data, file_data)
}

combined_data<- combined_data%>%
  mutate(source_location = str_replace(source_location, "Nyarungenge", "Nyarugenge"))

rwanda_districts <- st_read("C:/Users/HP/Desktop/IITA/IITA 2024/LFSM/Health Diet Cost/Data/gadm41_RWA_shp (2)/gadm41_RWA_2.shp")
great_lakes <- st_read("C:/Users/HP/Desktop/IITA/IITA 2024/LFSM/food_flows/Data/Great Lakes/Great Lakes/Great_Lakes_Shape.shp")

# Calculate centroids of Rwanda districts and great lakes regions
centroids_df <- st_centroid(rwanda_districts)
centroids_other_countries <- st_centroid(great_lakes)

# Perform the join based on District columns
joined_df_district <- merge(combined_data, centroids_df, by.x = "District", by.y = "NAME_2", all.x = TRUE)

# Perform the join based on source_location columns
joined_df_source <- merge(combined_data, centroids_df, by.x = "source_location", by.y = "NAME_2", all.x = TRUE)

# Perform the join based on country columns
joined_df_country <- merge(combined_data, centroids_other_countries, by.x = "source_location", by.y = "Country", all.x = TRUE)

# Define possible column names that represent country data
possible_country_columns <- c("NAME_0_1", "NAME_0_2", "NAME_0_1_1", "NAME_0_1_3")

# Function to find the first valid country column
find_valid_country_column <- function(df, column_names) {
  # Check each column name in the list of possible country columns
  for (col_name in column_names) {
    if (col_name %in% names(df)) {
      # Return the first valid column found
      return(col_name)
    }
  }
  # If no valid column found, return NULL
  return(NULL)
}

# Find the first valid country column name in joined_df_country
actual_country_column_name <- find_valid_country_column(joined_df_country, possible_country_columns)

# Check if a valid country column name was found
if (!is.null(actual_country_column_name)) {
  joined_df_country <- joined_df_country %>%
    distinct(source_location, .data[[actual_country_column_name]], .keep_all = TRUE)
} else {
  stop("No valid country column found in joined_df_country.")
}


# Add geometry columns for districts, source locations, and countries to the final joined data frame
joined_df <- full_join(
  joined_df_district %>%
    mutate(geometry_district = geometry) %>%
    select(-geometry),
  joined_df_source %>%
    mutate(geometry_source_location = geometry) %>%
    select(-geometry),
  by = c("District", "source_location")
)

# Combine the final dataframe with country data
joined_df <- full_join(
  joined_df,
  joined_df_country %>%
    mutate(geometry_country = geometry) %>%
    select(-geometry),
  by = c("source_location")
)

# Removing duplicate rows based on the first 9 columns
joined_df <-joined_df %>%
  distinct(across(1:9), .keep_all = TRUE)

# Select relevant columns 
grouped_df <- joined_df %>% select(date.x, time.x, Province.x,  District.x, items_sold_label.x,Food.Item...English.x ,source_channel.x, source_location, market_transport.x, geometry_district, geometry_source_location,geometry_country)

grouped_df <- grouped_df %>%
  mutate(
    geometry_source_location = if_else(
      st_is_empty(geometry_source_location),
      geometry_country,
      geometry_source_location
    )
  )
grouped_df <- grouped_df %>%
  select(-geometry_country)

food<-grouped_df%>%
  mutate(source_location = str_replace(source_location, "Nyarungenge", "Nyarugenge"))

# Ensure the date columns are in the same format
food$date <- as.Date(food$date.x, format = "%Y-%m-%d") 
data <- read_excel("C:/Users/HP/Desktop/IITA/IITA 2024/LFSM/Health Diet Cost/Data/esokodata/eSoko-2023_2024_data.xlsx")
colnames(data)<-c("Province", "District", "Market", "Commodity", "Price", "Date")
data$Price<-as.numeric(data$Price)


#####Adding date information to aggregate on
data<-data[!grepl("^44",data$Date),]###Removing random data with dates starting with 44....
data$Date<-as.Date(data$Date, "%m/%d/%Y")
data$year <- format(data$Date, "%y")
data$month<- format(data$Date, "%m")
data$Date_Update<-as.Date(data$Date, format="%m/%Y")
data <-data[order(data$Date_Update), ]
data$Date_Update_2<-format(data$Date_Update, "%m/%Y")
data$unique_month_value <- as.integer(factor(data$Date_Update_2, levels = unique(data$Date_Update_2)))


#####Updating commodity names to English and adding the food groups
commodity_translation<-read_xlsx("C:/Users/HP/Desktop/IITA/IITA 2024/LFSM/Health Diet Cost/Data/Food Items/Food_Items_Raw.xlsx")
data$Commodity_Eng <- commodity_translation$Standard[match(data$Commodity, commodity_translation$Rwa)]
data$Food_Group <- commodity_translation$Food_Group[match(data$Commodity_Eng, commodity_translation$Standard)]
data<-data[data$Food_Group!="Remove",]
data<-na.omit(data)

##Original egg data is price per egg. Assuming each egg is 50g, so multiplying by 2 to get price per kg, like other items
data[data$Commodity_Eng == "Egg", "Price"] <- data[data$Commodity_Eng == "Egg", "Price"] * 2
data[data$Commodity_Eng == "Egg_improved", "Price"] <- data[data$Commodity_Eng == "Egg_improved", "Price"] * 2
data<-data[data$Commodity_Eng!="Egg_improved",]

# Select specific columns from 'data'
data <- data %>%
  select( Date_Update,Province, District, Commodity, Commodity_Eng,Price)
# Standardize district
data$District <-str_to_title(data$District)

unique(data$Commodity_Eng)
unique(food$Food.Item...English.x)



# Create dictionaries for matching
commodity_dict <- unique(data$Commodity_Eng)
food_dict <- unique(food$Food.Item...English.x)

# Set similarity threshold
threshold <- 0.3  # Adjust this value based on your desired sensitivity

# Function to find the best match between a commodity and food item
find_best_match <- function(commodity, food_dict, threshold) {
  # Calculate string distances
  distances <- stringdist::stringdistmatrix(a = commodity, b = food_dict, method = "jw")
  # Find the minimum distance and its index
  min_distance <- min(distances)
  min_index <- which.min(distances)
  
  # Check if the minimum distance is within the threshold
  if (min_distance <= threshold) {
    return(food_dict[min_index])
  } else {
    return(NA)
  }
}

# Create a matching dictionary from Commodity_Eng to Food.Item...English
matching_dict <- data.frame(
  Commodity_Eng = commodity_dict,
  Matched_Food_Item = sapply(commodity_dict, find_best_match, food_dict = food_dict, threshold = threshold)
)

# Convert date.x to date type
food$date.x <- as.Date(food$date.x, format = "%Y-%m-%d")  # Adjust date format as needed

merged_data <- data %>%
  left_join(matching_dict, by = c("Commodity_Eng" = "Commodity_Eng")) %>%
  filter(!is.na(Matched_Food_Item)) %>%
  inner_join(food, by = c("Matched_Food_Item" = "Food.Item...English.x", "Date_Update" = "date.x", "District" = "District.x"))


# Filter out rows where Commodity_Eng is equal to "Cow_meat_bones"
merged_data <- merged_data %>%
  filter(Commodity_Eng != "Cow_meat_bones")

str(merged_data)
final_data<- merged_data %>%
  select(date,Province,District,source_location,items_sold_label.x,Matched_Food_Item,Price,source_channel.x,market_transport.x,geometry_district,geometry_source_location)

str(final_data)

# Filter the data for the selected item 
selected_item <- "Rice"
filtered_data <- final_data %>% filter(Matched_Food_Item == selected_item)
unique(final_data$District)
length(unique(final_data$District))
unique(rwanda_districts$NAME_2)
# Calculate the average price of the selected item for each district
avg_price_by_district <- filtered_data %>%
  group_by(District) %>%
  summarise(avg_price = mean(Price, na.rm = TRUE))

# Merge avg_price_by_district with the shapefile data for the districts to add geometry_district
rwanda_map_data <- merge(rwanda_districts, avg_price_by_district, by.x = "NAME_2", by.y = "District", all.x = TRUE)
# Filter data for points where geometry_source_location is the same as geometry_district
same_location <-filtered_data %>% filter(geometry_source_location == geometry_district)

# Filter data for lines where geometry_source_location is different from geometry_district
different_location <-filtered_data %>% filter(geometry_source_location != geometry_district)

# Create the plot
plot <- ggplot() +
  # Plot the districts using geometry_district and color fill based on avg_price
  geom_sf(data = rwanda_map_data, aes(geometry = geometry, fill = avg_price), color = "black") +
  
  geom_point(data = same_location, aes(x = st_coordinates(geometry_district)[, 1], y = st_coordinates(geometry_district)[, 2]), color = "green", size = 2) +
  geom_point(data = different_location, aes(x = st_coordinates(geometry_source_location)[, 1], y = st_coordinates(geometry_source_location)[, 2]), color = "green", size = 2) +
  geom_segment(data = different_location, aes(x = st_coordinates(geometry_source_location)[, 1], y = st_coordinates(geometry_source_location)[, 2],
                                              xend = st_coordinates(geometry_district)[, 1], yend = st_coordinates(geometry_district)[, 2]), 
               color = "red", arrow = arrow(length = unit(0.1, "inches")), na.rm = TRUE) +  
  
  scale_fill_gradient(low = "lightblue", high = "darkblue", name = "Average Price(Rwf)") +
  
  labs(title = paste("Flows and Average Prices of", selected_item, "in Rwanda")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


print(plot)


















