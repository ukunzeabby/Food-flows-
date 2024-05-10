library(sf)
library(dplyr)
library(ggplot2)
library(readr)
library(Hmisc)
library(stringr)


# Create a vector of file names
file_names <- paste0("C:/Users/HP/Desktop/IITA/IITA 2024/LFSM/food_flows/Result/Final Data/Market_Monitoring_Round ", 1:12, ".csv")

# Create an empty data frame to store the combined data
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
  # Reduce duplicate rows in joined_df_country by selecting unique combinations of
  # source_location and the actual country column name
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
write.csv(grouped_df,"food_flows_geo.csv")

grouped_df <- grouped_df %>%
  select(-geometry_country)


# Filter items_sold
grouped_df <- grouped_df %>%
  filter(Food.Item...English.x == "Rice")

# Filter data for points where geometry_source_location is the same as geometry_district
same_location <- grouped_df %>% filter(geometry_source_location == geometry_district)

# Filter data for lines where geometry_source_location is different from geometry_district
different_location <- grouped_df %>% filter(geometry_source_location != geometry_district)

# Plot the map
ggplot() +
  geom_sf(data = rwanda_districts, fill = "white", color = "black") +
  geom_point(data = same_location, aes(x = st_coordinates(geometry_district)[, 1], y = st_coordinates(geometry_district)[, 2]), color = "red", size = 2) +
  geom_point(data = different_location, aes(x = st_coordinates(geometry_source_location)[, 1], y = st_coordinates(geometry_source_location)[, 2]), color = "green", size = 2) +
  geom_segment(data = different_location, aes(x = st_coordinates(geometry_source_location)[, 1], y = st_coordinates(geometry_source_location)[, 2],
                                              xend = st_coordinates(geometry_district)[, 1], yend = st_coordinates(geometry_district)[, 2]), 
               arrow = arrow(length = unit(0.1, "inches")), color = "blue") +
  labs(title = "Flows of Maize in Rwanda  ") +
  theme_minimal() +
  scale_size_continuous(name="Flow Frequency")
