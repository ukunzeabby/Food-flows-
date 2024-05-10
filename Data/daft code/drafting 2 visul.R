# Load libraries
library(sf)
library(dplyr)
library(ggplot2)

# Read the data
df <- read.csv("C:/Users/HP/Desktop/IITA/IITA 2024/LFSM/food_flows/Result/Final Data/Market_Monitoring_Round 7.csv")
rwanda_districts <- st_read("C:/Users/HP/Desktop/IITA/IITA 2024/LFSM/Health Diet Cost/Data/gadm41_RWA_shp (2)/gadm41_RWA_2.shp")

# Calculate centroids of districts
centroids_df <- st_centroid(rwanda_districts)

# Filter data
valid_districts <- unique(df$District)
final_df <- df[df$source_location %in% valid_districts, ]

# Perform the join based on District columns
joined_df_district <- merge(final_df, centroids_df, by.x = "District", by.y = "NAME_2", all.x = TRUE)

# Perform the join based on source_location columns
joined_df_source <- merge(final_df, centroids_df, by.x = "source_location", by.y = "NAME_2", all.x = TRUE)

# Add geometry columns for districts and source locations to joined_df
joined_df <- joined_df_district %>%
  mutate(geometry_district = geometry) %>%
  select(-geometry) %>%
  full_join(joined_df_source %>% 
              mutate(geometry_source_location = geometry) %>%
              select(-geometry), by = c("District", "source_location"))

grouped_df <- joined_df %>% select(date.x, time.x, Province.x, District, items_sold.x, source_channel.x, source_location, market_transport.x, geometry_district, geometry_source_location)
#unique(grouped_df$items_sold.x)
# Filter items_sold
grouped_df <- grouped_df %>%
  filter(items_sold.x == "potato")

# Filter data for points where geometry_source_location is the same as geometry_district
same_location <- grouped_df %>% filter(geometry_source_location == geometry_district)

# Filter data for lines where geometry_source_location is different from geometry_district
different_location <- grouped_df %>% filter(geometry_source_location != geometry_district)

# Plot the map
ggplot() +
  geom_sf(data = rwanda_districts, fill = "white", color = "black") +
  geom_point(data = same_location, aes(x = st_coordinates(geometry_district)[, 1], y = st_coordinates(geometry_district)[, 2]), color = "red", size = 2) +
  geom_point(data = different_location, aes(x = st_coordinates(geometry_source_location)[, 1], y = st_coordinates(geometry_source_location)[, 2]), color = "red", size = 2) +
  geom_segment(data = different_location, aes(x = st_coordinates(geometry_source_location)[, 1], y = st_coordinates(geometry_source_location)[, 2],
                                              xend = st_coordinates(geometry_district)[, 1], yend = st_coordinates(geometry_district)[, 2]), 
               arrow = arrow(length = unit(0.1, "inches")), color = "blue") +
  labs(title = "Flows of patato in Rwanda district ")
