library(sf)
library(dplyr)
library(ggplot2)

# Read the CSV file
df <- read.csv("C:/Users/HP/Desktop/IITA/IITA 2024/LFSM/food_flows/Result/Final Data/Market_Monitoring_Round 7.csv")

# Read the shapefile of Rwanda districts
rwanda_districts <- st_read("C:/Users/HP/Desktop/IITA/IITA 2024/LFSM/Health Diet Cost/Data/gadm41_RWA_shp (2)/gadm41_RWA_2.shp")

# Calculate centroids of districts
centroids <- st_centroid(rwanda_districts)
centroids_df <- st_as_sf(centroids)

# Filter the valid districts
valid_districts <- unique(df$District)
final_df <- df[df$source_location %in% valid_districts, ]

# Perform the join based on source_location and District columns
joined_df <- merge(final_df, centroids_df, by.x = "District", by.y = "NAME_2", all.x = TRUE)

# Select relevant columns
joined_df <- joined_df %>%
  select(date, time, Province, District, geometry, items_sold, source_channel, source_location, market_transport)

# Filter for specific items_sold
joined_df <- joined_df %>%
  filter(items_sold == "onion")

# Filter rows where source_location is the same as the district
same_location_df <- joined_df[joined_df$source_location == joined_df$District, ]

# Filter rows where source_location is different from the district
different_location_df <- joined_df[joined_df$source_location != joined_df$District, ]

# Plot the map
ggplot() +
  geom_sf(data = rwanda_districts, fill = "lightgrey", color = "black") +
  geom_point(data = same_location_df, aes(x = st_coordinates(geometry)[, 1], y = st_coordinates(geometry)[, 2]), color = "red", size = 2) +
  geom_segment(data = different_location_df, aes(x = st_coordinates(geometry)[, 1], y = st_coordinates(geometry)[, 2], xend = st_coordinates(geometry)[, 1], yend = st_coordinates(geometry)[, 2]), color = "blue") +
  labs(title = "Flow of Onions from Source Location to Districts")
