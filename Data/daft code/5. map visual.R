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

# Perform the join based on source_location and District columns
joined_df <- merge(final_df, centroids_df, by.x = "District", by.y = "NAME_2", all.x = TRUE)

# Filter items_sold
joined_df <- joined_df %>%
  filter(items_sold == "onion")

# Separate data into same and different location dataframes
same_location_df <- joined_df[joined_df$source_location == joined_df$District, ]
different_location_df <- joined_df[joined_df$source_location != joined_df$District, ]

# Convert different location dataframe to sf object

different_location_sf <- st_as_sf(different_location_df)

# Convert centroids_df to sf object
centroids_sf <- st_as_sf(centroids_df)

# Spatial join to associate each source location with its corresponding district centroid
source_district_centroids <- st_join(different_location_sf, centroids_sf, join = st_contains)

# Plot
ggplot() +
  geom_sf(data = rwanda_districts, fill = "white", color = "black") +
  geom_segment(data = source_district_centroids, aes(x = st_coordinates(geometry)[, 1], y = st_coordinates(geometry)[, 2], 
                                                     xend = st_coordinates(geometry)[, 1], yend = st_coordinates(geometry)[, 2]), 
               arrow = arrow(length = unit(0.1, "inches")), color = "blue") +
  geom_point(data = centroids_sf, aes(x = st_coordinates(geometry)[, 1], y = st_coordinates(geometry)[, 2]), color = "red", size = 2) +
  labs(title = "Flow of Onions from Source Location to Districts")
