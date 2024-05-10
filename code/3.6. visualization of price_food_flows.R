# Load necessary libraries
library(sf)
library(dplyr)
library(ggplot2)

# Filter the data for the selected item (e.g., "Tomato")
selected_item <- "Tomato"
filtered_data <- final_data %>% filter(Matched_Food_Item == selected_item)

# Calculate the average price of the selected item for each district
avg_price_by_district <- filtered_data %>%
  group_by(District) %>%
  summarise(avg_price = mean(Price, na.rm = TRUE))

# Merge avg_price_by_district with the shapefile data for the districts to add geometry_district
rwanda_map_data <- merge(rwanda_shapefile, avg_price_by_district, by.x = "NAME_2", by.y = "District", all.x = TRUE)

# Create the plot
plot <- ggplot() +
  # Plot the districts using geometry_district and color fill based on avg_price
  geom_sf(data = rwanda_map_data, aes(geometry = geometry, fill = avg_price), color = "black") + # Black color for district borders
  
  # Plot the source locations using geometry_source_location
  geom_sf(data = filtered_data, aes(geometry = geometry_source_location), color = "green", size = 2) +
  
  # Plot the flows from source locations to districts using geom_segment
  geom_segment(data = filtered_data, aes(x = st_coordinates(geometry_source_location)[,1], y = st_coordinates(geometry_source_location)[,2],
                                         xend = st_coordinates(geometry_district)[,1], yend = st_coordinates(geometry_district)[,2]),
               color = "red", arrow = arrow(length = unit(0.1, "inches")), na.rm = TRUE) +  # na.rm to remove missing values
  
  # Add color bar for avg_price for fill
  scale_fill_gradient(low = "lightblue", high = "darkblue", name = "Average Price") +
  
  # Add titles and customize the plot
  labs(title = paste("Flows and Average Prices of", selected_item, "in Rwanda")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Print the plot
print(plot)

