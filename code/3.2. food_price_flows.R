# Load necessary libraries
library(sf)
library(dplyr)
library(ggplot2)

# Filter the data for the selected item (e.g., "Tomato")
selected_item <- "Tomato"
filtered_data <- final_data %>% filter(Matched_Food_Item == selected_item)

# Join avg_price_by_district with filtered_data to add geometry_district to avg_price_by_district
avg_price_by_district <- avg_price_by_district %>%
  left_join(filtered_data %>% select(District, geometry_district) %>% distinct(), by = "District")

# Create the plot
ggplot() +
  # Plot the districts using geometry_district
  geom_sf(data = filtered_data, aes(geometry = geometry_district), fill = "white", color = "black") +
  
  # Plot the source locations using geometry_source_location
  geom_sf(data = filtered_data, aes(geometry = geometry_source_location), color = "green", size = 2) +
  
  # Plot the flows from source locations to districts using geom_segment
  geom_segment(data = filtered_data, aes(x = st_coordinates(geometry_source_location)[,1], y = st_coordinates(geometry_source_location)[,2],
                                         xend = st_coordinates(geometry_district)[,1], yend = st_coordinates(geometry_district)[,2]),
               color = "blue", arrow = arrow(length = unit(0.1, "inches"))) +
  
  # Plot average prices as text on the map
  geom_sf_text(data = avg_price_by_district, aes(label = round(avg_price, 2), geometry = geometry_district), size = 3, color = "red") +
  
  # Add titles and customize the plot
  labs(title = paste("Flows and Average Prices of", selected_item, "in Rwanda")) +
  theme_minimal()
