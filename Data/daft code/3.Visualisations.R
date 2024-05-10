# Load required libraries
library(ggplot2)
library(dplyr)
library(sf)
library(readr)
library(sf)
library(Hmisc)

# Create a vector of file names
file_names <- paste0("C:/Users/HP/Desktop/IITA/IITA 2024/LFSM/food_flows/Result/Final Data/Market_Monitoring_Round ", 1:8, ".csv")

# Create an empty data frame to store the combined data
combined_data <- data.frame()

# Loop through each file and append its contents to the data frame
for (file_path in file_names) {
  # Read the CSV file
  file_data <- read.csv(file_path, header = TRUE)
  
  # Append the data to the combined data frame
  combined_data <- rbind(combined_data, file_data)
}

# Standardize market_transport
combined_data$market_transport <- tolower(combined_data$market_transport)  # Convert to lowercase
combined_data$market_transport <- gsub("\\s", "", combined_data$market_transport)  # Remove spaces
combined_data$market_transport <- gsub(",", ",", combined_data$market_transport)  # Ensure commas as separators

# Standardize Province
combined_data$Province <- sub("\\s*Sourhern\\s*", "Southern", combined_data$Province)
combined_data$Province <- sub("\\s*Nothern\\s*", "Northern", combined_data$Province)
combined_data$Province <- tools::toTitleCase(combined_data$Province)

# Standardize district
combined_data$District <- tools::toTitleCase(combined_data$District)
combined_data$District <- gsub("\\s", "_", combined_data$District)

# Standardize source_channel values
combined_data$source_channel <- tolower(combined_data$source_channel)
combined_data$source_channel <- gsub("\\s", "_", combined_data$source_channel)
combined_data$source_channel <- sub("\\s*own_a_farm\\s*", "own_farm", combined_data$source_channel)

# Standardize items_sold
combined_data$items_sold <- trimws(tolower(combined_data$items_sold))  # Convert to lowercase and remove leading/trailing whitespaces
combined_data <- na.omit(combined_data)
# Check unique values
unique_items_sold <- unique(combined_data$items_sold)

# Specify the output file path
output_file_path <- "C:/Users/HP/Desktop/IITA/IITA 2024/LFSM/food_flows/Result/plots for all rounds/combined_output_foodflows.csv"

# Write the combined data to the output CSV file
write.csv(combined_data, file = output_file_path, row.names = FALSE)

# Plotting market transport mode used
transport_counts <- combined_data %>%
  count(market_transport) %>%
  replace(is.na(.), "Unknown")

# Output folder path
output_folder <- "C:/Users/HP/Desktop/IITA/IITA 2024/LFSM/food_flows/Result/plots for all rounds/"
# Remove NA values from the data
combined_data_cleaned <- na.omit(combined_data)

# Plot 1: Frequency of Market Transport Modes
transport_counts_plot <- ggplot(transport_counts, aes(x = market_transport, y = n)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = n), vjust = -0.5, size = 3) +
  ggtitle("Frequency of Market Transport Modes") +
  xlab("Market Transport Mode") +
  ylab("Frequency") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Save Plot 1
plot_output_path_1 <- file.path(output_folder, "transport_counts_plot.png")
ggsave(plot_output_path_1, transport_counts_plot)
print(paste("Plot 1 saved to:", plot_output_path_1))


# Count market transport modes by province
market_transport_by_province_count <- combined_data_cleaned %>%
  count(market_transport, Province, name = "Count")

# Plot 2: Market Transport Modes by Province
market_transport_by_province_plot <- ggplot(market_transport_by_province_count, aes(x = market_transport, y = Count, fill = Province)) +
  geom_bar(stat = "identity", position = "stack") +
  ggtitle("Market Transport Modes by Province") +
  xlab("Market Transport Mode") +
  ylab("Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~ Province, scales = "free_y", ncol = 2)  +
  ylim(0, 1250) 
# Save Plot 2 for each province
plot_output_path_2 <- file.path(output_folder, "market_transport_by_province_plot.png")
ggsave(plot_output_path_2, market_transport_by_province_plot)
print(paste("Plot 2 saved to:", plot_output_path_2))


source_channel_plot <- ggplot(combined_data, aes(x = source_channel, fill = source_channel)) +
  geom_bar() +
  ggtitle("Source Channel Distribution") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

#item sold

items_sold_counts <- combined_data_cleaned %>%
  count(items_sold) 
print(items_sold_counts)
# Find the top 5 items sold
top_items_sold <- items_sold_counts %>%
  arrange(desc(n)) %>%
  slice_head(n = 5)

# Plot 4: Top 5 Items Sold
top_items_sold_plot <- ggplot(top_items_sold, aes(x = items_sold, y = n)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = n), vjust = -0.5, size = 3) +
  ggtitle("Top 5 Items Sold") +
  xlab("Items Sold") +
  ylab("Frequency") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Save Plot 4
plot_output_path_4 <- file.path(output_folder, "top_items_sold_plot.png")
ggsave(plot_output_path_4, top_items_sold_plot)
print(paste("Plot 4 saved to:", plot_output_path_4))
