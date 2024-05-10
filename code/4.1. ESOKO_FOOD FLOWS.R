library(dplyr)
library(stringr)
library(readr)
library(readxl)

data<-read_excel("C:/Users/HP/Desktop/IITA/IITA 2024/LFSM/Health Diet Cost/Data/esokodata/eSoko-2023_2024_data.xlsx")
food<-read_csv("C:/Users/HP/Desktop/IITA/IITA 2024/LFSM/food_flows/Result/plots for all rounds/Food_flows_data.csv")

food<- food%>%
  mutate(source_location = str_replace(source_location, "Nyarungenge", "Nyarugenge"))

# Ensure the date columns are in the same format
food$date <- as.Date(food$date, format = "%Y-%m-%d") 
colnames(data)<-c("Province", "District", "Market", "Commodity", "Price", "Date")
data<-na.omit(Price)
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
unique(food$Food.Item...English)



# Create dictionaries for matching
commodity_dict <- unique(data$Commodity_Eng)
food_dict <- unique(food$Food.Item...English)

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

# Merge dataframes based on the matching dictionary and conditions of date and district
merged_data <- data %>%
  left_join(matching_dict, by = c("Commodity_Eng" = "Commodity_Eng")) %>%
  filter(!is.na(Matched_Food_Item)) %>%
  inner_join(food, by = c("Matched_Food_Item" = "Food.Item...English", "Date_Update" = "date", "District" = "District"))


# Filter out rows where Commodity_Eng is equal to "Cow_meat_bones"
merged_data <- merged_data %>%
  filter(Commodity_Eng != "Cow_meat_bones")

#str(merged_data)
final_data<- merged_data %>%
  select(Date_Update,Province.x,District,source_location,items_sold,Matched_Food_Item,Price,source_channel,market_transport )

str(final_data)







