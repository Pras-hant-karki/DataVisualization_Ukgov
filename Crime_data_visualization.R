library(tidyverse)
library(dplyr)
library(scales)
library(fmsb)
library(data.table) 

# Set working directory 
setwd("C:/Users/Hp/Desktop/DataScience_Prashant_Karki")

# Read crime data - we'll work directly with it since the postcode join doesn't work
crime_Data = read_csv("C:/Users/Hp/Desktop/DataScience_Prashant_Karki/Cleaned_datasets/cleaned_crimedata/Cleaned_Crime_Data.csv")

View(crime_Data)

# Clean up the Falls Within column to get region names
crimeData = crime_Data %>% 
  mutate(Region = case_when(
    `Falls Within` == "South Yorkshire Police" ~ "SOUTH YORKSHIRE",
    `Falls Within` == "West Yorkshire Police" ~ "WEST YORKSHIRE",
    TRUE ~ `Falls Within`
  )) %>%
  as_tibble()

# Filter data for available years (2022-2024)
filtered_data <- crimeData %>%
  filter(Region %in% c("SOUTH YORKSHIRE", "WEST YORKSHIRE") & Year %in% c(2022,2023,2024)) %>% 
  filter(CrimeType == "Drugs") %>% 
  as_tibble()

# Chart 1: Create boxplot - Show crime counts by region
ggplot(filtered_data, aes(x = Region, y = CrimeCount)) +
  geom_boxplot() +
  labs(title = "Boxplot of 'Drugs' Crimes (2022-2024)",
       y = "Crime Count") +
  theme_minimal()

# Chart 2: Radar Chart for Multiple Crime Types in 2023
radarData <- crimeData %>%
  filter(Region %in% c("SOUTH YORKSHIRE", "WEST YORKSHIRE") & Year == 2023) %>%
  group_by(Region, CrimeType) %>% 
  summarise(TotalCrime = sum(CrimeCount, na.rm = TRUE), .groups = 'drop') %>%
  group_by(CrimeType) %>%
  filter(sum(TotalCrime) > 0) %>%
  ungroup() %>%
  # Spread data for radar chart format
  pivot_wider(names_from = CrimeType, values_from = TotalCrime, values_fill = 0)
if(nrow(radarData) > 0 && ncol(radarData) > 1) {
  # Prepare data for fmsb radar chart - convert to proper data frame format
  radar_df <- radarData[, -1]  # Remove Region column
  rownames(radar_df) <- radarData$Region
  max_values <- apply(radar_df, 2, max, na.rm = TRUE)
  min_values <- rep(0, ncol(radar_df))
  # Create the final data frame with max/min rows at top
  radar_data_final <- rbind(max_values, min_values, radar_df)
  radar_data_final <- as.data.frame(radar_data_final)
  par(mfrow = c(1, 1))  # Ensure single plot
  
  # Create radar chart
  colors_border = c(rgb(0.2,0.5,0.5,0.9), rgb(0.8,0.2,0.5,0.9))
  colors_in = c(rgb(0.2,0.5,0.5,0.4), rgb(0.8,0.2,0.5,0.4))
  
  radarchart(radar_data_final, axistype=1,
             pcol=colors_border, pfcol=colors_in, plwd=4, plty=1,
             cglcol="black", cglty=1, axislabcol="black", 
             caxislabels=seq(0, max(max_values), length.out=5), 
             cglwd=0.8, vlcex=0.8,
             title="Crime Types by Region (2023)")
  
  # Add legend
  legend(x=0.7, y=1.3, legend = rownames(radar_df), bty = "n", pch=20, 
         col=colors_border, text.col = "black", cex=1.2, pt.cex=3)
  
} else {
  # Fallback to bar chart if radar data is insufficient
  VehicleCrime <- crimeData %>%
    filter(Region %in% c("SOUTH YORKSHIRE", "WEST YORKSHIRE") & Year == 2023 & CrimeType == "Vehicle crime") %>%
    group_by(Region) %>% 
    summarise(TotalVehicleCrime = sum(CrimeCount), .groups = 'drop') %>% 
    as_tibble()
  
  if(nrow(VehicleCrime) > 0) {
    ggplot(VehicleCrime, aes(x = Region, y = TotalVehicleCrime, fill = Region)) +
      geom_bar(stat = "identity") +
      labs(title = "Vehicle Crime by Region (2023)",
           y = "Total Crime Count") +
      theme_minimal() +
      theme(legend.position = "none")
  } else {
    print("No crime data found for 2023")
  }
}

# Chart 3: Pie Chart for Robbery in 2022-2024
Robbery_2022_2024 = crimeData %>%
  filter(Region %in% c("SOUTH YORKSHIRE", "WEST YORKSHIRE") & Year %in% c(2022,2023,2024)) %>% 
  filter(CrimeType == "Robbery") %>% 
  group_by(Region) %>%
  summarise(TotalCrimeCount = sum(CrimeCount, na.rm = TRUE), .groups = 'drop') %>% 
  as_tibble()

# Create a pie chart for Robbery by Region
if(nrow(Robbery_2022_2024) > 0) {
  ggplot(Robbery_2022_2024, aes(x = "", y = TotalCrimeCount, fill = Region)) +
    geom_bar(stat = "identity", width = 1) +
    coord_polar("y") +
    labs(title = "Robbery by Region (2022-2024)", fill = "Region") +
    theme_minimal() +
    theme(axis.text = element_blank(),
          axis.title = element_blank(),
          legend.position = "right")
} else {
  print("No Robbery data found")
}

# Chart 4: Line graph for Drug Offense Rate in 2022-2024
Drugs_2022_2024 = crimeData %>%
  filter(Region %in% c("SOUTH YORKSHIRE", "WEST YORKSHIRE") & Year %in% c(2022,2023,2024)) %>% 
  filter(CrimeType == "Drugs") %>% 
  group_by(Year, Region) %>%
  summarise(TotalCrimeCount = sum(CrimeCount, na.rm = TRUE), .groups = 'drop') %>% 
  as_tibble()

# Create a line chart
if(nrow(Drugs_2022_2024) > 0) {
  ggplot(Drugs_2022_2024, aes(x = Year, y = TotalCrimeCount, color = Region)) +
    geom_line(size = 1.2) +
    geom_point(size = 3) +
    labs(x = "Year", y = "Drug Offense Count", 
         title = "Drug Offense Count from 2022 to 2024", color = "Region") +
    theme_minimal()
} else {
  print("No Drug offense data found")
}