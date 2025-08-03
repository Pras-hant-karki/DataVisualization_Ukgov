# Load necessary libraries
library(tidyverse)
library(dplyr)
library(stringi)
library(scales)
# Set working directory 
setwd("C:/Users/Hp/Desktop/DataScience_Prashant_Karki")

# Read CSV files for different years
hp2021 = read_csv("C:/Users/Hp/Desktop/DataScience_Prashant_Karki/Downloaded_DataSets/housing/housing_2021.csv", show_col_types = FALSE)
hp2022 = read_csv("C:/Users/Hp/Desktop/DataScience_Prashant_Karki/Downloaded_DataSets/housing/housing_2022.csv", show_col_types = FALSE)
hp2023 = read_csv("C:/Users/Hp/Desktop/DataScience_Prashant_Karki/Downloaded_DataSets/housing/housing_2023.csv", show_col_types = FALSE)

# Assign new column names
colnames(hp2021) = colnames(hp2022) = colnames(hp2023) = c("ID", "Price", "Year", "PostCode", "PAON", "SAON", "FL", "House_Num", "Flat", "Street_Name", "Locality", "Town", "District", "Country", "Type1", "Type2")

# Combine datasets and clean data
HousePrices = bind_rows(hp2021, hp2022, hp2023) %>%
  na.omit() %>%      # Remove rows with missing values
  distinct() %>%     # Keep only distinct rows
  as_tibble()

# Write cleaned data to CSV
write.csv(HousePrices, "C:/Users/Hp/Desktop/DataScience_Prashant_Karki/Cleaned_datasets/combined_housingdataset/Combined_House_Pricing_2021-2023.csv", row.names = FALSE)

# Filter rows based on South and West Yorkshire Districts
FilteredHousePrices <- filter(HousePrices, 
                              District %in% c("SHEFFIELD", "BARNSLEY", "ROTHERHAM", "DONCASTER", 
                                              "LEEDS", "BRADFORD", "WAKEFIELD", "KIRKLEES", "CALDERDALE"))


# Define a regular expression pattern to match the space and everything after it
pattern = ' .*$'

# Clean and refine the filtered data
FilteredHousePrices <- FilteredHousePrices %>%
  mutate(shortPostcode = gsub(pattern, "", PostCode)) %>%
  mutate(Year = str_trim(substring(Year, 1, 4))) %>%
  select(PostCode, shortPostcode, Year, District, Town, Price, Country) %>%
  na.omit() %>%
  distinct() %>%
  as_tibble()

# View the filtered data
View(FilteredHousePrices)

# Export the filtered data to CSV
write.csv(FilteredHousePrices, "C:/Users/Hp/Desktop/DataScience_Prashant_Karki/Cleaned_datasets/cleaned_housingdataset/Cleaned_House_Pricing_2021-2023.csv", row.names = FALSE)
