# Libraries
library(tidyverse)
library(dplyr)
library(stringi)
library(scales)

# Set working directory 
setwd("C:/Users/Hp/Desktop/DataScience_Prashant_Karki")

# Read data
uncleanedhouseprices <- read_csv('C:/Users/Hp/Desktop/DataScience_Prashant_Karki/Cleaned_datasets/combined_housingdataset/Combined_House_Pricing_2021-2023.csv')
Population = read_csv("C:/Users/Hp/Desktop/DataScience_Prashant_Karki/Downloaded_DataSets/population/Population.csv", show_col_types = FALSE)

# Define districts for South and West Yorkshire
south_yorkshire_districts <- c("Sheffield", "Barnsley", "Rotherham", "Doncaster")
west_yorkshire_districts <- c("Leeds", "Bradford", "Wakefield", "Kirklees", "Calderdale")

# Define regex pattern
pattern = ' .*$'

# Calculate population estimates for each year from 2011 to 2023
Population = Population %>%
  mutate(shortPostcode = gsub(pattern, "", Postcode)) %>%
  group_by(shortPostcode) %>%
  summarise_at(vars(Population), list(Population2011 = sum)) %>%
  mutate(
    Population2012 = 1.00695353132322269 * Population2011,
    Population2013 = 1.00669740535540783 * Population2012,
    Population2014 = 1.00736463978721671 * Population2013,
    Population2015 = 1.00792367505802859 * Population2014,
    Population2016 = 1.00757874492811929 * Population2015,
    Population2017 = 1.00679374473924223 * Population2016,
    Population2018 = 1.00605929132212552 * Population2017,
    Population2019 = 1.00561255390388033 * Population2018,
    Population2020 = 1.00561255390388033 * Population2019,
    Population2021 = 1.00561255390388033 * Population2020,
    Population2022 = 1.00561255390388033 * Population2021,
    Population2023 = 1.00561255390388033 * Population2022
  ) %>%
  select(shortPostcode, Population2021, Population2022, Population2023)

# Clean and join house prices data with population data
FilteredHousePrices <- FilteredHousePrices %>%
  mutate(shortPostcode = gsub(pattern, "", PostCode)) %>%
  mutate(Year = str_trim(substring(Year, 1, 4))) %>%
  left_join(Population, by = "shortPostcode") %>%
  select(PostCode, shortPostcode, Year, Town, District, Country, Population2021, Population2022, Population2023) %>%
  group_by(shortPostcode) %>%
  arrange(Country) %>%
  as_tibble() %>%
  na.omit() %>%
  distinct()

# View the cleaned data
View(FilteredHousePrices)

# Write cleaned data to CSV
write.csv(FilteredHousePrices, "C:/Users/Hp/Desktop/DataScience_Prashant_Karki/Cleaned_datasets/cleaned_populationdata/Cleaned_Population.csv", row.names = FALSE)

