library(data.table)
library(tidyverse)
library(stringr)
library(lubridate)

# Set working directory 
setwd("C:/Users/Hp/Desktop/DataScience_Prashant_Karki")
# Importing CSV files

#south-yorkshire-street
Crime_ds_2022_06_south_yorkshire  = read_csv("C:/Users/Hp/Desktop/DataScience_Prashant_Karki/Downloaded_DataSets/crime_ds/2022/2022-06/2022-06-south-yorkshire-street.csv")
Crime_ds_2022_07_south_yorkshire  = read_csv("C:/Users/Hp/Desktop/DataScience_Prashant_Karki/Downloaded_DataSets/crime_ds/2022/2022-07/2022-07-south-yorkshire-street.csv")
Crime_ds_2022_08_south_yorkshire  = read_csv("C:/Users/Hp/Desktop/DataScience_Prashant_Karki/Downloaded_DataSets/crime_ds/2022/2022-08/2022-08-south-yorkshire-street.csv")
Crime_ds_2022_09_south_yorkshire  = read_csv("C:/Users/Hp/Desktop/DataScience_Prashant_Karki/Downloaded_DataSets/crime_ds/2022/2022-09/2022-09-south-yorkshire-street.csv")
Crime_ds_2022_10_south_yorkshire  = read_csv("C:/Users/Hp/Desktop/DataScience_Prashant_Karki/Downloaded_DataSets/crime_ds/2022/2022-10/2022-10-south-yorkshire-street.csv")
Crime_ds_2022_11_south_yorkshire  = read_csv("C:/Users/Hp/Desktop/DataScience_Prashant_Karki/Downloaded_DataSets/crime_ds/2022/2022-11/2022-11-south-yorkshire-street.csv")
Crime_ds_2022_12_south_yorkshire  = read_csv("C:/Users/Hp/Desktop/DataScience_Prashant_Karki/Downloaded_DataSets/crime_ds/2022/2022-12/2022-12-south-yorkshire-street.csv")

Crime_ds_2023_01_south_yorkshire  = read_csv("C:/Users/Hp/Desktop/DataScience_Prashant_Karki/Downloaded_DataSets/crime_ds/2023/2023-01/2023-01-south-yorkshire-street.csv")
Crime_ds_2023_02_south_yorkshire  = read_csv("C:/Users/Hp/Desktop/DataScience_Prashant_Karki/Downloaded_DataSets/crime_ds/2023/2023-02/2023-02-south-yorkshire-street.csv")
Crime_ds_2023_03_south_yorkshire  = read_csv("C:/Users/Hp/Desktop/DataScience_Prashant_Karki/Downloaded_DataSets/crime_ds/2023/2023-03/2023-03-south-yorkshire-street.csv")
Crime_ds_2023_04_south_yorkshire  = read_csv("C:/Users/Hp/Desktop/DataScience_Prashant_Karki/Downloaded_DataSets/crime_ds/2023/2023-04/2023-04-south-yorkshire-street.csv")
Crime_ds_2023_05_south_yorkshire  = read_csv("C:/Users/Hp/Desktop/DataScience_Prashant_Karki/Downloaded_DataSets/crime_ds/2023/2023-05/2023-05-south-yorkshire-street.csv")
Crime_ds_2023_06_south_yorkshire  = read_csv("C:/Users/Hp/Desktop/DataScience_Prashant_Karki/Downloaded_DataSets/crime_ds/2023/2023-06/2023-06-south-yorkshire-street.csv")
Crime_ds_2023_07_south_yorkshire  = read_csv("C:/Users/Hp/Desktop/DataScience_Prashant_Karki/Downloaded_DataSets/crime_ds/2023/2023-07/2023-07-south-yorkshire-street.csv")
Crime_ds_2023_08_south_yorkshire  = read_csv("C:/Users/Hp/Desktop/DataScience_Prashant_Karki/Downloaded_DataSets/crime_ds/2023/2023-08/2023-08-south-yorkshire-street.csv")
Crime_ds_2023_09_south_yorkshire  = read_csv("C:/Users/Hp/Desktop/DataScience_Prashant_Karki/Downloaded_DataSets/crime_ds/2023/2023-09/2023-09-south-yorkshire-street.csv")
Crime_ds_2023_10_south_yorkshire  = read_csv("C:/Users/Hp/Desktop/DataScience_Prashant_Karki/Downloaded_DataSets/crime_ds/2023/2023-10/2023-10-south-yorkshire-street.csv")
Crime_ds_2023_11_south_yorkshire  = read_csv("C:/Users/Hp/Desktop/DataScience_Prashant_Karki/Downloaded_DataSets/crime_ds/2023/2023-11/2023-11-south-yorkshire-street.csv")
Crime_ds_2023_12_south_yorkshire  = read_csv("C:/Users/Hp/Desktop/DataScience_Prashant_Karki/Downloaded_DataSets/crime_ds/2023/2023-12/2023-12-south-yorkshire-street.csv")

Crime_ds_2024_01_south_yorkshire  = read_csv("C:/Users/Hp/Desktop/DataScience_Prashant_Karki/Downloaded_DataSets/crime_ds/2024/2024-01/2024-01-south-yorkshire-street.csv")
Crime_ds_2024_02_south_yorkshire  = read_csv("C:/Users/Hp/Desktop/DataScience_Prashant_Karki/Downloaded_DataSets/crime_ds/2024/2024-02/2024-02-south-yorkshire-street.csv")
Crime_ds_2024_03_south_yorkshire  = read_csv("C:/Users/Hp/Desktop/DataScience_Prashant_Karki/Downloaded_DataSets/crime_ds/2024/2024-03/2024-03-south-yorkshire-street.csv")
Crime_ds_2024_04_south_yorkshire  = read_csv("C:/Users/Hp/Desktop/DataScience_Prashant_Karki/Downloaded_DataSets/crime_ds/2024/2024-04/2024-04-south-yorkshire-street.csv")
Crime_ds_2024_05_south_yorkshire  = read_csv("C:/Users/Hp/Desktop/DataScience_Prashant_Karki/Downloaded_DataSets/crime_ds/2024/2024-05/2024-05-south-yorkshire-street.csv")
Crime_ds_2024_06_south_yorkshire  = read_csv("C:/Users/Hp/Desktop/DataScience_Prashant_Karki/Downloaded_DataSets/crime_ds/2024/2024-06/2024-06-south-yorkshire-street.csv")
Crime_ds_2024_07_south_yorkshire  = read_csv("C:/Users/Hp/Desktop/DataScience_Prashant_Karki/Downloaded_DataSets/crime_ds/2024/2024-07/2024-07-south-yorkshire-street.csv")
Crime_ds_2024_08_south_yorkshire  = read_csv("C:/Users/Hp/Desktop/DataScience_Prashant_Karki/Downloaded_DataSets/crime_ds/2024/2024-08/2024-08-south-yorkshire-street.csv")
Crime_ds_2024_09_south_yorkshire  = read_csv("C:/Users/Hp/Desktop/DataScience_Prashant_Karki/Downloaded_DataSets/crime_ds/2024/2024-09/2024-09-south-yorkshire-street.csv")
Crime_ds_2024_10_south_yorkshire  = read_csv("C:/Users/Hp/Desktop/DataScience_Prashant_Karki/Downloaded_DataSets/crime_ds/2024/2024-10/2024-10-south-yorkshire-street.csv")
Crime_ds_2024_11_south_yorkshire  = read_csv("C:/Users/Hp/Desktop/DataScience_Prashant_Karki/Downloaded_DataSets/crime_ds/2024/2024-11/2024-11-south-yorkshire-street.csv")
Crime_ds_2024_12_south_yorkshire  = read_csv("C:/Users/Hp/Desktop/DataScience_Prashant_Karki/Downloaded_DataSets/crime_ds/2024/2024-12/2024-12-south-yorkshire-street.csv")

#west-yorkshire-street

Crime_ds_2022_06_west_yorkshire  = read_csv("C:/Users/Hp/Desktop/DataScience_Prashant_Karki/Downloaded_DataSets/crime_ds/2022/2022-06/2022-06-west-yorkshire-street.csv")
Crime_ds_2022_07_west_yorkshire  = read_csv("C:/Users/Hp/Desktop/DataScience_Prashant_Karki/Downloaded_DataSets/crime_ds/2022/2022-07/2022-07-west-yorkshire-street.csv")
Crime_ds_2022_08_west_yorkshire  = read_csv("C:/Users/Hp/Desktop/DataScience_Prashant_Karki/Downloaded_DataSets/crime_ds/2022/2022-08/2022-08-west-yorkshire-street.csv")
Crime_ds_2022_09_west_yorkshire  = read_csv("C:/Users/Hp/Desktop/DataScience_Prashant_Karki/Downloaded_DataSets/crime_ds/2022/2022-09/2022-09-west-yorkshire-street.csv")
Crime_ds_2022_10_west_yorkshire  = read_csv("C:/Users/Hp/Desktop/DataScience_Prashant_Karki/Downloaded_DataSets/crime_ds/2022/2022-10/2022-10-west-yorkshire-street.csv")
Crime_ds_2022_11_west_yorkshire  = read_csv("C:/Users/Hp/Desktop/DataScience_Prashant_Karki/Downloaded_DataSets/crime_ds/2022/2022-11/2022-11-west-yorkshire-street.csv")
Crime_ds_2022_12_west_yorkshire  = read_csv("C:/Users/Hp/Desktop/DataScience_Prashant_Karki/Downloaded_DataSets/crime_ds/2022/2022-12/2022-12-west-yorkshire-street.csv")

Crime_ds_2023_01_west_yorkshire  = read_csv("C:/Users/Hp/Desktop/DataScience_Prashant_Karki/Downloaded_DataSets/crime_ds/2023/2023-01/2023-01-west-yorkshire-street.csv")
Crime_ds_2023_02_west_yorkshire  = read_csv("C:/Users/Hp/Desktop/DataScience_Prashant_Karki/Downloaded_DataSets/crime_ds/2023/2023-02/2023-02-west-yorkshire-street.csv")
Crime_ds_2023_03_west_yorkshire  = read_csv("C:/Users/Hp/Desktop/DataScience_Prashant_Karki/Downloaded_DataSets/crime_ds/2023/2023-03/2023-03-west-yorkshire-street.csv")
Crime_ds_2023_04_west_yorkshire  = read_csv("C:/Users/Hp/Desktop/DataScience_Prashant_Karki/Downloaded_DataSets/crime_ds/2023/2023-04/2023-04-west-yorkshire-street.csv")
Crime_ds_2023_05_west_yorkshire  = read_csv("C:/Users/Hp/Desktop/DataScience_Prashant_Karki/Downloaded_DataSets/crime_ds/2023/2023-05/2023-05-west-yorkshire-street.csv")
Crime_ds_2023_06_west_yorkshire  = read_csv("C:/Users/Hp/Desktop/DataScience_Prashant_Karki/Downloaded_DataSets/crime_ds/2023/2023-06/2023-06-west-yorkshire-street.csv")
Crime_ds_2023_07_west_yorkshire  = read_csv("C:/Users/Hp/Desktop/DataScience_Prashant_Karki/Downloaded_DataSets/crime_ds/2023/2023-07/2023-07-west-yorkshire-street.csv")
Crime_ds_2023_08_west_yorkshire  = read_csv("C:/Users/Hp/Desktop/DataScience_Prashant_Karki/Downloaded_DataSets/crime_ds/2023/2023-08/2023-08-west-yorkshire-street.csv")
Crime_ds_2023_09_west_yorkshire  = read_csv("C:/Users/Hp/Desktop/DataScience_Prashant_Karki/Downloaded_DataSets/crime_ds/2023/2023-09/2023-09-west-yorkshire-street.csv")
Crime_ds_2023_10_west_yorkshire  = read_csv("C:/Users/Hp/Desktop/DataScience_Prashant_Karki/Downloaded_DataSets/crime_ds/2023/2023-10/2023-10-west-yorkshire-street.csv")
Crime_ds_2023_11_west_yorkshire  = read_csv("C:/Users/Hp/Desktop/DataScience_Prashant_Karki/Downloaded_DataSets/crime_ds/2023/2023-11/2023-11-west-yorkshire-street.csv")
Crime_ds_2023_12_west_yorkshire  = read_csv("C:/Users/Hp/Desktop/DataScience_Prashant_Karki/Downloaded_DataSets/crime_ds/2023/2023-12/2023-12-west-yorkshire-street.csv")

Crime_ds_2024_01_west_yorkshire  = read_csv("C:/Users/Hp/Desktop/DataScience_Prashant_Karki/Downloaded_DataSets/crime_ds/2024/2024-01/2024-01-west-yorkshire-street.csv")
Crime_ds_2024_02_west_yorkshire  = read_csv("C:/Users/Hp/Desktop/DataScience_Prashant_Karki/Downloaded_DataSets/crime_ds/2024/2024-02/2024-02-west-yorkshire-street.csv")
Crime_ds_2024_03_west_yorkshire  = read_csv("C:/Users/Hp/Desktop/DataScience_Prashant_Karki/Downloaded_DataSets/crime_ds/2024/2024-03/2024-03-west-yorkshire-street.csv")
Crime_ds_2024_04_west_yorkshire  = read_csv("C:/Users/Hp/Desktop/DataScience_Prashant_Karki/Downloaded_DataSets/crime_ds/2024/2024-04/2024-04-west-yorkshire-street.csv")
Crime_ds_2024_05_west_yorkshire  = read_csv("C:/Users/Hp/Desktop/DataScience_Prashant_Karki/Downloaded_DataSets/crime_ds/2024/2024-05/2024-05-west-yorkshire-street.csv")
Crime_ds_2024_06_west_yorkshire  = read_csv("C:/Users/Hp/Desktop/DataScience_Prashant_Karki/Downloaded_DataSets/crime_ds/2024/2024-06/2024-06-west-yorkshire-street.csv")
Crime_ds_2024_07_west_yorkshire  = read_csv("C:/Users/Hp/Desktop/DataScience_Prashant_Karki/Downloaded_DataSets/crime_ds/2024/2024-07/2024-07-west-yorkshire-street.csv")
Crime_ds_2024_08_west_yorkshire  = read_csv("C:/Users/Hp/Desktop/DataScience_Prashant_Karki/Downloaded_DataSets/crime_ds/2024/2024-08/2024-08-west-yorkshire-street.csv")
Crime_ds_2024_09_west_yorkshire  = read_csv("C:/Users/Hp/Desktop/DataScience_Prashant_Karki/Downloaded_DataSets/crime_ds/2024/2024-09/2024-09-west-yorkshire-street.csv")
Crime_ds_2024_10_west_yorkshire  = read_csv("C:/Users/Hp/Desktop/DataScience_Prashant_Karki/Downloaded_DataSets/crime_ds/2024/2024-10/2024-10-west-yorkshire-street.csv")
Crime_ds_2024_11_west_yorkshire  = read_csv("C:/Users/Hp/Desktop/DataScience_Prashant_Karki/Downloaded_DataSets/crime_ds/2024/2024-11/2024-11-west-yorkshire-street.csv")
Crime_ds_2024_12_west_yorkshire  = read_csv("C:/Users/Hp/Desktop/DataScience_Prashant_Karki/Downloaded_DataSets/crime_ds/2024/2024-12/2024-12-west-yorkshire-street.csv")



# Create lists of all data frames
south_yorkshire_list <- list(
  Crime_ds_2022_06_south_yorkshire, Crime_ds_2022_07_south_yorkshire, Crime_ds_2022_08_south_yorkshire,
  Crime_ds_2022_09_south_yorkshire, Crime_ds_2022_10_south_yorkshire, Crime_ds_2022_11_south_yorkshire, 
  Crime_ds_2022_12_south_yorkshire, Crime_ds_2023_01_south_yorkshire, Crime_ds_2023_02_south_yorkshire,
  Crime_ds_2023_03_south_yorkshire, Crime_ds_2023_04_south_yorkshire, Crime_ds_2023_05_south_yorkshire, 
  Crime_ds_2023_06_south_yorkshire, Crime_ds_2023_07_south_yorkshire, Crime_ds_2023_08_south_yorkshire, 
  Crime_ds_2023_09_south_yorkshire, Crime_ds_2023_10_south_yorkshire, Crime_ds_2023_11_south_yorkshire,
  Crime_ds_2023_12_south_yorkshire, Crime_ds_2024_01_south_yorkshire, Crime_ds_2024_02_south_yorkshire,
  Crime_ds_2024_03_south_yorkshire, Crime_ds_2024_04_south_yorkshire, Crime_ds_2024_05_south_yorkshire,
  Crime_ds_2024_06_south_yorkshire, Crime_ds_2024_07_south_yorkshire, Crime_ds_2024_08_south_yorkshire,
  Crime_ds_2024_09_south_yorkshire, Crime_ds_2024_10_south_yorkshire, Crime_ds_2024_11_south_yorkshire,
  Crime_ds_2024_12_south_yorkshire
)

west_yorkshire_list <- list(
  Crime_ds_2022_06_west_yorkshire, Crime_ds_2022_07_west_yorkshire, Crime_ds_2022_08_west_yorkshire,
  Crime_ds_2022_09_west_yorkshire, Crime_ds_2022_10_west_yorkshire, Crime_ds_2022_11_west_yorkshire, 
  Crime_ds_2022_12_west_yorkshire, Crime_ds_2023_01_west_yorkshire, Crime_ds_2023_02_west_yorkshire,
  Crime_ds_2023_03_west_yorkshire, Crime_ds_2023_04_west_yorkshire, Crime_ds_2023_05_west_yorkshire, 
  Crime_ds_2023_06_west_yorkshire, Crime_ds_2023_07_west_yorkshire, Crime_ds_2023_08_west_yorkshire, 
  Crime_ds_2023_09_west_yorkshire, Crime_ds_2023_10_west_yorkshire, Crime_ds_2023_11_west_yorkshire,
  Crime_ds_2023_12_west_yorkshire, Crime_ds_2024_01_west_yorkshire, Crime_ds_2024_02_west_yorkshire,
  Crime_ds_2024_03_west_yorkshire, Crime_ds_2024_04_west_yorkshire, Crime_ds_2024_05_west_yorkshire,
  Crime_ds_2024_06_west_yorkshire, Crime_ds_2024_07_west_yorkshire, Crime_ds_2024_08_west_yorkshire,
  Crime_ds_2024_09_west_yorkshire, Crime_ds_2024_10_west_yorkshire, Crime_ds_2024_11_west_yorkshire,
  Crime_ds_2024_12_west_yorkshire
)

# Combine using bind_rows from dplyr (handles large numbers of data frames better)
Crime_combined <- bind_rows(
  bind_rows(south_yorkshire_list),
  bind_rows(west_yorkshire_list)
) %>% 
  as_tibble()

write.csv(Crime_combined,"C:/Users/Hp/Desktop/DataScience_Prashant_Karki/Cleaned_datasets/combined_crimedata/Combined_Crime_Data.csv",row.names = FALSE)

View(Crime_combined)


# Load combined dataset
crimedata = read_csv('C:/Users/Hp/Desktop/DataScience_Prashant_Karki/Cleaned_datasets/combined_crimedata/Combined_Crime_Data.csv') %>% 
  select(Month, `LSOA code`, `Crime type`, `Falls within`)

colnames(crimedata) = c("Year", "LSOA.code", "CrimeType", "Falls Within")

# Load LSOA to Postcode mapping
LsoaToPostcode = read_csv('C:/Users/Hp/Desktop/DataScience_Prashant_Karki/Cleaned_datasets/cleaned_LSOA/Cleaned_LSOA.csv')
colnames(LsoaToPostcode) = c("LSOA.code", "shortPostcode", "Town", "District", "County")

# Check for duplicates
any(duplicated(crimedata$LSOA.code))
any(duplicated(LsoaToPostcode$LSOA.code))

# Remove duplicates
crimedata = unique(crimedata, by = "LSOA.code")
LsoaToPostcode = unique(LsoaToPostcode, by = "LSOA.code")

# Clean and join data
Crime_DataCleaned = crimedata %>%
  left_join(LsoaToPostcode, by = "LSOA.code") %>% 
  mutate(Year = str_trim(substring(Year, 1, 4))) %>% 
  group_by(shortPostcode, CrimeType, Year, `Falls Within`) %>% 
  filter(!is.na(shortPostcode) & !is.na(CrimeType) & !is.na(Year) & !is.na(`Falls Within`)) %>%
  summarize(CrimeCount = n()) %>%
  ungroup()

# Write cleaned dataset to CSV
write.csv(Crime_DataCleaned, "C:/Users/Hp/Desktop/DataScience_Prashant_Karki/Cleaned_datasets/cleaned_crimedata/Cleaned_Crime_Data.csv", row.names = FALSE)

# View cleaned dataset
View(Crime_DataCleaned)
