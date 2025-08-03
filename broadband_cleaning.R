library(tidyverse)
library(dplyr)
library(stringi)
library(scales)

# Set working directory 
setwd("C:/Users/Hp/Desktop/DataScience_Prashant_Karki")

# BROADBAND DATA CLEANING
Broadband = read_csv("C:/Users/Hp/Desktop/DataScience_Prashant_Karki/Downloaded_DataSets/broadband/broadband_performance.csv", show_col_types = FALSE)


pattern = ' .*$'
BroadbandData = Broadband %>%
  mutate(shortPostcode=gsub(pattern,"",postcode_space)) %>% 
  mutate(ID = row_number()) %>% 
  select(`ID`, `postcode area`, shortPostcode, `Average download speed (Mbit/s)`,
         `Average upload speed (Mbit/s)`, `Minimum download speed (Mbit/s)`,
         `Minimum upload speed (Mbit/s)`) %>% 
  na.omit()
colnames(BroadbandData) = c( "ID","postcode area", "shortPostcode", "Avgdownload",
                             "Average upload speed (Mbit/s)", "Mindownload",
                             "Minimum upload speed (Mbit/s)")
write.csv(BroadbandData, "C:/Users/Hp/Desktop/DataScience_Prashant_Karki/Cleaned_datasets/cleaned_broadband/Cleaned_Broadband_Speed.csv",row.names = FALSE)
view(BroadbandData)
