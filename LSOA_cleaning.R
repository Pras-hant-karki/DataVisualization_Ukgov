#importing libraries
#install.packages("data.table")
library(data.table)
library(tidyverse)
library(dplyr)

# Set working directory 
setwd("C:/Users/Hp/Desktop/DataScience_Prashant_Karki")

Cleaned_HP = read.csv("C:/Users/Hp/Desktop/DataScience_Prashant_Karki/Cleaned_datasets/cleaned_populationdata/Cleaned_Population.csv")

LSOA = read_csv("C:/Users/Hp/Desktop/DataScience_Prashant_Karki/Downloaded_DataSets/LSOA/Postcode to LSOA.csv")
pattern = ' .*$'
LSOA_Cleaned = LSOA %>%
  select(lsoa11cd, pcds) %>%
  mutate(shortPostcode = gsub(pattern, "", pcds)) %>%
  right_join(Cleaned_HP, by = "shortPostcode", relationship = "many-to-many") %>%
  select(lsoa11cd, shortPostcode, Town, District, Country)


LSOA_Cleaned


colnames(LSOA_Cleaned)[1] = "LSOA code"
view(LSOA_Cleaned)
write.csv(LSOA_Cleaned, "C:/Users/Hp/Desktop/DataScience_Prashant_Karki/Cleaned_datasets/cleaned_LSOA/Cleaned_LSOA.csv", row.names = FALSE)

