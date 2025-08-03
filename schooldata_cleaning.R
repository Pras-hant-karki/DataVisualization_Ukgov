library(tidyverse)
library(dplyr)
library(stringi)
library(scales)
library(data.table)

# Load school info data for each year
Schools2021 <- read_csv("C:/Users/Hp/Desktop/DataScience_Prashant_Karki/Downloaded_DataSets/School_ds/Performancetables_063739/2021-2022/england_school_information.csv")
Schools2022 <- read_csv("C:/Users/Hp/Desktop/DataScience_Prashant_Karki/Downloaded_DataSets/School_ds/Performancetables_064133/2022-2023/england_school_information.csv")
Schools2023 <- read_csv("C:/Users/Hp/Desktop/DataScience_Prashant_Karki/Downloaded_DataSets/School_ds/Performancetables_064244/2023-2024/england_school_information.csv")

# Load KS4 performance data for each year
KS4_2021 <- read_csv("C:/Users/Hp/Desktop/DataScience_Prashant_Karki/Downloaded_DataSets/School_ds/Performancetables_063739/2021-2022/england_ks4final.csv")
KS4_2022 <- read_csv("C:/Users/Hp/Desktop/DataScience_Prashant_Karki/Downloaded_DataSets/School_ds/Performancetables_064133/2022-2023/england_ks4final.csv")
KS4_2023 <- read_csv("C:/Users/Hp/Desktop/DataScience_Prashant_Karki/Downloaded_DataSets/School_ds/Performancetables_064244/2023-2024/england_ks4final.csv")

# Add year column to each dataset
Schools2021$Year <- "2022"
Schools2022$Year <- "2023"
Schools2023$Year <- "2024"

KS4_2021$Year <- "2022"
KS4_2022$Year <- "2023"
KS4_2023$Year <- "2024"

# Convert all columns to character to avoid type conflicts
Schools2021 <- Schools2021 %>% mutate(across(everything(), as.character))
Schools2022 <- Schools2022 %>% mutate(across(everything(), as.character))
Schools2023 <- Schools2023 %>% mutate(across(everything(), as.character))

KS4_2021 <- KS4_2021 %>% mutate(across(everything(), as.character))
KS4_2022 <- KS4_2022 %>% mutate(across(everything(), as.character))
KS4_2023 <- KS4_2023 %>% mutate(across(everything(), as.character))

# Combine school information data
CombinedSchools <- bind_rows(Schools2021, Schools2022, Schools2023) %>%
  filter(!is.na(URN), !is.na(SCHNAME), !is.na(LANAME)) %>%
  distinct() %>%
  as_tibble()


# Combine KS4 performance data
CombinedKS4 <- bind_rows(KS4_2021, KS4_2022, KS4_2023) %>%
  filter(!is.na(URN)) %>%
  distinct() %>%
  as_tibble()


# Filter for Yorkshire schools and clean data
CleanedSchools <- filter(CombinedSchools, 
                         str_detect(LANAME, regex("york|leeds|bradford|sheffield|rotherham|barnsley|doncaster|wakefield|kirklees|calderdale", ignore_case = TRUE))) %>%
  mutate(shortPostcode = str_trim(substring(POSTCODE, 1, 4))) %>%
  mutate(SCHNAME = str_to_title(SCHNAME)) %>%
  mutate(County = case_when(
    str_detect(LANAME, regex("leeds|bradford|wakefield|kirklees|calderdale", ignore_case = TRUE)) ~ "West Yorkshire",
    str_detect(LANAME, regex("sheffield|rotherham|barnsley|doncaster", ignore_case = TRUE)) ~ "South Yorkshire",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(County)) %>%
  select(URN, SCHNAME, POSTCODE, shortPostcode, County, Year) %>%
  arrange(County)

# Convert URN to numeric in CleanedSchools for consistent joining
CleanedSchools <- CleanedSchools %>%
  mutate(URN = as.numeric(URN))

# Filter KS4 data for Yorkshire schools only
CleanedKS4 <- filter(CombinedKS4, URN %in% CleanedSchools$URN) %>%
  select(URN, Year, ATT8SCR) %>%
  mutate(URN = as.numeric(URN)) %>%
  mutate(ATT8SCR = ifelse(ATT8SCR %in% c("SUPP", "NE", "NEW", "NP"), NA, as.numeric(ATT8SCR)))

# Join school info with performance data
FinalCleanedSchools <- inner_join(CleanedSchools, CleanedKS4, by = c("URN", "Year")) %>%
  select(ID = URN, 
         Year, 
         PostCode = POSTCODE, 
         shortPostcode, 
         SchoolName = SCHNAME, 
         Attainment8Score = ATT8SCR, 
         County) %>%
  arrange(Year, County, SchoolName)

# Export cleaned data
write_csv(FinalCleanedSchools, "C:/Users/Hp/Desktop/DataScience_Prashant_Karki/Cleaned_datasets/cleaned_schooldata/CleanedSchools.csv")
View(FinalCleanedSchools)
