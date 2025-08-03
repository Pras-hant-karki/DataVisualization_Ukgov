library(tidyverse)
library(dplyr)
library(scales)
library(fmsb)
library(data.table) 

# Set working directory 
setwd("C:/Users/Hp/Desktop/DataScience_Prashant_Karki")

Towns = read_csv("C:/Users/Hp/Desktop/DataScience_Prashant_Karki/Cleaned_datasets/cleaned_populationdata/Cleaned_Population.csv")%>% 
  select(shortPostcode, Town, District, Country)
BroadbandCleaned = read_csv("C:/Users/Hp/Desktop/DataScience_Prashant_Karki/Cleaned_datasets/cleaned_broadband/Cleaned_Broadband_Speed.csv", show_col_types = FALSE)

broadband=Towns %>% 
  left_join(BroadbandCleaned,by="shortPostcode")
View(broadband)


# CHART 1: Box plot for average download speed 
broadband %>%
  ggplot(aes(x = Country, y = Avgdownload, fill = Country)) +
  geom_boxplot() +
  scale_y_continuous(breaks = seq(0, 1000, 50)) +
  labs(title = "Average Download Speed (Mbit/s)",
       x = "Country", y = "Average Download Speed (Mbit/s)") +
  coord_flip()


# CHART 2: Bar Plot for average download speed
broadband %>%
  filter(Country == "SOUTH YORKSHIRE") %>%
  ggplot(aes(x = District, y = Avgdownload, fill = District)) +
  geom_bar(stat = "identity") +
  labs(title = "Average Download Speed in South Yorkshire",
       x = "District", y = "Average Download Speed (Mbit/s)")


# CHART 3: Bar Plot for average download speed 
broadband %>%
  filter(Country == "WEST YORKSHIRE") %>%
  ggplot(aes(x = District, y = Avgdownload, fill = District)) +
  geom_bar(stat = "identity") +
  labs(title = "Average Download Speed West Yorkshire",
       x = "District", y = "Average Download Speed (Mbit/s)")
