library(tidyverse)
library(dplyr)
library(scales)
library(fmsb)
library(data.table) 

options(scipen=100)

# Set working directory 
setwd("C:/Users/Hp/Desktop/DataScience_Prashant_Karki")

euro <- dollar_format(prefix = "/u20ac", big.mark = ",")
HousePrices <- read_csv("C:/Users/Hp/Desktop/DataScience_Prashant_Karki/Cleaned_datasets/cleaned_housingdataset/Cleaned_House_Pricing_2021-2023.csv", show_col_types = FALSE)


# House Pricing Box Plot
House_town <- HousePrices %>%
  group_by(Town, District, Country, Year) %>%
  summarise(AveragePrice = mean(Price)) %>%
  ungroup(Town, District, Country, Year) %>%
  na.omit()

House_town %>%
  filter(Year == 2023) %>%
  ggplot(aes(x = Country, y = AveragePrice, fill = Country)) +
  geom_boxplot() +
  labs(title = "Average House Prices in  2023")



# House Pricing Bar Plot
HousePrices %>% 
  filter(Year == 2023) %>% 
  group_by(Country) %>% 
  summarise(AveragePrice = mean(Price)) %>% 
  ggplot(aes(x = Country, y = AveragePrice)) +
  geom_bar(position = "stack", stat = "identity", fill = "orange") +
  scale_y_continuous(limits = c(0, 1000000), breaks = seq(0, 1000000, 50000), 
                     labels = euro(seq(0, 1000000, 50000))) +
  geom_text(aes(label = euro(AveragePrice)), vjust = -0.25) +
  labs(title = "Average House Prices in 2023") 
coord_flip()



# House Pricing Line Graph
HousePrices %>%
  filter(Year == 2021 | Year == 2022 | Year == 2023) %>%
  group_by(Country,Year) %>%
  summarise(AveragePrice = mean(Price)) %>%
  ggplot( aes(x = Year, y = AveragePrice, group = Country, color = Country)) +
  geom_line(linewidth = 1) + #defining line width 
  geom_point(size = 2, color = "navy") + #defining point size and color
  scale_y_continuous(limits=c(0,700000), breaks = seq(0,700000,100000), labels = label_number()) +
  labs(title = "2021-2023 Average House Prices", #defining labels 
       x = "Year",
       y = "Average Price")


