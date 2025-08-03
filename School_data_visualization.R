library(tidyverse)

# Load Cleaned School and Population Data
schoolData <- read_csv("C:/Users/Hp/Desktop/DataScience_Prashant_Karki/Cleaned_datasets/cleaned_schooldata/CleanedSchools.csv", show_col_types = FALSE)
Town <- read_csv("C:/Users/Hp/Desktop/DataScience_Prashant_Karki/Cleaned_datasets/cleaned_populationdata/Cleaned_Population.csv") %>% 
  select(shortPostcode, District, Country)

# Ensure numeric types and consistent keys
schoolData <- schoolData %>%
  mutate(
    Attainment8Score = as.numeric(Attainment8Score),  # Column name as per FinalCleanedSchools
    shortPostcode = shortPostcode  # Already exists in cleaned file
  )

# Join with Town/Population data
school_Data <- schoolData %>%
  left_join(Town, by = "shortPostcode") %>%  # Removed `relationship = "many-to-many"` — not needed
  na.omit()

# Chart 1: Boxplot for average attainment score by Country in 2022
school_Data %>%
  filter(Year == 2022) %>%
  mutate(Country = as.factor(Country)) %>%
  ggplot(aes(x = Country, y = Attainment8Score, fill = Country)) +
  geom_boxplot(color = "black") +
  labs(
    title = "Boxplot of Attainment 8 Score (2022)",
    x = "County",
    y = "Attainment 8 Score"
  ) +
  theme_minimal()

# Chart 2: Line chart – West Yorkshire 2020–2023
west_yorkshire_data <- school_Data %>%
  filter(County == "West Yorkshire", Year %in% 2020:2023) %>%
  group_by(District, Year) %>%
  summarise(AverageAttainment = mean(Attainment8Score, na.rm = TRUE), .groups = 'drop')

ggplot(west_yorkshire_data, aes(x = Year, y = AverageAttainment, group = District, color = District)) +
  geom_line(size = 1) +
  geom_text(aes(label = round(AverageAttainment, 1)), vjust = -0.85, size = 3) +
  geom_point(size = 2) +
  labs(title = "West Yorkshire Average Attainment Score (2020–2023) by District") +
  scale_x_continuous(breaks = 2020:2023)

# Chart 3: Line chart – South Yorkshire 2020–2023
south_yorkshire_data <- school_Data %>%
  filter(County == "South Yorkshire", Year %in% 2020:2023) %>%
  group_by(District, Year) %>%
  summarise(AverageAttainment = mean(Attainment8Score, na.rm = TRUE), .groups = 'drop')

ggplot(south_yorkshire_data, aes(x = Year, y = AverageAttainment, group = District, color = District)) +
  geom_line(size = 1) +
  geom_text(aes(label = round(AverageAttainment, 1)), vjust = -0.85, size = 3) +
  geom_point(size = 2) +
  labs(title = "South Yorkshire Average Attainment Score (2020–2023) by District") +
  scale_x_continuous(breaks = 2020:2023)
