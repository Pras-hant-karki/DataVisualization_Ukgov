library(tidyverse)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(scales)

# Define color palette
yorkshire_colors <- c("SOUTH YORKSHIRE" = "yellow", "WEST YORKSHIRE" = "#3498DB")

# Set working directory 
setwd("C:/Users/Hp/Desktop/DataScience_Prashant_Karki")

# Read the cleaned house price data
house_data <- read_csv("C:/Users/Hp/Desktop/DataScience_Prashant_Karki/Cleaned_datasets/cleaned_housingdataset/Cleaned_House_Pricing_2021-2023.csv", show_col_types = FALSE)

# Convert Price to numeric and filter out extreme outliers
house_data <- house_data %>%
  mutate(
    Price = as.numeric(Price),
    Year = as.numeric(Year)
  ) %>%
  filter(
    Price >= 10000,  # Minimum reasonable house price
    Price <= 2000000,  # Maximum reasonable house price to remove extreme outliers
    !is.na(Price),
    !is.na(Year),
    Country %in% c("SOUTH YORKSHIRE", "WEST YORKSHIRE")
  )

# Display basic statistics
cat("=== HOUSE PRICE COMPARISON: SOUTH VS WEST YORKSHIRE ===\n\n")
cat("Data Summary:\n")
print(house_data %>% 
        group_by(Country) %>% 
        summarise(
          Count = n(),
          .groups = 'drop'
        ))

# Calculate summary statistics for both regions
summary_stats <- house_data %>%
  group_by(Country) %>%
  summarise(
    Count = n(),
    Mean_Price = round(mean(Price, na.rm = TRUE), 0),
    Median_Price = round(median(Price, na.rm = TRUE), 0),
    Price_SD = round(sd(Price, na.rm = TRUE), 0),
    Min_Price = round(min(Price, na.rm = TRUE), 0),
    Max_Price = round(max(Price, na.rm = TRUE), 0),
    Q1_Price = round(quantile(Price, 0.25, na.rm = TRUE), 0),
    Q3_Price = round(quantile(Price, 0.75, na.rm = TRUE), 0),
    .groups = 'drop'
  )

print(summary_stats)

# Calculate yearly trends
yearly_stats <- house_data %>%
  group_by(Country, Year) %>%
  summarise(
    Count = n(),
    Mean_Price = round(mean(Price, na.rm = TRUE), 0),
    Median_Price = round(median(Price, na.rm = TRUE), 0),
    .groups = 'drop'
  )

print(yearly_stats)

# 1. Box Plot Comparison - House Prices
p1 <- ggplot(house_data, aes(x = Country, y = Price, fill = Country)) +
  geom_boxplot(alpha = 0.7, outlier.alpha = 0.3) +
  geom_violin(alpha = 0.3) +
  scale_fill_manual(values = yorkshire_colors, name = "Region") +
  scale_y_continuous(labels = scales::comma_format(prefix = "£"), 
                     breaks = scales::pretty_breaks(n = 8)) +
  labs(
    title = "House Price Distribution Comparison",
    subtitle = "South Yorkshire vs West Yorkshire (2020-2024)",
    x = "Region",
    y = "House Price (£)",
    caption = "Data source: UK House Price Data"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    legend.position = "none"
  ) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "orange") +
  stat_summary(fun = mean, geom = "text", 
               aes(label = paste("Mean: £", scales::comma(round(after_stat(y), -3)))), 
               vjust = -1, color = "orange", size = 3)


# 2. Density Plot - Price Distribution
p2 <- ggplot(house_data, aes(x = Price, fill = Country)) +
  geom_density(alpha = 0.6) +
  scale_fill_manual(values = yorkshire_colors, name = "Region") +
  scale_x_continuous(labels = scales::comma_format(prefix = "£"),
                     breaks = scales::pretty_breaks(n = 6)) +
  labs(
    title = "House Price Density Distribution",
    subtitle = "Probability density comparison between regions",
    x = "House Price (£)",
    y = "Density"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    legend.position = "bottom"
  ) +
  geom_vline(data = summary_stats, aes(xintercept = Mean_Price, color = Country), 
             linetype = "dashed", linewidth = 1, show.legend = FALSE) +
  scale_color_manual(values = yorkshire_colors)


# 3. Yearly Trend Line Plot
p3 <- ggplot(yearly_stats, aes(x = Year, y = Mean_Price, color = Country, group = Country)) +
  geom_line(linewidth = 1.2, alpha = 0.8) +
  geom_point(size = 3, alpha = 0.9) +
  geom_text(aes(label = scales::comma(Mean_Price, prefix = "£", accuracy = 1000)), 
            vjust = -1, size = 3, fontface = "bold") +
  scale_color_manual(values = yorkshire_colors, name = "Region") +
  scale_y_continuous(labels = scales::comma_format(prefix = "£"),
                     breaks = scales::pretty_breaks(n = 6)) +
  scale_x_continuous(breaks = unique(yearly_stats$Year)) +
  labs(
    title = "Average House Price Trends",
    subtitle = "Year-over-year comparison (2020-2024)",
    x = "Year",
    y = "Average House Price (£)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    legend.position = "bottom"
  )


# 4. Bar Chart - Mean vs Median Comparison
price_comparison <- summary_stats %>%
  select(Country, Mean_Price, Median_Price) %>%
  pivot_longer(cols = c(Mean_Price, Median_Price), 
               names_to = "Price_Type", values_to = "Price") %>%
  mutate(Price_Type = case_when(
    Price_Type == "Mean_Price" ~ "Mean",
    Price_Type == "Median_Price" ~ "Median"
  ))

p4 <- ggplot(price_comparison, aes(x = Country, y = Price, fill = Price_Type)) +
  geom_col(position = "dodge", alpha = 0.8) +
  geom_text(aes(label = scales::comma(Price, prefix = "£")), 
            position = position_dodge(width = 0.9), vjust = -0.5, size = 3, fontface = "bold") +
  scale_fill_manual(values = c("Mean" = "green", "Median" = "grey"), name = "Price Type") +
  scale_y_continuous(labels = scales::comma_format(prefix = "£"),
                     breaks = scales::pretty_breaks(n = 6)) +
  labs(
    title = "Mean vs Median House Prices",
    subtitle = "Statistical comparison by region",
    x = "Region",
    y = "House Price (£)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    legend.position = "bottom"
  )


# 5. District/City Breakdown
district_summary <- house_data %>%
  group_by(Country, District) %>%
  summarise(
    Count = n(),
    Mean_Price = round(mean(Price, na.rm = TRUE), 0),
    Median_Price = round(median(Price, na.rm = TRUE), 0),
    .groups = 'drop'
  ) %>%
  filter(Count >= 20) %>%  # Only include districts with sufficient data
  arrange(Country, desc(Mean_Price))

p5 <- ggplot(district_summary, aes(x = reorder(District, Mean_Price), 
                                   y = Mean_Price, fill = Country)) +
  geom_col(alpha = 0.8) +
  geom_text(aes(label = scales::comma(Mean_Price, prefix = "£", accuracy = 1000)), 
            hjust = -0.1, size = 3) +
  scale_fill_manual(values = yorkshire_colors, name = "Region") +
  scale_y_continuous(labels = scales::comma_format(prefix = "£"),
                     breaks = scales::pretty_breaks(n = 6)) +
  labs(
    title = "Average House Price by District",
    subtitle = "Breakdown within each Yorkshire region",
    x = "District",
    y = "Average House Price (£)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    legend.position = "bottom"
  ) +
  coord_flip()


# 6. Comprehensive Grouped Bar Chart
comprehensive_comparison <- summary_stats %>%
  select(Country, Mean_Price, Median_Price, Q1_Price, Q3_Price) %>%
  pivot_longer(cols = c(Mean_Price, Median_Price, Q1_Price, Q3_Price), 
               names_to = "Price_Metric", values_to = "Price") %>%
  mutate(Price_Metric = case_when(
    Price_Metric == "Mean_Price" ~ "Mean",
    Price_Metric == "Median_Price" ~ "Median",
    Price_Metric == "Q1_Price" ~ "Q1 (25th %ile)",
    Price_Metric == "Q3_Price" ~ "Q3 (75th %ile)"
  ))

p6 <- ggplot(comprehensive_comparison, aes(x = Price_Metric, y = Price, fill = Country)) +
  geom_col(position = "dodge", alpha = 0.8, width = 0.7) +
  geom_text(aes(label = scales::comma(Price, prefix = "£", accuracy = 1000)), 
            position = position_dodge(width = 0.7), vjust = -0.3, size = 3.5, fontface = "bold") +
  scale_fill_manual(values = yorkshire_colors, name = "Region") +
  scale_y_continuous(labels = scales::comma_format(prefix = "£"),
                     breaks = scales::pretty_breaks(n = 8)) +
  labs(
    title = "Comprehensive House Price Comparison",
    subtitle = "South Yorkshire vs West Yorkshire - All Statistical Measures",
    x = "Price Metrics",
    y = "House Price (£)",
    caption = "Comparison of key statistical measures of house prices"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    plot.caption = element_text(hjust = 0.5, size = 10, face = "italic"),
    legend.position = "bottom",
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 11),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 11),
    axis.text.y = element_text(size = 11),
    axis.title = element_text(size = 12, face = "bold"),
    panel.grid.major = element_line(color = "grey90", linewidth = 0.5),
    panel.grid.minor = element_blank()
  ) +
  coord_cartesian(ylim = c(0, max(comprehensive_comparison$Price) * 1.15))


# 7. Price Range Distribution
price_ranges <- house_data %>%
  mutate(
    Price_Range = case_when(
      Price < 100000 ~ "Under £100k",
      Price >= 100000 & Price < 200000 ~ "£100k-£200k",
      Price >= 200000 & Price < 300000 ~ "£200k-£300k",
      Price >= 300000 & Price < 500000 ~ "£300k-£500k",
      Price >= 500000 ~ "£500k+"
    ),
    Price_Range = factor(Price_Range, levels = c("Under £100k", "£100k-£200k", 
                                                 "£200k-£300k", "£300k-£500k", "£500k+"))
  ) %>%
  group_by(Country, Price_Range) %>%
  summarise(Count = n(), .groups = 'drop') %>%
  group_by(Country) %>%
  mutate(Percentage = round(Count / sum(Count) * 100, 1))

p7 <- ggplot(price_ranges, aes(x = Price_Range, y = Percentage, fill = Country)) +
  geom_col(position = "dodge", alpha = 0.8) +
  geom_text(aes(label = paste0(Percentage, "%")), 
            position = position_dodge(width = 0.9), vjust = -0.3, size = 3, fontface = "bold") +
  scale_fill_manual(values = yorkshire_colors, name = "Region") +
  labs(
    title = "House Price Range Distribution",
    subtitle = "Percentage of properties in each price bracket",
    x = "Price Range",
    y = "Percentage of Properties (%)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
p7

# Create combined visualization
combined_plot <- grid.arrange(p1, p2, p3, p4, ncol = 2, nrow = 2)

# Display additional plots separately
print(p5)
print(p6)
print(p7)

# Statistical tests
cat("\n=== STATISTICAL ANALYSIS ===\n")

# Perform t-test for house prices
price_test <- t.test(
  house_data$Price[house_data$Country == "SOUTH YORKSHIRE"],
  house_data$Price[house_data$Country == "WEST YORKSHIRE"]
)

cat("T-test for House Prices (South vs West Yorkshire):\n")
cat("p-value:", price_test$p.value, "\n")
cat("South Yorkshire mean: £", scales::comma(round(price_test$estimate[1], 0)), "\n")
cat("West Yorkshire mean: £", scales::comma(round(price_test$estimate[2], 0)), "\n")
cat("Difference: £", scales::comma(round(price_test$estimate[1] - price_test$estimate[2], 0)), "\n")

# Mann-Whitney U test (non-parametric alternative)
wilcox_test <- wilcox.test(
  house_data$Price[house_data$Country == "SOUTH YORKSHIRE"],
  house_data$Price[house_data$Country == "WEST YORKSHIRE"]
)

cat("\nMann-Whitney U test (non-parametric):\n")
cat("p-value:", wilcox_test$p.value, "\n")

# Save plots
ggsave("C:/Users/Hp/Desktop/DataScience_Prashant_Karki/Recommendation System/Graphs/Yorkshire_HousePrice_Comparison.png", combined_plot, 
       width = 16, height = 12, dpi = 300)
ggsave("C:/Users/Hp/Desktop/DataScience_Prashant_Karki/Recommendation System/Graphs/Yorkshire_HousePrice_Districts.png", p5, 
       width = 12, height = 8, dpi = 300)
ggsave("C:/Users/Hp/Desktop/DataScience_Prashant_Karki/Recommendation System/Graphs/Yorkshire_HousePrice_Comprehensive.png", p6, 
       width = 14, height = 10, dpi = 300)
ggsave("C:/Users/Hp/Desktop/DataScience_Prashant_Karki/Recommendation System/Graphs/Yorkshire_HousePrice_Ranges.png", p7, 
       width = 12, height = 8, dpi = 300)

# Save summary data
write_csv(summary_stats, "C:/Users/Hp/Desktop/DataScience_Prashant_Karki/Cleaned_datasets/cleaned_housingdataset/Yorkshire_HousePrice_Summary.csv")
write_csv(district_summary, "C:/Users/Hp/Desktop/DataScience_Prashant_Karki/Cleaned_datasets/cleaned_housingdataset/Yorkshire_HousePrice_Districts.csv")
write_csv(yearly_stats, "C:/Users/Hp/Desktop/DataScience_Prashant_Karki/Cleaned_datasets/cleaned_housingdataset/Yorkshire_HousePrice_Yearly.csv")
write_csv(price_ranges, "C:/Users/Hp/Desktop/DataScience_Prashant_Karki/Cleaned_datasets/cleaned_housingdataset/Yorkshire_HousePrice_Ranges.csv")

cat("\n=== ANALYSIS COMPLETE ===\n")
cat("Visualizations saved to Graphs/ directory:\n")
cat("- Yorkshire_HousePrice_Comparison.png (Combined 4-panel view)\n")
cat("- Yorkshire_HousePrice_Districts.png (District breakdown)\n")
cat("- Yorkshire_HousePrice_Comprehensive.png (Grouped bar chart)\n")
cat("- Yorkshire_HousePrice_Ranges.png (Price range distribution)\n")
cat("Summary data saved to Cleaned Data/ directory\n")