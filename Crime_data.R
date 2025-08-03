library(tidyverse)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(scales)

# Define color palette
yorkshire_colors <- c("South Yorkshire" = "black", "West Yorkshire" = "red")

# Set working directory 
setwd("C:/Users/Hp/Desktop/DataScience_Prashant_Karki")

# Read the cleaned crime data
crime_data <- read_csv("C:/Users/Hp/Desktop/DataScience_Prashant_Karki/Cleaned_datasets/cleaned_crimedata/Cleaned_Crime_Data.csv", show_col_types = FALSE)

# Clean and prepare the data
crime_data <- crime_data %>%
  mutate(
    # Simplify the region names
    Region = case_when(
      `Falls Within` == "South Yorkshire Police" ~ "South Yorkshire",
      `Falls Within` == "West Yorkshire Police" ~ "West Yorkshire",
      TRUE ~ "Other"
    ),
    # Ensure numeric values
    CrimeCount = as.numeric(CrimeCount),
    Year = as.numeric(Year)
  ) %>%
  filter(
    Region != "Other",
    !is.na(CrimeCount),
    CrimeCount > 0,
    Year >= 2022 & Year <= 2025
  )

# Display basic statistics
cat("=== CRIME STATISTICS COMPARISON: SOUTH VS WEST YORKSHIRE ===\n\n")
cat("Data Summary:\n")
print(crime_data %>% 
        group_by(Region) %>% 
        summarise(
          Total_Records = n(),
          Total_Crimes = sum(CrimeCount, na.rm = TRUE),
          Crime_Types = n_distinct(CrimeType),
          Years_Covered = n_distinct(Year),
          .groups = 'drop'
        ))

# Calculate summary statistics for both regions
summary_stats <- crime_data %>%
  group_by(Region) %>%
  summarise(
    Total_Crimes = sum(CrimeCount, na.rm = TRUE),
    Avg_Crimes_Per_Record = round(mean(CrimeCount, na.rm = TRUE), 2),
    Median_Crimes = round(median(CrimeCount, na.rm = TRUE), 2),
    Crime_Types = n_distinct(CrimeType),
    Max_Single_Crime_Count = max(CrimeCount, na.rm = TRUE),
    .groups = 'drop'
  )

print(summary_stats)

# Crime type breakdown
crime_type_stats <- crime_data %>%
  group_by(Region, CrimeType) %>%
  summarise(
    Total_Count = sum(CrimeCount, na.rm = TRUE),
    Avg_Count = round(mean(CrimeCount, na.rm = TRUE), 2),
    Records = n(),
    .groups = 'drop'
  ) %>%
  group_by(Region) %>%
  mutate(
    Percentage = round(Total_Count / sum(Total_Count) * 100, 2)
  ) %>%
  arrange(Region, desc(Total_Count))

print(crime_type_stats)

# Yearly trends
yearly_stats <- crime_data %>%
  group_by(Region, Year) %>%
  summarise(
    Total_Crimes = sum(CrimeCount, na.rm = TRUE),
    Avg_Crimes = round(mean(CrimeCount, na.rm = TRUE), 2),
    Crime_Types = n_distinct(CrimeType),
    .groups = 'drop'
  )

print(yearly_stats)

# 1. Total Crime Count Comparison (Bar Chart)
p1 <- ggplot(summary_stats, aes(x = Region, y = Total_Crimes, fill = Region)) +
  geom_col(alpha = 0.8, width = 0.6) +
  geom_text(aes(label = scales::comma(Total_Crimes)), 
            vjust = -0.5, size = 4, fontface = "bold") +
  scale_fill_manual(values = yorkshire_colors, name = "Region") +
  scale_y_continuous(labels = scales::comma_format(), 
                     breaks = scales::pretty_breaks(n = 6)) +
  labs(
    title = "Total Crime Count Comparison",
    subtitle = "South Yorkshire vs West Yorkshire (2022-2025)",
    x = "Region",
    y = "Total Number of Crimes",
    caption = "Data source: UK Police Crime Statistics"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    legend.position = "none",
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 11),
    axis.title = element_text(size = 12, face = "bold")
  )

# 2. Crime Type Breakdown (Grouped Bar Chart)
top_crime_types <- crime_type_stats %>%
  group_by(CrimeType) %>%
  summarise(Total_Overall = sum(Total_Count), .groups = 'drop') %>%
  slice_max(Total_Overall, n = 8) %>%
  pull(CrimeType)

crime_type_comparison <- crime_type_stats %>%
  filter(CrimeType %in% top_crime_types)

p2 <- ggplot(crime_type_comparison, aes(x = reorder(CrimeType, Total_Count), 
                                        y = Total_Count, fill = Region)) +
  geom_col(position = "dodge", alpha = 0.8) +
  geom_text(aes(label = scales::comma(Total_Count)), 
            position = position_dodge(width = 0.9), hjust = -0.1, size = 3) +
  scale_fill_manual(values = yorkshire_colors, name = "Region") +
  scale_y_continuous(labels = scales::comma_format()) +
  labs(
    title = "Crime Type Comparison",
    subtitle = "Top crime categories by total count",
    x = "Crime Type",
    y = "Total Crime Count"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  coord_flip()


# 3. Yearly Trend Analysis
p3 <- ggplot(yearly_stats, aes(x = Year, y = Total_Crimes, color = Region, group = Region)) +
  geom_line(linewidth = 1.2, alpha = 0.8) +
  geom_point(size = 3, alpha = 0.9) +
  geom_text(aes(label = scales::comma(Total_Crimes, accuracy = 1)), 
            vjust = -1, size = 3, fontface = "bold") +
  scale_color_manual(values = yorkshire_colors, name = "Region") +
  scale_y_continuous(labels = scales::comma_format(),
                     breaks = scales::pretty_breaks(n = 6)) +
  scale_x_continuous(breaks = unique(yearly_stats$Year)) +
  labs(
    title = "Crime Trends Over Time",
    subtitle = "Year-over-year comparison (2022-2025)",
    x = "Year",
    y = "Total Crime Count"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    legend.position = "bottom"
  )


# 4. Crime Percentage Distribution
crime_percentage <- crime_type_stats %>%
  filter(CrimeType %in% top_crime_types) %>%
  select(Region, CrimeType, Percentage)

p4 <- ggplot(crime_percentage, aes(x = CrimeType, y = Percentage, fill = Region)) +
  geom_col(position = "dodge", alpha = 0.8) +
  geom_text(aes(label = paste0(Percentage, "%")), 
            position = position_dodge(width = 0.9), vjust = -0.3, size = 3) +
  scale_fill_manual(values = yorkshire_colors, name = "Region") +
  labs(
    title = "Crime Type Distribution (%)",
    subtitle = "Percentage breakdown of crime types by region",
    x = "Crime Type",
    y = "Percentage of Total Crimes (%)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


# 5. Comprehensive Grouped Bar Chart - Key Metrics
key_metrics <- crime_type_stats %>%
  filter(CrimeType %in% c("Anti-social behaviour", "Violence and sexual offences", 
                          "Criminal damage and arson", "Other theft", "Burglary", "Drugs")) %>%
  select(Region, CrimeType, Total_Count)

p5 <- ggplot(key_metrics, aes(x = CrimeType, y = Total_Count, fill = Region)) +
  geom_col(position = "dodge", alpha = 0.8, width = 0.7) +
  geom_text(aes(label = scales::comma(Total_Count, accuracy = 1)), 
            position = position_dodge(width = 0.7), vjust = -0.3, size = 3.5, fontface = "bold") +
  scale_fill_manual(values = yorkshire_colors, name = "Region") +
  scale_y_continuous(labels = scales::comma_format(),
                     breaks = scales::pretty_breaks(n = 8)) +
  labs(
    title = "Comprehensive Crime Comparison",
    subtitle = "South Yorkshire vs West Yorkshire - Key Crime Categories",
    x = "Crime Categories",
    y = "Total Crime Count",
    caption = "Comparison of major crime types between regions"
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
  coord_cartesian(ylim = c(0, max(key_metrics$Total_Count) * 1.15))


# 6. Crime Rate per Region (Average per Record)
p6 <- ggplot(summary_stats, aes(x = Region, y = Avg_Crimes_Per_Record, fill = Region)) +
  geom_col(alpha = 0.8, width = 0.6) +
  geom_text(aes(label = round(Avg_Crimes_Per_Record, 2)), 
            vjust = -0.5, size = 4, fontface = "bold") +
  scale_fill_manual(values = yorkshire_colors, name = "Region") +
  labs(
    title = "Average Crime Rate per Record",
    subtitle = "Mean number of crimes per data record",
    x = "Region",
    y = "Average Crimes per Record"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    legend.position = "none"
  )


# 7. Specific Crime Analysis - Violence and Theft
specific_crimes <- crime_type_stats %>%
  filter(CrimeType %in% c("Violence and sexual offences", "Theft from the person", 
                          "Vehicle crime", "Robbery")) %>%
  select(Region, CrimeType, Total_Count)

p7 <- ggplot(specific_crimes, aes(x = CrimeType, y = Total_Count, fill = Region)) +
  geom_col(position = "dodge", alpha = 0.8) +
  geom_text(aes(label = scales::comma(Total_Count)), 
            position = position_dodge(width = 0.9), vjust = -0.3, size = 3, fontface = "bold") +
  scale_fill_manual(values = yorkshire_colors, name = "Region") +
  scale_y_continuous(labels = scales::comma_format()) +
  labs(
    title = "Serious Crime Categories",
    subtitle = "Violence, theft, and robbery comparison",
    x = "Crime Type",
    y = "Total Crime Count"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


# Create combined visualization
combined_plot <- grid.arrange(p1, p3, p4, p6, ncol = 2, nrow = 2)

# Display additional plots separately
print(p2)
print(p5)
print(p7)

# Statistical Analysis
cat("\n=== STATISTICAL ANALYSIS ===\n")

# Total crime comparison
south_total <- summary_stats$Total_Crimes[summary_stats$Region == "South Yorkshire"]
west_total <- summary_stats$Total_Crimes[summary_stats$Region == "West Yorkshire"]

cat("Total Crime Comparison:\n")
cat("South Yorkshire total crimes:", scales::comma(south_total), "\n")
cat("West Yorkshire total crimes:", scales::comma(west_total), "\n")
cat("Difference:", scales::comma(west_total - south_total), "more crimes in West Yorkshire\n")
cat("Percentage difference:", round((west_total - south_total) / south_total * 100, 2), "%\n")

# Crime rate comparison
south_rate <- summary_stats$Avg_Crimes_Per_Record[summary_stats$Region == "South Yorkshire"]
west_rate <- summary_stats$Avg_Crimes_Per_Record[summary_stats$Region == "West Yorkshire"]

cat("\nAverage Crime Rate Comparison:\n")
cat("South Yorkshire rate:", south_rate, "crimes per record\n")
cat("West Yorkshire rate:", west_rate, "crimes per record\n")

# Top crime types by region
cat("\n=== TOP CRIME TYPES BY REGION ===\n")
cat("Top 5 Crime Types in South Yorkshire:\n")
print(crime_type_stats %>% 
        filter(Region == "South Yorkshire") %>% 
        head(5) %>% 
        select(CrimeType, Total_Count, Percentage))

cat("\nTop 5 Crime Types in West Yorkshire:\n")
print(crime_type_stats %>% 
        filter(Region == "West Yorkshire") %>% 
        head(5) %>% 
        select(CrimeType, Total_Count, Percentage))

# Save plots
ggsave("C:/Users/Hp/Desktop/DataScience_Prashant_Karki/Recommendation System/Graphs/Yorkshire_Crime_Comparison.png", combined_plot, 
       width = 16, height = 12, dpi = 300)
ggsave("C:/Users/Hp/Desktop/DataScience_Prashant_Karki/Recommendation System/Graphs/Yorkshire_Crime_Types.png", p2, 
       width = 12, height = 8, dpi = 300)
ggsave("C:/Users/Hp/Desktop/DataScience_Prashant_Karki/Recommendation System/Graphs/Yorkshire_Crime_Comprehensive.png", p5, 
       width = 14, height = 10, dpi = 300)
ggsave("C:/Users/Hp/Desktop/DataScience_Prashant_Karki/Recommendation System/Graphs/Yorkshire_Crime_Serious.png", p7, 
       width = 12, height = 8, dpi = 300)

# Save summary data
write_csv(summary_stats, "C:/Users/Hp/Desktop/DataScience_Prashant_Karki/Cleaned_datasets/cleaned_crimedata/Yorkshire_Crime_Summary.csv")
write_csv(crime_type_stats, "C:/Users/Hp/Desktop/DataScience_Prashant_Karki/Cleaned_datasets/cleaned_crimedata/Yorkshire_Crime_Types.csv")
write_csv(yearly_stats, "C:/Users/Hp/Desktop/DataScience_Prashant_Karki/Cleaned_datasets/cleaned_crimedata/Yorkshire_Crime_Yearly.csv")
write_csv(key_metrics, "C:/Users/Hp/Desktop/DataScience_Prashant_Karki/Cleaned_datasets/cleaned_crimedata/Yorkshire_Crime_KeyMetrics.csv")

cat("\n=== ANALYSIS COMPLETE ===\n")
cat("Visualizations saved to Graphs/ directory:\n")
cat("- Yorkshire_Crime_Comparison.png (Combined 4-panel view)\n")
cat("- Yorkshire_Crime_Types.png (Crime type breakdown)\n")
cat("- Yorkshire_Crime_Comprehensive.png (Grouped bar chart)\n")
cat("- Yorkshire_Crime_Serious.png (Serious crime analysis)\n")
cat("Summary data saved to Cleaned Data/ directory\n")