library(tidyverse)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(scales)

# Define color palette
yorkshire_colors <- c("South Yorkshire" = "black", "West Yorkshire" = "#E67E22")

# Set working directory 
setwd("C:/Users/Hp/Desktop/DataScience_Prashant_Karki")

# Read the cleaned school data
school_data <- read_csv("C:/Users/Hp/Desktop/DataScience_Prashant_Karki/Cleaned_datasets/cleaned_schooldata/CleanedSchools.csv", show_col_types = FALSE)
colnames(school_data)

# Extract postcode area and classify regions
school_data <- school_data %>%
  mutate(
    # Extract the postcode area (letters before numbers)
    postcode_area = str_extract(PostCode, "^[A-Z]+"),
    Region = case_when(
      postcode_area %in% c("S", "DN") ~ "South Yorkshire",
      postcode_area %in% c("BD", "LS", "WF", "HD", "HX") ~ "West Yorkshire",
      TRUE ~ "Other"
    )
  ) %>%
  filter(Region != "Other") %>%
  # Convert Attainment8Score to numeric and filter reasonable ranges
  mutate(Attainment8Score = as.numeric(Attainment8Score)) %>%
  filter(Attainment8Score >= 0, Attainment8Score <= 100, !is.na(Attainment8Score))

# Display basic statistics
cat("=== SCHOOL PERFORMANCE COMPARISON: SOUTH VS WEST YORKSHIRE ===\n\n")
cat("Data Summary:\n")
print(school_data %>% 
        group_by(Region) %>% 
        summarise(
          Count = n(),
          Schools = n_distinct(SchoolName),
          .groups = 'drop'
        ))

# Calculate summary statistics for both regions
summary_stats <- school_data %>%
  group_by(Region) %>%
  summarise(
    Count = n(),
    Schools = n_distinct(SchoolName),
    Mean_ATT8 = round(mean(Attainment8Score, na.rm = TRUE), 2),
    Median_ATT8 = round(median(Attainment8Score, na.rm = TRUE), 2),
    ATT8_SD = round(sd(Attainment8Score, na.rm = TRUE), 2),
    Min_ATT8 = round(min(Attainment8Score, na.rm = TRUE), 2),
    Max_ATT8 = round(max(Attainment8Score, na.rm = TRUE), 2),
    Q1_ATT8 = round(quantile(Attainment8Score, 0.25, na.rm = TRUE), 2),
    Q3_ATT8 = round(quantile(Attainment8Score, 0.75, na.rm = TRUE), 2),
    .groups = 'drop'
  )

print(summary_stats)

# Calculate yearly trends
yearly_stats <- school_data %>%
  group_by(Region, Year) %>%
  summarise(
    Count = n(),
    Schools = n_distinct(SchoolName),
    Mean_ATT8 = round(mean(Attainment8Score, na.rm = TRUE), 2),
    Median_ATT8 = round(median(Attainment8Score, na.rm = TRUE), 2),
    .groups = 'drop'
  )

print(yearly_stats)

# 1. Box Plot Comparison - Attainment 8 Scores
p1 <- ggplot(school_data, aes(x = Region, y = Attainment8Score, fill = Region)) +
  geom_boxplot(alpha = 0.7, outlier.alpha = 0.3) +
  geom_violin(alpha = 0.3) +
  scale_fill_manual(values = yorkshire_colors, name = "Region") +
  labs(
    title = "Attainment 8 Score Distribution",
    subtitle = "South Yorkshire vs West Yorkshire School Performance",
    x = "Region",
    y = "Attainment 8 Score",
    caption = "Data source: England KS4 Final Results | Higher scores indicate better performance"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    legend.position = "none"
  ) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "brown") +
  stat_summary(fun = mean, geom = "text", 
               aes(label = paste("Mean:", round(after_stat(y), 1))), 
               vjust = -1, color = "brown", size = 3)


# 2. Density Plot - Attainment 8 Distribution
p2 <- ggplot(school_data, aes(x = Attainment8Score, fill = Region)) +
  geom_density(alpha = 0.6) +
  scale_fill_manual(values = yorkshire_colors, name = "Region") +
  labs(
    title = "Attainment 8 Score Density Distribution",
    subtitle = "Performance distribution comparison between regions",
    x = "Attainment 8 Score",
    y = "Density"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    legend.position = "bottom"
  ) +
  geom_vline(data = summary_stats, aes(xintercept = Mean_ATT8, color = Region), 
             linetype = "dashed", linewidth = 1, show.legend = FALSE) +
  scale_color_manual(values = yorkshire_colors)


# 3. Yearly Trend Line Plot
p3 <- ggplot(yearly_stats, aes(x = Year, y = Mean_ATT8, color = Region, group = Region)) +
  geom_line(linewidth = 1.2, alpha = 0.8) +
  geom_point(size = 3, alpha = 0.9) +
  geom_text(aes(label = round(Mean_ATT8, 1)), 
            vjust = -1, size = 3, fontface = "bold") +
  scale_color_manual(values = yorkshire_colors, name = "Region") +
  scale_x_continuous(breaks = unique(yearly_stats$Year)) +
  labs(
    title = "Average Attainment 8 Score Trends",
    subtitle = "Year-over-year performance comparison (2020-2022)",
    x = "Year",
    y = "Average Attainment 8 Score"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    legend.position = "bottom"
  )


# 4. Bar Chart - Mean vs Median Comparison
performance_comparison <- summary_stats %>%
  select(Region, Mean_ATT8, Median_ATT8) %>%
  pivot_longer(cols = c(Mean_ATT8, Median_ATT8), 
               names_to = "Statistic", values_to = "Score") %>%
  mutate(Statistic = case_when(
    Statistic == "Mean_ATT8" ~ "Mean",
    Statistic == "Median_ATT8" ~ "Median"
  ))

p4 <- ggplot(performance_comparison, aes(x = Region, y = Score, fill = Statistic)) +
  geom_col(position = "dodge", alpha = 0.8) +
  geom_text(aes(label = round(Score, 1)), 
            position = position_dodge(width = 0.9), vjust = -0.5, size = 3, fontface = "bold") +
  scale_fill_manual(values = c("Mean" = "blue", "Median" = "brown"), name = "Statistic") +
  labs(
    title = "Mean vs Median Attainment 8 Scores",
    subtitle = "Statistical comparison by region",
    x = "Region",
    y = "Attainment 8 Score"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    legend.position = "bottom"
  )


# 5. Postcode Area Breakdown
postcode_summary <- school_data %>%
  group_by(Region, postcode_area) %>%
  summarise(
    Count = n(),
    Schools = n_distinct(SchoolName),
    Mean_ATT8 = round(mean(Attainment8Score, na.rm = TRUE), 2),
    Median_ATT8 = round(median(Attainment8Score, na.rm = TRUE), 2),
    .groups = 'drop'
  ) %>%
  filter(Schools >= 5) %>%  # Only include areas with sufficient schools
  arrange(Region, desc(Mean_ATT8))

p5 <- ggplot(postcode_summary, aes(x = reorder(postcode_area, Mean_ATT8), 
                                   y = Mean_ATT8, fill = Region)) +
  geom_col(alpha = 0.8) +
  geom_text(aes(label = round(Mean_ATT8, 1)), 
            hjust = -0.1, size = 3) +
  scale_fill_manual(values = yorkshire_colors, name = "Region") +
  labs(
    title = "Average Attainment 8 Score by Postcode Area",
    subtitle = "Breakdown within each Yorkshire region",
    x = "Postcode Area",
    y = "Average Attainment 8 Score"
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
  select(Region, Mean_ATT8, Median_ATT8, Q1_ATT8, Q3_ATT8) %>%
  pivot_longer(cols = c(Mean_ATT8, Median_ATT8, Q1_ATT8, Q3_ATT8), 
               names_to = "Performance_Metric", values_to = "Score") %>%
  mutate(Performance_Metric = case_when(
    Performance_Metric == "Mean_ATT8" ~ "Mean",
    Performance_Metric == "Median_ATT8" ~ "Median",
    Performance_Metric == "Q1_ATT8" ~ "Q1 (25th %ile)",
    Performance_Metric == "Q3_ATT8" ~ "Q3 (75th %ile)"
  ))

p6 <- ggplot(comprehensive_comparison, aes(x = Performance_Metric, y = Score, fill = Region)) +
  geom_col(position = "dodge", alpha = 0.8, width = 0.7) +
  geom_text(aes(label = round(Score, 1)), 
            position = position_dodge(width = 0.7), vjust = -0.3, size = 3.5, fontface = "bold") +
  scale_fill_manual(values = yorkshire_colors, name = "Region") +
  labs(
    title = "Comprehensive School Performance Comparison",
    subtitle = "South Yorkshire vs West Yorkshire - All Statistical Measures",
    x = "Performance Metrics",
    y = "Attainment 8 Score",
    caption = "Comparison of key statistical measures of school performance"
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
  coord_cartesian(ylim = c(0, max(comprehensive_comparison$Score) * 1.15))


# 7. Performance Grade Distribution
performance_grades <- school_data %>%
  mutate(
    Performance_Grade = case_when(
      Attainment8Score >= 60 ~ "Excellent (60+)",
      Attainment8Score >= 50 & Attainment8Score < 60 ~ "Good (50-59)",
      Attainment8Score >= 40 & Attainment8Score < 50 ~ "Satisfactory (40-49)",
      Attainment8Score >= 30 & Attainment8Score < 40 ~ "Below Average (30-39)",
      Attainment8Score < 30 ~ "Poor (<30)"
    ),
    Performance_Grade = factor(Performance_Grade, levels = c("Poor (<30)", "Below Average (30-39)", 
                                                             "Satisfactory (40-49)", "Good (50-59)", "Excellent (60+)"))
  ) %>%
  group_by(Region, Performance_Grade) %>%
  summarise(Count = n(), .groups = 'drop') %>%
  group_by(Region) %>%
  mutate(Percentage = round(Count / sum(Count) * 100, 1))

p7 <- ggplot(performance_grades, aes(x = Performance_Grade, y = Percentage, fill = Region)) +
  geom_col(position = "dodge", alpha = 0.8) +
  geom_text(aes(label = paste0(Percentage, "%")), 
            position = position_dodge(width = 0.9), vjust = -0.3, size = 3, fontface = "bold") +
  scale_fill_manual(values = yorkshire_colors, name = "Region") +
  labs(
    title = "School Performance Grade Distribution",
    subtitle = "Percentage of schools in each performance category",
    x = "Performance Grade (Attainment 8 Score)",
    y = "Percentage of Schools (%)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


# 8. Top Performing Schools
top_schools <- school_data %>%
  group_by(Region, SchoolName) %>%
  summarise(
    Avg_ATT8 = round(mean(Attainment8Score, na.rm = TRUE), 2),
    Years_Data = n(),
    .groups = 'drop'
  ) %>%
  group_by(Region) %>%
  slice_max(Avg_ATT8, n = 10) %>%
  arrange(Region, desc(Avg_ATT8))


# Create combined visualization
combined_plot <- grid.arrange(p1, p2, p3, p4, ncol = 2, nrow = 2)

# Display additional plots separately
print(p5)
print(p6)
print(p7)

# Statistical tests
cat("\n=== STATISTICAL ANALYSIS ===\n")

# Perform t-test for Attainment 8 scores
att8_test <- t.test(
  school_data$Attainment8Score[school_data$Region == "South Yorkshire"],
  school_data$Attainment8Score[school_data$Region == "West Yorkshire"]
)

cat("T-test for Attainment 8 Scores (South vs West Yorkshire):\n")
cat("p-value:", att8_test$p.value, "\n")
cat("South Yorkshire mean:", round(att8_test$estimate[1], 2), "\n")
cat("West Yorkshire mean:", round(att8_test$estimate[2], 2), "\n")
cat("Difference:", round(att8_test$estimate[1] - att8_test$estimate[2], 2), "\n")

# Mann-Whitney U test (non-parametric alternative)
wilcox_test <- wilcox.test(
  school_data$Attainment8Score[school_data$Region == "South Yorkshire"],
  school_data$Attainment8Score[school_data$Region == "West Yorkshire"]
)

cat("\nMann-Whitney U test (non-parametric):\n")
cat("p-value:", wilcox_test$p.value, "\n")

# Top performing schools analysis
cat("\n=== TOP PERFORMING SCHOOLS ===\n")
cat("Top 5 Schools in South Yorkshire:\n")
print(top_schools %>% filter(Region == "South Yorkshire") %>% head(5) %>% select(SchoolName, Avg_ATT8))

cat("\nTop 5 Schools in West Yorkshire:\n")
print(top_schools %>% filter(Region == "West Yorkshire") %>% head(5) %>% select(SchoolName, Avg_ATT8))

# Save plots
ggsave("C:/Users/Hp/Desktop/DataScience_Prashant_Karki/Recommendation System/Graphs/Yorkshire_School_Comparison.png", combined_plot, 
       width = 16, height = 12, dpi = 300)
ggsave("C:/Users/Hp/Desktop/DataScience_Prashant_Karki/Recommendation System/Graphs/Yorkshire_School_Postcodes.png", p5, 
       width = 12, height = 8, dpi = 300)
ggsave("C:/Users/Hp/Desktop/DataScience_Prashant_Karki/Recommendation System/Graphs/Yorkshire_School_Comprehensive.png", p6, 
       width = 14, height = 10, dpi = 300)
ggsave("C:/Users/Hp/Desktop/DataScience_Prashant_Karki/Recommendation System/Graphs/Yorkshire_School_Grades.png", p7, 
       width = 12, height = 8, dpi = 300)

# Save summary data
write_csv(summary_stats, "C:/Users/Hp/Desktop/DataScience_Prashant_Karki/Cleaned_datasets/cleaned_schooldata/Yorkshire_School_Summary.csv")
write_csv(postcode_summary, "C:/Users/Hp/Desktop/DataScience_Prashant_Karki/Cleaned_datasets/cleaned_schooldata/Yorkshire_School_Postcodes.csv")
write_csv(yearly_stats, "C:/Users/Hp/Desktop/DataScience_Prashant_Karki/Cleaned_datasets/cleaned_schooldata/Yorkshire_School_Yearly.csv")
write_csv(performance_grades, "C:/Users/Hp/Desktop/DataScience_Prashant_Karki/Cleaned_datasets/cleaned_schooldata/Yorkshire_School_Grades.csv")
write_csv(top_schools, "C:/Users/Hp/Desktop/DataScience_Prashant_Karki/Cleaned_datasets/cleaned_schooldata/Yorkshire_School_TopPerformers.csv")

cat("\n=== ANALYSIS COMPLETE ===\n")
cat("Visualizations saved to Graphs/ directory:\n")
cat("- Yorkshire_School_Comparison.png (Combined 4-panel view)\n")
cat("- Yorkshire_School_Postcodes.png (Postcode area breakdown)\n")
cat("- Yorkshire_School_Comprehensive.png (Grouped bar chart)\n")
cat("- Yorkshire_School_Grades.png (Performance grade distribution)\n")
cat("Summary data saved to Cleaned Data/ directory\n")