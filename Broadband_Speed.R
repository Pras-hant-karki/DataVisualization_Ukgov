library(tidyverse)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(scales)

# Define color palette to replace viridis
yorkshire_colors <- c("South Yorkshire" = "brown", "West Yorkshire" = "navy")

# Set working directory 
setwd("C:/Users/Hp/Desktop/DataScience_Prashant_Karki")

# Read the cleaned broadband data
broadband_data <- read_csv("C:/Users/Hp/Desktop/DataScience_Prashant_Karki/Cleaned_datasets/cleaned_broadband/Cleaned_Broadband_Speed.csv", show_col_types = FALSE)

south_yorkshire_postcodes <- c("S", "DN")
west_yorkshire_postcodes <- c("BD", "LS", "WF", "HD", "HX")

# Filter and categorize data by Yorkshire regions
yorkshire_broadband <- broadband_data %>%
  filter(`postcode area` %in% c(south_yorkshire_postcodes, west_yorkshire_postcodes)) %>%
  mutate(
    Region = case_when(
      `postcode area` %in% south_yorkshire_postcodes ~ "South Yorkshire",
      `postcode area` %in% west_yorkshire_postcodes ~ "West Yorkshire",
      TRUE ~ "Other"
    )
  ) %>%
  filter(Region != "Other")

# Display basic statistics
cat("=== BROADBAND SPEED COMPARISON: SOUTH VS WEST YORKSHIRE ===\n\n")
cat("Data Summary:\n")
print(yorkshire_broadband %>% 
        group_by(Region) %>% 
        summarise(
          Count = n(),
          .groups = 'drop'
        ))

# Calculate summary statistics for both regions
summary_stats <- yorkshire_broadband %>%
  group_by(Region) %>%
  summarise(
    Count = n(),
    Avg_Download_Mean = round(mean(Avgdownload, na.rm = TRUE), 2),
    Avg_Download_Median = round(median(Avgdownload, na.rm = TRUE), 2),
    Avg_Download_SD = round(sd(Avgdownload, na.rm = TRUE), 2),
    Avg_Upload_Mean = round(mean(`Average upload speed (Mbit/s)`, na.rm = TRUE), 2),
    Avg_Upload_Median = round(median(`Average upload speed (Mbit/s)`, na.rm = TRUE), 2),
    Min_Download_Mean = round(mean(Mindownload, na.rm = TRUE), 2),
    Min_Upload_Mean = round(mean(`Minimum upload speed (Mbit/s)`, na.rm = TRUE), 2),
    .groups = 'drop'
  )

print(summary_stats)

# 1. Box Plot Comparison - Download Speeds
p1 <- ggplot(yorkshire_broadband, aes(x = Region, y = Avgdownload, fill = Region)) +
  geom_boxplot(alpha = 0.7, outlier.alpha = 0.3) +
  geom_violin(alpha = 0.3) +
  scale_fill_manual(values = yorkshire_colors, name = "Region") +
  labs(
    title = "Average Download Speed Comparison",
    subtitle = "South Yorkshire vs West Yorkshire",
    x = "Region",
    y = "Average Download Speed (Mbit/s)",
    caption = "Data source: Broadband Performance Data"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    legend.position = "none"
  ) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "green") +
  stat_summary(fun = mean, geom = "text", aes(label = paste("Mean:", round(..y.., 1))), 
               vjust = -1, color = "green", size = 3)


# 2. Box Plot Comparison - Upload Speeds
p2 <- ggplot(yorkshire_broadband, aes(x = Region, y = `Average upload speed (Mbit/s)`, fill = Region)) +
  geom_boxplot(alpha = 0.7, outlier.alpha = 0.3) +
  geom_violin(alpha = 0.3) +
  scale_fill_manual(values = yorkshire_colors, name = "Region") +
  labs(
    title = "Average Upload Speed Comparison",
    subtitle = "South Yorkshire vs West Yorkshire",
    x = "Region",
    y = "Average Upload Speed (Mbit/s)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    legend.position = "none"
  ) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "red") +
  stat_summary(fun = mean, geom = "text", aes(label = paste("Mean:", round(..y.., 1))), 
               vjust = -1, color = "red", size = 3)


# 3. Density Plot - Download Speeds Distribution
p3 <- ggplot(yorkshire_broadband, aes(x = Avgdownload, fill = Region)) +
  geom_density(alpha = 0.6) +
  scale_fill_manual(values = yorkshire_colors, name = "Region") +
  labs(
    title = "Download Speed Distribution",
    subtitle = "Density comparison between regions",
    x = "Average Download Speed (Mbit/s)",
    y = "Density"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    legend.position = "bottom"
  ) +
  geom_vline(data = summary_stats, aes(xintercept = Avg_Download_Mean, color = Region), 
             linetype = "dashed", linewidth = 1, show.legend = FALSE) +
  scale_color_manual(values = yorkshire_colors)


# 4. Bar Chart - Mean Speeds Comparison
mean_comparison <- summary_stats %>%
  select(Region, Avg_Download_Mean, Avg_Upload_Mean) %>%
  pivot_longer(cols = c(Avg_Download_Mean, Avg_Upload_Mean), 
               names_to = "Speed_Type", values_to = "Speed") %>%
  mutate(Speed_Type = case_when(
    Speed_Type == "Avg_Download_Mean" ~ "Download",
    Speed_Type == "Avg_Upload_Mean" ~ "Upload"
  ))

p4 <- ggplot(mean_comparison, aes(x = Region, y = Speed, fill = Speed_Type)) +
  geom_col(position = "dodge", alpha = 0.8) +
  geom_text(aes(label = paste(Speed, "Mbps")), 
            position = position_dodge(width = 0.9), vjust = -0.5, size = 3) +
  scale_fill_manual(values = c("Download" = "skyblue", "Upload" = "grey"), name = "Speed Type") +
  labs(
    title = "Mean Broadband Speeds Comparison",
    subtitle = "Download vs Upload speeds by region",
    x = "Region",
    y = "Speed (Mbit/s)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    legend.position = "bottom"
  )


# 5. Postcode Area Breakdown
postcode_summary <- yorkshire_broadband %>%
  group_by(Region, `postcode area`) %>%
  summarise(
    Count = n(),
    Avg_Download = round(mean(Avgdownload, na.rm = TRUE), 2),
    Avg_Upload = round(mean(`Average upload speed (Mbit/s)`, na.rm = TRUE), 2),
    .groups = 'drop'
  ) %>%
  arrange(Region, desc(Avg_Download))

p5 <- ggplot(postcode_summary, aes(x = reorder(`postcode area`, Avg_Download), 
                                   y = Avg_Download, fill = Region)) +
  geom_col(alpha = 0.8) +
  geom_text(aes(label = paste(Avg_Download, "Mbps")), hjust = -0.1, size = 3) +
  scale_fill_manual(values = yorkshire_colors, name = "Region") +
  labs(
    title = "Average Download Speed by Postcode Area",
    subtitle = "Breakdown within each Yorkshire region",
    x = "Postcode Area",
    y = "Average Download Speed (Mbit/s)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    legend.position = "bottom"
  ) +
  coord_flip()


# 6. Grouped Bar Chart - Comprehensive Comparison
comprehensive_comparison <- summary_stats %>%
  select(Region, Avg_Download_Mean, Avg_Upload_Mean, Min_Download_Mean, Min_Upload_Mean) %>%
  pivot_longer(cols = c(Avg_Download_Mean, Avg_Upload_Mean, Min_Download_Mean, Min_Upload_Mean), 
               names_to = "Speed_Metric", values_to = "Speed") %>%
  mutate(Speed_Metric = case_when(
    Speed_Metric == "Avg_Download_Mean" ~ "Avg Download",
    Speed_Metric == "Avg_Upload_Mean" ~ "Avg Upload",
    Speed_Metric == "Min_Download_Mean" ~ "Min Download",
    Speed_Metric == "Min_Upload_Mean" ~ "Min Upload"
  )) %>%
  mutate(Speed_Type = ifelse(grepl("Download", Speed_Metric), "Download", "Upload"),
         Metric_Type = ifelse(grepl("Avg", Speed_Metric), "Average", "Minimum"))

p6 <- ggplot(comprehensive_comparison, aes(x = Speed_Metric, y = Speed, fill = Region)) +
  geom_col(position = "dodge", alpha = 0.8, width = 0.7) +
  geom_text(aes(label = paste(round(Speed, 1), "Mbps")), 
            position = position_dodge(width = 0.7), vjust = -0.3, size = 3.5, fontface = "bold") +
  scale_fill_manual(values = yorkshire_colors, name = "Region") +
  labs(
    title = "Comprehensive Broadband Speed Comparison",
    subtitle = "South Yorkshire vs West Yorkshire - All Metrics",
    x = "Speed Metrics",
    y = "Speed (Mbit/s)",
    caption = "Comparison of average and minimum download/upload speeds"
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
    panel.grid.major = element_line(color = "orange", linewidth = 0.5),
    panel.grid.minor = element_blank()
  ) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 8)) +
  coord_cartesian(ylim = c(0, max(comprehensive_comparison$Speed) * 1.15))
p6

# 7. Side-by-side Region Comparison with Statistics
region_stats_comparison <- summary_stats %>%
  select(Region, Count, Avg_Download_Mean, Avg_Download_Median, Avg_Download_SD, 
         Avg_Upload_Mean, Avg_Upload_Median) %>%
  pivot_longer(cols = c(Avg_Download_Mean, Avg_Download_Median, Avg_Upload_Mean, Avg_Upload_Median), 
               names_to = "Statistic", values_to = "Value") %>%
  mutate(Statistic = case_when(
    Statistic == "Avg_Download_Mean" ~ "Download Mean",
    Statistic == "Avg_Download_Median" ~ "Download Median", 
    Statistic == "Avg_Upload_Mean" ~ "Upload Mean",
    Statistic == "Avg_Upload_Median" ~ "Upload Median"
  ))

p7 <- ggplot(region_stats_comparison, aes(x = Statistic, y = Value, fill = Region)) +
  geom_col(position = "dodge", alpha = 0.8) +
  geom_text(aes(label = paste(round(Value, 1))), 
            position = position_dodge(width = 0.9), vjust = -0.3, size = 3, fontface = "bold") +
  scale_fill_manual(values = yorkshire_colors, name = "Region") +
  labs(
    title = "Statistical Measures Comparison",
    subtitle = "Mean vs Median speeds by region",
    x = "Statistical Measures",
    y = "Speed (Mbit/s)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


# Create combined visualization
combined_plot <- grid.arrange(p1, p2, p3, p4, ncol = 2, nrow = 2)

# Display additional plots separately
print(p5)
print(p6)
print(p7)

# Statistical tests
cat("\n=== STATISTICAL ANALYSIS ===\n")

# Perform t-test for download speeds
download_test <- t.test(
  yorkshire_broadband$Avgdownload[yorkshire_broadband$Region == "South Yorkshire"],
  yorkshire_broadband$Avgdownload[yorkshire_broadband$Region == "West Yorkshire"]
)

cat("T-test for Download Speeds (South vs West Yorkshire):\n")
cat("p-value:", download_test$p.value, "\n")
cat("South Yorkshire mean:", round(download_test$estimate[1], 2), "Mbps\n")
cat("West Yorkshire mean:", round(download_test$estimate[2], 2), "Mbps\n")
cat("Difference:", round(download_test$estimate[1] - download_test$estimate[2], 2), "Mbps\n")

# Perform t-test for upload speeds
upload_test <- t.test(
  yorkshire_broadband$`Average upload speed (Mbit/s)`[yorkshire_broadband$Region == "South Yorkshire"],
  yorkshire_broadband$`Average upload speed (Mbit/s)`[yorkshire_broadband$Region == "West Yorkshire"]
)

cat("\nT-test for Upload Speeds (South vs West Yorkshire):\n")
cat("p-value:", upload_test$p.value, "\n")
cat("South Yorkshire mean:", round(upload_test$estimate[1], 2), "Mbps\n")
cat("West Yorkshire mean:", round(upload_test$estimate[2], 2), "Mbps\n")
cat("Difference:", round(upload_test$estimate[1] - upload_test$estimate[2], 2), "Mbps\n")

# Save plots
ggsave("C:/Users/Hp/Desktop/DataScience_Prashant_Karki/Recommendation System/Graphs/Yorkshire_Broadband_Comparison.png", combined_plot, 
       width = 16, height = 12, dpi = 300)
ggsave("C:/Users/Hp/Desktop/DataScience_Prashant_Karki/Recommendation System/Graphs/Yorkshire_Postcode_Breakdown.png", p5, 
       width = 12, height = 8, dpi = 300)
ggsave("C:/Users/Hp/Desktop/DataScience_Prashant_Karki/Recommendation System/Graphs/Yorkshire_Comprehensive_Comparison.png", p6, 
       width = 14, height = 10, dpi = 300)
ggsave("C:/Users/Hp/Desktop/DataScience_Prashant_Karki/Recommendation System/Graphs/Yorkshire_Statistical_Comparison.png", p7, 
       width = 12, height = 8, dpi = 300)

# Save summary data
write_csv(summary_stats, "C:/Users/Hp/Desktop/DataScience_Prashant_Karki/Cleaned_datasets/cleaned_broadband/Yorkshire_Broadband_Summary.csv")
write_csv(postcode_summary, "C:/Users/Hp/Desktop/DataScience_Prashant_Karki/Cleaned_datasets/cleaned_LSOA/Yorkshire_Postcode_Summary.csv")

cat("\n=== ANALYSIS COMPLETE ===\n")
cat("Visualizations saved to Graphs/ directory:\n")
cat("- Yorkshire_Broadband_Comparison.png (Combined 4-panel view)\n")
cat("- Yorkshire_Postcode_Breakdown.png (Postcode area breakdown)\n")
cat("- Yorkshire_Comprehensive_Comparison.png (Grouped bar chart)\n")
cat("- Yorkshire_Statistical_Comparison.png (Statistical measures)\n")
cat("Summary data saved to Cleaned Data/ directory\n")