# Read the daily activity data
dailyactivity <- read.csv("/Users/kentseng/Desktop/Google Certificate/Bellabeat_ Project_Fitabase Data /Bellabeat_User_Behavior_Analysis/csv_files/dailyActivity_merged.csv")

# Read the daily calories data
dailyclalories <- read.csv("/Users/kentseng/Desktop/Google Certificate/Bellabeat_ Project_Fitabase Data /Bellabeat_User_Behavior_Analysis/csv_files/dailyCalories_merged.csv")

# Read the daily steps data
dailysteps <- read.csv("/Users/kentseng/Desktop/Google Certificate/Bellabeat_ Project_Fitabase Data /Bellabeat_User_Behavior_Analysis/csv_files/dailySteps_merged.csv")

# Read the heart_rate data
heartrate <- read.csv("/Users/kentseng/Desktop/Google Certificate/Bellabeat_ Project_Fitabase Data /Bellabeat_User_Behavior_Analysis/csv_files/heartrate_seconds_merged.csv")

# Read the sleep day data
sleepday <- read.csv("/Users/kentseng/Desktop/Google Certificate/Bellabeat_ Project_Fitabase Data /Bellabeat_User_Behavior_Analysis/csv_files/update_sleepDay_merged.csv")

# Read the weight log info data
weightloginfo <- read.csv("/Users/kentseng/Desktop/Google Certificate/Bellabeat_ Project_Fitabase Data /Bellabeat_User_Behavior_Analysis/csv_files/update_weightLogInfo_merged.csv")

# Set enviroment
install.packages("ggplot2")
insatll.packages("tidyvers")
library(ggplot2)
library(tidyverse)
library(lubridate)

#Activity Intensity Distribution
dailyactivity %>%
  gather(key = "ActivityIntensity", value = "Minutes", VeryActiveMinutes, FairlyActiveMinutes, LightlyActiveMinutes, SedentaryMinutes) %>%
  ggplot(aes(x = Minutes)) +
  geom_histogram(bins = 30) +
  facet_wrap(~ActivityIntensity, scales = "free_x") +
  labs(title = "Distribution of Activity Intensity", 
       x = "Minutes", 
       y = "Frequency") +
  theme_minimal()

# Create the count plot
ggplot(weightloginfo, aes(x = IsManualReport, fill = as.factor(IsManualReport))) +
  geom_bar(color = "black") +
  scale_fill_manual(values = c("blue", "red"), name = "Is Manual Report",
                    breaks = c("TRUE", "FALSE"), labels = c("Yes", "No")) +
  theme_minimal() +
  labs(title = "Count of Manual Reports", x = "IsManualReport", y = "Count")  

# Weight Loss Over Time
avg_weight_over_time <- weightloginfo %>% 
  group_by(Date) %>% 
  summarise(AverageWeight = mean(WeightKg, na.rm = TRUE))

ggplot(avg_weight_over_time, aes(x = Date, y = AverageWeight)) + 
  geom_smooth(aes(group = 1)) +  # Added group = 1 inside aes() for geom_smooth
  labs(title = "Average Weight Over Time", 
       x = "Date", 
       y = "Average Weight (KG)") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# Calculate average sleep duration over time
avg_sleep_over_time <- sleepday %>% 
  group_by(Date) %>% 
  summarise(AverageSleep = mean(TotalMinutesAsleep, na.rm = TRUE))

# Plot the data
ggplot(avg_sleep_over_time, aes(x = Date, y = AverageSleep)) + 
  geom_smooth(aes(group = 1)) +  
  labs(title = "Average Sleep Duration Over Time", 
       x = "Date", 
       y = "Average Sleep (Minutes)") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))




