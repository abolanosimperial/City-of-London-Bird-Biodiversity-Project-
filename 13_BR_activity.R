#House Keeping 
rm (list = ls())

#Setting My Working Directory 
setwd("~/Documents/Education /MSc_Ecology_Conservation_and_Evolution /Thesis/Analysis")


#Loading Libraries 
library(tidyverse)
library(vegan)
library(ggplot2)
library(stringr)
library(dplyr)
library(lubridate)
library(ggsignif)
library(lme4)
library(glmmTMB)
library(MASS)
library(emmeans)


#Reading the bird calls data 

bird <- read.csv("bird_calls_data.csv")

unique(bird$Common.Name)

#Calculatiing Average calls per day for Gray and White Wagtail and Black Redstart 
# Filter for the three species of interest
target_species <- c("House Sparrow", "Common Swift", "Black Redstart", "Peregrine Falcon")
filtered_data <- bird[bird$Common.Name %in% target_species, ]

# Calculate total calls per site per species across entire study
total_calls_per_site <- filtered_data %>%
  group_by(Monitor, Common.Name) %>%
  summarise(total_calls = n(), .groups = 'drop')

# Add site types
total_calls_per_site <- total_calls_per_site %>% 
  mutate(site_type = case_when(
    Monitor %in% c("FOCG1", "FOCG4", "FOCG15") ~ "conventional",
    Monitor %in% c("FOCG6", "FOCG10", "FOCG9", "FOCG8") ~ "intensive",
    Monitor %in% c("FOCG5", "FOCG7", "FOCG3") ~ "extensive",
    Monitor %in% c("FOCG12", "FOCG13", "FOCG11", "FOCG2", "FOCG14") ~ "garden",
    TRUE ~ "other"
  ))

#Adding site Loc 
total_calls_per_site <- total_calls_per_site %>% 
  mutate(site_loc = case_when(
    Monitor %in% c("FOCG1", "FOCG4", "FOCG15", "FOCG6", "FOCG10", "FOCG9", "FOCG8", "FOCG5", "FOCG7", "FOCG3") ~ "roof",
    Monitor %in% c("FOCG12", "FOCG13", "FOCG11", "FOCG2", "FOCG14") ~ "garden",
    TRUE ~ "other"
  ))


br_total_calls <- subset(total_calls_per_site, Common.Name == "Black Redstart")

# Change reference level to garden
br_total_calls$site_type <- as.factor(br_total_calls$site_type)
br_total_calls$site_type <- relevel(br_total_calls$site_type, ref = "garden")



#Negative binomial 
m4 <- glm.nb(total_calls ~ site_type, data = br_total_calls)
summary(m4)

#Checking bewteen sites 
emmeans_results <- emmeans(m4, "site_type")
pairs(emmeans_results)


# Calculate average calls per day across the full 98-day period
avg_calls_per_day <- total_calls_per_site %>%
  mutate(avg_calls_per_day = total_calls / 98)

# Add site types
avg_calls_per_day <- avg_calls_per_day %>% 
  mutate(site_type = case_when(
    Monitor %in% c("FOCG1", "FOCG4", "FOCG15") ~ "conventional",
    Monitor %in% c("FOCG6", "FOCG10", "FOCG9", "FOCG8") ~ "intensive",
    Monitor %in% c("FOCG5", "FOCG7", "FOCG3") ~ "extensive",
    Monitor %in% c("FOCG12", "FOCG13", "FOCG11", "FOCG2", "FOCG14") ~ "garden",
    TRUE ~ "other"
  ))


#Adding site Loc 
avg_calls_per_day <- avg_calls_per_day %>% 
  mutate(site_loc = case_when(
    Monitor %in% c("FOCG1", "FOCG4", "FOCG15", "FOCG6", "FOCG10", "FOCG9", "FOCG8", "FOCG5", "FOCG7", "FOCG3") ~ "roof",
    Monitor %in% c("FOCG12", "FOCG13", "FOCG11", "FOCG2", "FOCG14") ~ "garden",
    TRUE ~ "other"
  ))

#Looking at black redstats 

br_avg_calls_per_day <- subset(avg_calls_per_day, Common.Name == "Black Redstart")

br_add <- read.csv("br_focg13.csv")

br_avg_calls_per_day <- rbind(br_avg_calls_per_day, br_add)

write.csv(br_avg_calls_per_day, "br_avg_calls_per_day.csv")

#Exploring the percentage of days they had vocalizations 
# Total number of study days
total_days <- 98

br_data <- subset(filtered_data, Common.Name == "Black Redstart")


 detections_br <- br_data %>%
  group_by(Monitor) %>%
  summarise(
    days_with_detections = n_distinct(Date),
    proportion_detected_days = days_with_detections / total_days
  )

unique(br_data$Date)

dates <- unique(br_data$Date)

# Convert to Date format and sort
sorted_dates <- sort(as.Date(dates))

# View the sorted dates
print(sorted_dates)


#Exploring House Sparrow 
hs_total_calls <- subset(total_calls_per_site, Common.Name == "House Sparrow")

#Exploring Swift  
s_total_calls <- subset(total_calls_per_site, Common.Name == "Common Swift")


#Exploring Peregrine Falcon 
pf_total_calls <- subset(total_calls_per_site, Common.Name == "Peregrine Falcon")

