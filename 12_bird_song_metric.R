#Code for making my birnd song metric per site 
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

#Reading my Observations Data 
bird <- read.csv("final_bird_data_2025.csv")



#Looking at the nomura data 
nomura <- subset(bird, site == "Nomura")

unique(nomura$Common.Name)

#Cleaning my data set to remove overlaping recordings of the same bird 
thin_birdnet_data <- function(bird, time_threshold = 3) {
  data_sorted <- bird %>%
    arrange(Begin.Path, Common.Name, File.Offset..s.)
  
  # Grouping by Begin.Path and Common.Name
  thinned_data <- data_sorted %>%
    group_by(Begin.Path, Common.Name) %>%
    mutate(
      time_diff = c(Inf, diff(File.Offset..s.)), 
      keep = case_when(
        row_number() == 1 ~ TRUE, 
        time_diff > time_threshold ~ TRUE, 
        TRUE ~ FALSE 
      )
    ) %>%
    group_by(Begin.Path, Common.Name, 
             overlap_group = cumsum(time_diff > time_threshold | row_number() == 1)) %>%
    slice_sample(n = 1) %>% 
    ungroup() %>%
    select(-time_diff, -keep, -overlap_group)
  
  return(thinned_data)
}

bird_thinned <- thin_birdnet_data(bird, time_threshold = 3)

write.csv(bird_thinned, "bird_calls_data.csv")

# Function to calculate seconds of bird recordings per site
calculate_bird_seconds_per_site <- function(thinned_data, site_column = "site") {
  # Check if site column exists
  if(!site_column %in% names(thinned_data)) {
    stop(paste("Column", site_column, "not found in data. Available columns:", 
               paste(names(thinned_data), collapse = ", ")))
  }
  
  # Checking for additional columns and include them if they exist
  additional_cols <- c("site_type", "Monitor")
  available_additional_cols <- additional_cols[additional_cols %in% names(thinned_data)]
  
  #Creating Grouping Variables
  group_vars <- c(site_column, available_additional_cols)
  
  site_metrics <- thinned_data %>%
    group_by(across(all_of(group_vars))) %>%
    summarise(
      n_observations = n(),
      seconds_of_bird_sounds = n_observations * 3,
      n_species = n_distinct(Common.Name),
      .groups = 'drop'
    ) %>%
    arrange(desc(seconds_of_bird_sounds))
  
  return(site_metrics)
}


sec_bird_song_metric <- calculate_bird_seconds_per_site (bird_thinned)



write.csv(sec_bird_song_metric, "bioactivity.csv")
