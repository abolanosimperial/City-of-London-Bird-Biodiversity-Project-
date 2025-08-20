#Doing Data Validation 

#House Keeping 
rm (list = ls())

#Setting My Working Directory 
setwd("~/Documents/Education /MSc_Ecology_Conservation_and_Evolution /Thesis/site_data")

#Loading Libraries 
library(tidyverse)
library(vegan)
library(ggplot2)
library(stringr)
library(dplyr)

#reading february to march data set 

bird_feb <- read_csv("bird_feb_pro_data.csv")


#reading my march to april data set 

bird_march <- read_csv("bird_march_pro_data.csv")

#reading my april to may data set 

bird_april <- read_csv("bird_april_pro_data.csv")

#reading my may to June data 
bird_may <- read_csv("bird_may_pro_data.csv")


#binding both data sets

bird <- rbind(bird_feb, bird_march, bird_april, bird_may)



#Making the Community Matrix 
community_matrix <- bird %>%
  # Group by site and species
  group_by(site, Common.Name) %>%
  # Count observations
  summarize(count = n(), .groups = "drop") %>%
  # Reshape to wide format with sites as rows and species as columns
  pivot_wider(
    id_cols = site,
    names_from = Common.Name,
    values_from = count,
    values_fill = 0  
  ) 

#Adding Site Type 
community_matrix <- community_matrix %>% 
  mutate(site_type = case_when(
    grepl("Mansion_House|Charterhouse|Wallbrooke_Whart", site) ~ "conventional",
    grepl("Weil|120_fenchurch|canon_bridge|Algate_School", site) ~ "intensive",
    grepl("Guildhall|Nomura|Wood_Street", site) ~ "extensive",
    grepl("Inner_Temple|St_Dunstans|Cleary_Garden|Barber_Surgens|Chrest_Church_Gregtrians", site) ~ "garden",
    TRUE ~ "other"
  ))

#Getting species totals to remove the ones that are too rare to count 
species_totals <- community_matrix %>%
  summarise_if(is.numeric, sum)

totals <- pivot_longer(species_totals, 
             cols = everything(), 
             names_to = "Common.Names",
             values_to = "Total_Count")

refined <- subset(totals, Total_Count > 1)


#Subseting the bird 
refined_bird <- bird %>%
  filter(Common.Name %in% refined$Common.Names)

unique(refined_bird$Common.Name)

#Using dyplar to get random recordings for validation
validation <- refined_bird %>%
  group_by(Common.Name) %>%
  slice_sample(n = 20) %>%  
  ungroup()

unique(validation$Common.Name)


#Writing my CSV File for my Validation Data Set 

write.csv(validation, "validation_final.csv")


#Subsetting for recordings that need to be validated again at 0.9 Confidence 

validation0.9 <- subset(refined_bird, Species.Code %in% c("comswi", "eurcoo", "perfal", "redwin", "sonthr1", "tawowl1"))

unique(validation0.9$Common.Name)

validation0.9 <- subset(validation0.9, Confidence > 0.9)

#Getting 20 random salmples with 0.9 confidence per species 
validation2.0 <- validation0.9 %>%
  group_by(Common.Name) %>%
  slice_sample(n = 20) %>%  
  ungroup()

write.csv(validation2.0, "validation_2.0.csv")



#Reading my CSV that has the species I need to remove: 

species_delete <- read_csv("species_to_remove.csv")

#Deleting the species from my data set 
final_bird_data_2025 <- anti_join(refined_bird, species_delete, by = "Species.Code")

#Reading my validation Data set and deleting all recordings that I said were FALSE

validated_data <- read.csv("validated_random_select.csv")

# Create list of recordings to remove (from validation data)
recordings_to_remove <- validated_data %>%
  filter(validation %in% c("FALSE", "RM")) %>%
  select(Begin.Path, File.Offset..s., Species.Code)

# Remove from cleaned data
final_bird_data_2025 <- final_bird_data_2025 %>%
  anti_join(recordings_to_remove, 
            by = c("Begin.Path", "File.Offset..s.", "Species.Code"))



#Getting only 0.9 recordings for Swifts and Falcons 

final_bird_data_2025 <- final_bird_data_2025 %>%
  filter(
    (!Species.Code %in% c("comswi", "perfal")) |
      (Species.Code %in% c("comswi", "perfal") & Confidence > 0.9)
  )

#Confirming My Code Worked
swift <- subset(final_bird_data_2025, Species.Code == "comswi")

#Reading data for Coot recordings I want to keep 

coot_true_rec <- read.csv("coot_t_validation.csv")


#Getting only validated TRUE recordings for Coot 
final_bird_data_2025 <- final_bird_data_2025 %>%
  filter(
    Species.Code != "eurcoo" |
      (Species.Code == "eurcoo" & 
         paste(Begin.Path, File.Offset..s., Species.Code) %in% 
         paste(coot_true_rec$Begin.Path, coot_true_rec$File.Offset..s., coot_true_rec$Species.Code))
  )

#Testing the code 
coot <- subset(final_bird_data_2025, Species.Code == "eurcoo")

#Cleaning up my data set: 
#Correcting the Roof Type Aldgate is 

final_bird_data_2025 <- final_bird_data_2025 %>%
  mutate(site_type = ifelse(site_type == "other", "intensive", site_type))

#checking my code worked
unique(final_bird_data_2025$site)

aldgate <- subset(final_bird_data_2025, site == "Algate_School")

unique(final_bird_data_2025$site_type)

#Fixing the site names
final_bird_data_2025 <- final_bird_data_2025 %>%
  mutate(site = ifelse(site == "Mansion_House", "Mansion House", site))

final_bird_data_2025 <- final_bird_data_2025 %>%
  mutate(site = ifelse(site == "Barber_Surgens", "Barber Surgeon", site))

final_bird_data_2025 <- final_bird_data_2025 %>%
  mutate(site = ifelse(site == "Wood_Street", "Wood Street", site))

final_bird_data_2025 <- final_bird_data_2025 %>%
  mutate(site = ifelse(site == "Charterhouse", "79 Charterhouse", site))


final_bird_data_2025 <- final_bird_data_2025 %>%
  mutate(site = ifelse(site == "Algate_School", "Aldgate School", site))

final_bird_data_2025 <- final_bird_data_2025 %>%
  mutate(site = ifelse(site == "120_fenchurch", "120 Fenchurch", site))

final_bird_data_2025 <- final_bird_data_2025 %>%
  mutate(site = ifelse(site == "Cleary_Garden", "Cleary Garden", site))

final_bird_data_2025 <- final_bird_data_2025 %>%
  mutate(site = ifelse(site == "Inner_Temple", "Inner Temple", site))

final_bird_data_2025 <- final_bird_data_2025 %>%
  mutate(site = ifelse(site == "St_Dunstans", "St Dunstan", site))

final_bird_data_2025 <- final_bird_data_2025 %>%
  mutate(site = ifelse(site == "Chrest_Church_Gregtrians", "Christchurch Greyfairs", site))


final_bird_data_2025 <- final_bird_data_2025 %>%
  mutate(site = ifelse(site == "Wallbrooke_Whart", "Walbrooke Wharf", site))

final_bird_data_2025 <- final_bird_data_2025 %>%
  mutate(site = ifelse(site == "canon_bridge", "Cannon Bridge", site))

#Checking my code worked 
unique(final_bird_data_2025$site)

#Adding Monitor ID to my data set 
final_bird_data_2025 <- final_bird_data_2025 %>%
  mutate(Monitor = case_when(
    site == "Mansion House" ~ "FOCG1",
    site == "Barber Surgeon" ~ "FOCG2",
    site == "Wood Street" ~ "FOCG3",
    site == "79 Charterhouse" ~ "FOCG4",
    site == "Guildhall" ~ "FOCG5",
    site == "Weil" ~ "FOCG6",
    site == "Nomura" ~ "FOCG7",
    site == "Aldgate School" ~ "FOCG8",
    site == "120 Fenchurch" ~ "FOCG10",
    site == "Cleary Garden" ~ "FOCG11",
    site == "Inner Temple" ~ "FOCG12",
    site == "St Dunstan" ~ "FOCG13",
    site == "Christchurch Greyfairs" ~ "FOCG14",
    site == "Walbrooke Wharf" ~ "FOCG15",
    site == "Cannon Bridge" ~ "FOCG9",
    TRUE ~ NA_character_  # For any sites not listed above
  ))

# Checking my code 
unique(final_bird_data_2025$Monitor)


#Exporting my validated data set 2025 

write.csv(final_bird_data_2025, "final_bird_data_2025.csv")




