#Wranggling and cleaning the April to May Data 

#House Keeping 
rm (list = ls())

#Setting My Working Directory 
setwd("~/Documents/Education /MSc_Ecology_Conservation_and_Evolution /Thesis/site_data")

#Loading Libraries 
library(tidyverse)
library(vegan)
library(ggplot2)
library(stringr)

#Reading my data sets 

#Reading the data
FOCG1_april <- read.delim("FOCG1_april_st.txt", 
                          stringsAsFactors = FALSE)

FOCG2_april <- read.delim("FOCG2_april_st.txt", 
                          stringsAsFactors = FALSE)

FOCG3_april <- read.delim("FOCG3_april_st.txt", 
                          stringsAsFactors = FALSE)

FOCG4_april <- read.delim("FOCG4_april_st.txt", 
                          stringsAsFactors = FALSE)

FOCG5_april <- read.delim("FOCG5_april_st.txt", 
                          stringsAsFactors = FALSE)

FOCG6_april <- read.delim("FOCG6_april_st.txt", 
                          stringsAsFactors = FALSE)

FOCG7_april <- read.delim("FOCG7_april_st.txt", 
                          stringsAsFactors = FALSE)

FOCG8_april <- read.delim("FOCG8_april_st.txt", 
                          stringsAsFactors = FALSE)

FOCG9_april <- read.delim("FOCG9_april_st.txt", 
                          stringsAsFactors = FALSE)

FOCG10_april <- read.delim("FOCG10_april_st.txt", 
                           stringsAsFactors = FALSE)

FOCG11_april <- read.delim("FOCG11_april_st.txt", 
                           stringsAsFactors = FALSE)

FOCG12_april <- read.delim("FOCG12_april_st.txt", 
                           stringsAsFactors = FALSE)

FOCG13_april <- read.delim("FOCG13_april_st.txt", 
                           stringsAsFactors = FALSE)

FOCG14_april <- read.delim("FOCG14_april_st.txt", 
                           stringsAsFactors = FALSE)

FOCG15_april <- read.delim("FOCG15_april_st.txt", 
                           stringsAsFactors = FALSE)


#Combining the data set 

all_data <- list (FOCG1_april, FOCG2_april, FOCG3_april, FOCG4_april, FOCG5_april, FOCG6_april, FOCG7_april, FOCG8_april,
                  FOCG9_april, FOCG10_april, FOCG11_april, FOCG12_april, FOCG13_april, FOCG14_april, FOCG15_april)


comb_df <- bind_rows(all_data)

#Extracting the Site name 
comb_df$site <- sub("D:/([^/\\\\]+).*", "\\1", comb_df$Begin.Path)

unique(comb_df$site)

# Extract the date - matches FOCG followed by any digit
comb_df$Date <- str_extract(comb_df$Begin.Path, "\\d{8}")
comb_df$Date <- as.Date(comb_df$Date, format="%Y%m%d")

unique(comb_df$Date)

# Extracting the Time
comb_df$Time <- sub(".*FOCG\\d+_\\d{8}_(\\d{6})\\.wav", "\\1", comb_df$Begin.Path)


# Format as proper time
comb_df$Time <- format(strptime(comb_df$Time, format="%H%M%S"), format="%H:%M:%S")

#Adding type to each site 

comb_df <- comb_df %>% 
  mutate(site_type = case_when(
    grepl("Mansion_House|Charterhouse|Wallbrooke_Whart", site) ~ "conventional",
    grepl("Weil|120_fenchurch|canon_bridge|Algate_School", site) ~ "intensive",
    grepl("Guildhall|Nomura|Wood_Street", site) ~ "extensive",
    grepl("Inner_Temple|St_Dunstans|Cleary_Garden|Barber_Surgens|Chrest_Church_Gregtrians", site) ~ "garden",
    TRUE ~ "other"
  ))

#Subsetting for the start and end dates so all devices have the same effort 

comb_df_e <- subset(
  comb_df,
  (Date > "2025-04-26" | (Date == "2025-04-26" & Time >= "00:00")) &
    (Date < "2025-05-19" | (Date == "2025-05-19" & Time <= "09:07"))
)


#subsetting the data set for only 80% confidence 


comb_df_hc <- subset(comb_df_e, Confidence > 0.8 )

unique(comb_df_hc$Common.Name)


#Cheking for Dates
dates <- unique(comb_df_hc$Date)

# Convert to Date format and sort
sorted_dates <- sort(as.Date(dates))

# View the sorted dates
print(sorted_dates)


#Extracting as a CSV 

write.csv(comb_df_hc, "bird_april_pro_data.csv")
