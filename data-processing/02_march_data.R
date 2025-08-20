#Analyzing and wrangling March to April Recordings 

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
FOCG1_march <- read.delim("FOCG1_march_st.txt", 
                        stringsAsFactors = FALSE)

FOCG2_march <- read.delim("FOCG2_march_st.txt", 
                          stringsAsFactors = FALSE)

FOCG3_march <- read.delim("FOCG3_march_st.txt", 
                          stringsAsFactors = FALSE)

FOCG4_march <- read.delim("FOCG4_march_st.txt", 
                          stringsAsFactors = FALSE)

FOCG5_march <- read.delim("FOCG5_march_st.txt", 
                          stringsAsFactors = FALSE)

FOCG6_march <- read.delim("FOCG6_march_st.txt", 
                          stringsAsFactors = FALSE)

FOCG7_march <- read.delim("FOCG7_march_st.txt", 
                          stringsAsFactors = FALSE)

FOCG8_march <- read.delim("FOCG8_march_st.txt", 
                          stringsAsFactors = FALSE)

FOCG9_march <- read.delim("FOCG9_march_st.txt", 
                          stringsAsFactors = FALSE)

FOCG10_march <- read.delim("FOCG10_march_st.txt", 
                          stringsAsFactors = FALSE)

FOCG11_march <- read.delim("FOCG11_march_st.txt", 
                          stringsAsFactors = FALSE)

FOCG12_march <- read.delim("FOCG12_march_st.txt", 
                          stringsAsFactors = FALSE)

FOCG13_march <- read.delim("FOCG13_march_st.txt", 
                          stringsAsFactors = FALSE)

FOCG14_march <- read.delim("FOCG14_march_st.txt", 
                          stringsAsFactors = FALSE)

FOCG15_march <- read.delim("FOCG15_march_st.txt", 
                          stringsAsFactors = FALSE)


#Combining the data set 

all_data <- list (FOCG1_march, FOCG2_march, FOCG3_march, FOCG4_march, FOCG5_march, FOCG6_march, FOCG7_march, FOCG8_march,
                  FOCG9_march, FOCG10_march, FOCG11_march, FOCG12_march, FOCG13_march, FOCG14_march, FOCG15_march)


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
    grepl("Weil|120_fenchurch|canon_bridge|Aldgate_School", site) ~ "intensive",
    grepl("Guildhall|Nomura|Wood_Street", site) ~ "extensive",
    grepl("Inner_Temple|St_Dunstans|Cleary_Garden|Barber_Surgens|Chrest_Church_Gregtrians", site) ~ "garden",
    TRUE ~ "other"
  ))

#Subsetting for the start and end dates so all devices have the same effort 

comb_df_e <- subset(
  comb_df,
  (Date > "2025-03-14" | (Date == "2025-03-14" & Time >= "00:00")) &
    (Date < "2025-04-13" | (Date == "2025-04-13" & Time <= "14:37"))
)


#subsetting the data set for only 80% confidence 


comb_df_hc <- subset(comb_df_e, Confidence > 0.8 )

unique(comb_df_hc$Common.Name)

#Extracting as a CSV 

write.csv(comb_df_hc, "bird_march_pro_data.csv")

unique(comb_df_hc$Date)

dates <- unique(comb_df_hc$Date)

# Convert to Date format and sort
sorted_dates <- sort(as.Date(dates))

# View the sorted dates
print(sorted_dates)


