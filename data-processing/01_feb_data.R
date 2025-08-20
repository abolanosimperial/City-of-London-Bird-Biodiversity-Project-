#Analyzing and wrangling February to March Recordings 

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
FOCG1_feb <- read.delim("FOCG1_feb_st.txt", 
                        stringsAsFactors = FALSE)

FOCG2_feb <- read.delim("FOCG2_feb_st.txt", 
                        stringsAsFactors = FALSE)

FOCG3_feb <- read.delim("FOCG3_feb_st.txt", 
                        stringsAsFactors = FALSE)

FOCG4_feb <- read.delim("FOCG4_feb_st.txt", 
                        stringsAsFactors = FALSE)

FOCG5_feb <- read.delim("FOCG5_feb_st.txt", 
                        stringsAsFactors = FALSE)

FOCG6_feb <- read.delim("FOCG6_feb_st.txt", 
                        stringsAsFactors = FALSE)

FOCG7_feb <- read.delim("FOCG7_feb_st.txt", 
                        stringsAsFactors = FALSE)

FOCG8_feb <- read.delim("FOCG8_feb_st.txt", 
                        stringsAsFactors = FALSE)

FOCG9_feb <- read.delim("FOCG9_feb_st.txt", 
                        stringsAsFactors = FALSE)

FOCG10_feb <- read.delim("FOCG10_feb_st.txt", 
                        stringsAsFactors = FALSE)

FOCG11_feb <- read.delim("FOCG11_feb_st.txt", 
                        stringsAsFactors = FALSE)

FOCG12_feb <- read.delim("FOCG12_feb_st.txt", 
                        stringsAsFactors = FALSE)

FOCG13_feb <- read.delim("FOCG13_feb_st.txt", 
                        stringsAsFactors = FALSE)

FOCG14_feb <- read.delim("FOCG14_feb_st.txt", 
                        stringsAsFactors = FALSE)

FOCG15_feb <- read.delim("FOCG15_feb_st.txt", 
                        stringsAsFactors = FALSE)


#Combining the data set 

all_data <- list (FOCG1_feb, FOCG2_feb, FOCG3_feb, FOCG4_feb, FOCG5_feb, FOCG6_feb, FOCG7_feb, FOCG8_feb,
                  FOCG10_feb, FOCG11_feb, FOCG12_feb, FOCG13_feb, FOCG14_feb, FOCG15_feb)


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

#Doing it for Cannon Bridge since the files were different 
# Extracting the Site name
FOCG9_feb$site <- sub(".*/Green_Roof_Data/([^/\\\\]+)/.*", "\\1", FOCG9_feb$Begin.Path)

# Extracting the Date
FOCG9_feb$Date <- sub(".*FOCG\\d+_(\\d{8})_.*", "\\1", FOCG9_feb$Begin.Path)
FOCG9_feb$Date <- as.Date(FOCG9_feb$Date, format="%Y%m%d")

# Extracting the Time
FOCG9_feb$Time <- sub(".*FOCG\\d+_\\d{8}_(\\d{6})\\.wav", "\\1", FOCG9_feb$Begin.Path)
# Format as proper time
FOCG9_feb$Time <- format(strptime(FOCG9_feb$Time, format="%H%M%S"), format="%H:%M:%S")


#Combining Cannon bridge with the rest of the data sets 

comb_df <- bind_rows(comb_df, FOCG9_feb)

unique(comb_df$site)

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

comb_df_e <- subset(comb_df, (Date > "2025-02-21" | (Date == "2025-02-21" & Time >= "15:27")) & 
                      (Date < "2025-03-04" | (Date == "2025-03-04" & Time <= "11:27")))

unique(comb_df_e$site)

#subsetting the data set for only 80% confidence 


comb_df_hc <- subset(comb_df_e, Confidence > 0.8 )

unique(comb_df_hc$Common.Name)

write.csv (comb_df_hc, "/Users/andresbolanos/Documents/Education /MSc_Ecology_Conservation_and_Evolution /Thesis/site_data/bird_feb_pro_data.csv")

#looking at the species at each site 

unique(comb_df_hc$Common.Name)





