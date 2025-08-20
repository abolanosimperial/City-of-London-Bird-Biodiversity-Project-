#Processing May to June Data 

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
FOCG1_may <- read.delim("FOCG1_may_st.txt", 
                          stringsAsFactors = FALSE)

FOCG2_may <- read.delim("FOCG2_may_st.txt", 
                          stringsAsFactors = FALSE)

FOCG3_may <- read.delim("FOCG3_may_st.txt", 
                          stringsAsFactors = FALSE)

FOCG4_may <- read.delim("FOCG4_may_st.txt", 
                          stringsAsFactors = FALSE)

FOCG5_may <- read.delim("FOCG5_may_st.txt", 
                          stringsAsFactors = FALSE)

FOCG6_may <- read.delim("FOCG6_may_st.txt", 
                          stringsAsFactors = FALSE)

FOCG7_may <- read.delim("FOCG7_may_st.txt", 
                          stringsAsFactors = FALSE)

FOCG8_may <- read.delim("FOCG8_may_st.txt", 
                          stringsAsFactors = FALSE)

FOCG9_may <- read.delim("FOCG9_may_st.txt", 
                          stringsAsFactors = FALSE)

FOCG10_may <- read.delim("FOCG10_may_st.txt", 
                           stringsAsFactors = FALSE)

FOCG11_may <- read.delim("FOCG11_may_st.txt", 
                           stringsAsFactors = FALSE)

FOCG12_may <- read.delim("FOCG12_may_st.txt", 
                           stringsAsFactors = FALSE)

FOCG13_may <- read.delim("FOCG13_may_st.txt", 
                           stringsAsFactors = FALSE)

FOCG14_may <- read.delim("FOCG14_may_st.txt", 
                           stringsAsFactors = FALSE)

FOCG15_may <- read.delim("FOCG15_may_st.txt", 
                           stringsAsFactors = FALSE)


#Combining the data set 

all_data <- list (FOCG1_may, FOCG2_may, FOCG3_may, FOCG4_may, FOCG5_may, FOCG6_may, FOCG7_may, FOCG8_may,
                  FOCG9_may, FOCG10_may, FOCG11_may, FOCG12_may, FOCG13_may, FOCG14_may, FOCG15_may)


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
  (Date > "2025-05-24" | (Date == "2025-05-24" & Time >= "00:00")) &
    (Date < "2025-06-23" | (Date == "2025-06-23" & Time <= "09:20"))
)


#subsetting the data set for only 80% confidence 


comb_df_hc <- subset(comb_df_e, Confidence > 0.8 )

unique(comb_df_hc$Common.Name)

#Cheking Dates 
dates <- unique(comb_df_hc$Date)

# Convert to Date format and sort
sorted_dates <- sort(as.Date(dates))

# View the sorted dates
print(sorted_dates)



#Extracting as a CSV 

write.csv(comb_df_hc, "bird_may_pro_data.csv")



