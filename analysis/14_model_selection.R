#Model Selection Process to pick the best model 
#House Keeping 
rm (list = ls())

#Setting My Working Directory 
setwd("~/Documents/Education /MSc_Ecology_Conservation_and_Evolution /Thesis/Analysis")


#Loading Libraries 
library(tidyverse)
library(vegan)
library(ggplot2)
library(stringr)
library(RColorBrewer)
library(cowplot)

#Reading my Observations Data 

bird <- read.csv("final_bird_data_2025.csv")



# Making the Community Matrix as Presence-Absence (0/1)
presence_absence_matrix <- bird %>%
  # Group by site and species
  group_by(Monitor, Common.Name) %>%
  # We just need to know if species is present
  summarize(present = 1, .groups = "drop") %>%
  # Reshape to wide format with sites as rows and species as columns
  pivot_wider(
    id_cols = Monitor,
    names_from = Common.Name,
    values_from = present,
    values_fill = 0  
  )

#Adding garden type 
presence_absence_matrix <- presence_absence_matrix %>% 
  mutate(site_type = case_when(
    Monitor %in% c("FOCG1", "FOCG4", "FOCG15") ~ "conventional",
    Monitor %in% c("FOCG6", "FOCG10", "FOCG9", "FOCG8") ~ "intensive",
    Monitor %in% c("FOCG5", "FOCG7", "FOCG3") ~ "extensive",
    Monitor %in% c("FOCG12", "FOCG13", "FOCG11", "FOCG2", "FOCG14") ~ "garden",
    TRUE ~ "other"
  ))

#Making my first PCOA
pcoa <- cmdscale(vegdist(presence_absence_matrix[, !colnames(presence_absence_matrix) %in% c("Monitor", "site_type")], method = "bray"), k = 2, eig = TRUE)

pcoa_df <- data.frame(
  Monitor = presence_absence_matrix$Monitor,
  site_type = presence_absence_matrix$site_type,
  Axis1 = pcoa$points[,1],
  Axis2 = pcoa$points[,2]
)

#Reading my connectivity/ stepping stone metric 

stepping_stone <- read.csv("all_sites_95th_percentile_below_site_results.csv")

stepping_stone <- stepping_stone %>%
  mutate(metric = replace_na(metric, 27.10800))

roof_final_data <- merge(pcoa_df, stepping_stone, by = "Monitor")



#Cleaning up the data set to only include columns I want. 

roof_final_data <- roof_final_data %>% select(Monitor, site_type, Axis1, site_height, metric, dist_to_garden)


roof_final_data <- roof_final_data %>%
  rename(ss_metric = metric)

#Now reading my data for species richness and bird call activity per site 

bioactivity_data <- read.csv ("bioactivity_data.csv")

bioactivity_data <- bioactivity_data %>% 
  select (Monitor, seconds_of_bird_sounds, n_species)

roof_final_data <- merge (roof_final_data, bioactivity_data, by = "Monitor")




#Model 2: PCoA1 vs Stepping Stone Height Metric 
m2 <- glm(Axis1 ~ ss_metric, data = roof_final_data, family = gaussian)
summary(m2)

par(mfrow=c(2,2)) 
plot(m2)

AIC(m2)


#Model 4: PCoA1 vs Stepping Stone Metric with fixed factor site Type 
m4 <- glm(Axis1 ~ ss_metric + site_type, data = roof_final_data, family = gaussian)
summary(m4)

par(mfrow=c(2,2)) 
plot(m4)

sum(cooks.distance(m4)>1)

AIC(m4)



#Model 6: Species Richness vs Stepping Stone Metric 
m6 <- glm(n_species ~ ss_metric, data = roof_final_data, family = poisson)
summary(m6)

par(mfrow=c(2,2)) 
plot(m6)

sum(cooks.distance(m6)>1)

AIC(m6)

#Model 7: Species Richness vs Garden Edge with fixed effect site type
m7 <- glm(n_species ~ dist_to_garden + site_type, data = roof_final_data, family = poisson)
summary(m7)

par(mfrow=c(2,2)) 
plot(m7)

AIC(m7)

#Model 8: Species Richness vs Stepping Stone Metric with fixed effect site type 
m8 <- glm(n_species ~ ss_metric + site_type, data = roof_final_data, family = poisson)
summary(m8)

par(mfrow=c(2,2)) 
plot(m8)

AIC(m8)


#Done working on Models that only include Roof Sites 
#Now Getting data set for all sites. 
#Reading lsm data 

lsm_layer1_200 <- read.csv("lsm_layer1.1_200.csv")

lsm_layer2_175 <- read.csv("lsm_layer2.2_175.csv")

lsm_layer3_150 <- read.csv("lsm_layer3.3_150.csv")


#Making Data set for combining with layers 

lsm_data <- merge(bioactivity_data, pcoa_df)

#Merging Model comparison of Layer 1 

layer1_final_data  <- merge(lsm_data, lsm_layer1_200, by = "Monitor")

layer1_final_data <- layer1_final_data %>% 
  select (Monitor, seconds_of_bird_sounds, n_species, site_type, C200.enn_cv, C200.ca_m2, Axis1)

#Doing Analysis for Layer 1
#Model 13: Layer 1 PCoA1 vs Habitat Area 
m13 <- glm(Axis1 ~ C200.ca_m2, data = layer1_final_data, family = gaussian)
summary(m13)

par(mfrow=c(2,2)) 
plot(m13)


sum(cooks.distance(m13)>1)

AIC(m13)

#Model 14: Layer 1 PCoA1 vs Nerest Neighbor CV
m14 <- glm(Axis1 ~ C200.enn_cv, data = layer1_final_data, family = gaussian)
summary(m14)

par(mfrow=c(2,2)) 
plot(m14)


sum(cooks.distance(m14)>1)

AIC(m14)

#Model 15: Layer 1 PCoA1 vs Habitat Area with site type as fixed factor 
m15 <- glm(Axis1 ~ C200.ca_m2 + site_type, data = layer1_final_data, family = gaussian)
summary(m15)

par(mfrow=c(2,2)) 
plot(m15)


sum(cooks.distance(m15)>1)

AIC(m15)

#Model 16: Layer 1 PCoA1 vs Nerest Neighbor CV with site type as fixed factor 
m16 <- glm(Axis1 ~ C200.enn_cv + site_type, data = layer1_final_data, family = gaussian)
summary(m16)

par(mfrow=c(2,2)) 
plot(m16)


sum(cooks.distance(m16)>1)

AIC(m16)

#Model 17: Layer 1 Species Richness vs Habitat Area  
m17 <- glm(n_species ~ C200.ca_m2, data = layer1_final_data, family = poisson)
summary(m17)

par(mfrow=c(2,2)) 
plot(m17)


sum(cooks.distance(m17)>1)

AIC(m17)

#Model 18: Layer 1 Species Richness vs Nerest Neighbor CV  
m18 <- glm(n_species ~ C200.enn_cv , data = layer1_final_data, family = poisson)
summary(m18)

par(mfrow=c(2,2)) 
plot(m18)


sum(cooks.distance(m18)>1)

AIC(m18)

#Model 19: Layer 1 Species Richness vs Habitat Area with site type as mixed effect
m19 <- glm(n_species ~ C200.ca_m2 + site_type, data = layer1_final_data, family = poisson)
summary(m19)

par(mfrow=c(2,2)) 
plot(m19)


sum(cooks.distance(m19)>1)

AIC(m19)

#Model 20: Layer 1 Species Richness vs Nerest Neighbor CV with site type as fixed effect 
m20 <- glm(n_species ~ C200.enn_cv + site_type, data = layer1_final_data, family = poisson)
summary(m20)

par(mfrow=c(2,2)) 
plot(m20)


sum(cooks.distance(m20)>1)

AIC(m20)


#Now looking at Layer 2 
layer2_final_data  <- merge(lsm_data, lsm_layer2_175, by = "Monitor")

layer2_final_data <- layer2_final_data %>% 
  select (Monitor, seconds_of_bird_sounds, n_species, site_type, C175.enn_cv, C175.ca_m2, Axis1)

#Now doing Model Selection For Layer 2
#Model 25: Layer 2 PCoA1 vs Habitat Area 
m25 <- glm(Axis1 ~ C175.ca_m2, data = layer2_final_data, family = gaussian)
summary(m25)

par(mfrow=c(2,2)) 
plot(m25)


sum(cooks.distance(m25)>1)

AIC(m25)

#Model 26: Layer 2 PCoA1 vs Nerest Neighbor CV
m26 <- glm(Axis1 ~ C175.enn_cv, data = layer2_final_data, family = gaussian)
summary(m26)

par(mfrow=c(2,2)) 
plot(m26)


sum(cooks.distance(m26)>1)

AIC(m26)

#Model 27: Layer 2 PCoA1 vs Habitat Area with site type as fixed factor 
m27 <- glm(Axis1 ~ C175.ca_m2 + site_type, data = layer2_final_data, family = gaussian)
summary(m27)

par(mfrow=c(2,2)) 
plot(m27)


sum(cooks.distance(m27)>1)

AIC(m27)

#Model 28: Layer 2 PCoA1 vs Nerest Neighbor CV with site type as fixed factor 
m28 <- glm(Axis1 ~ C175.enn_cv + site_type, data = layer2_final_data, family = gaussian)
summary(m28)

par(mfrow=c(2,2)) 
plot(m28)


sum(cooks.distance(m28)>1)

AIC(m28)

#Model 29: Layer 2 Species Richness vs Habitat Area  
m29 <- glm(n_species ~ C175.ca_m2, data = layer2_final_data, family = poisson)
summary(m29)

par(mfrow=c(2,2)) 
plot(m29)


sum(cooks.distance(m29)>1)

AIC(m29)

#Model 30: Layer 2 Species Richness vs Nerest Neighbor CV  
m30 <- glm(n_species ~ C175.enn_cv , data = layer2_final_data, family = poisson)
summary(m30)

par(mfrow=c(2,2)) 
plot(m30)


sum(cooks.distance(m30)>1)

AIC(m30)

#Model 31: Layer 2 Species Richness vs Habitat Area with site type as mixed effect
m31 <- glm(n_species ~ C175.ca_m2 + site_type, data = layer2_final_data, family = poisson)
summary(m31)

par(mfrow=c(2,2)) 
plot(m31)


sum(cooks.distance(m31)>1)

AIC(m31)

#Model 32: Layer 2 Species Richness vs Nerest Neighbor CV with site type as fixed effect 
m32 <- glm(n_species ~ C175.enn_cv + site_type, data = layer2_final_data, family = poisson)
summary(m32)

par(mfrow=c(2,2)) 
plot(m32)


sum(cooks.distance(m32)>1)

AIC(m32)


#Now looking at Layer 3
layer3_final_data  <- merge(lsm_data, lsm_layer3_150, by = "Monitor")

layer3_final_data <- layer3_final_data %>% 
  select (Monitor, seconds_of_bird_sounds, n_species, site_type, C150.enn_cv, C150.ca_m2, Axis1)

#Now doing Model Selection For Layer 3
#Model 37: Layer 3 PCoA1 vs Habitat Area 
m37 <- glm(Axis1 ~ C150.ca_m2, data = layer3_final_data, family = gaussian)
summary(m37)

par(mfrow=c(2,2)) 
plot(m37)


sum(cooks.distance(m37)>1)

AIC(m37)

#Model 38: Layer 3 PCoA1 vs Nerest Neighbor CV
m38 <- glm(Axis1 ~ C150.enn_cv, data = layer3_final_data, family = gaussian)
summary(m38)

par(mfrow=c(2,2)) 
plot(m38)


sum(cooks.distance(m38)>1)

AIC(m38)

#Model 39: Layer 3 PCoA1 vs Habitat Area with site type as fixed factor 
m39 <- glm(Axis1 ~ C150.ca_m2 + site_type, data = layer3_final_data, family = gaussian)
summary(m39)

par(mfrow=c(2,2)) 
plot(m39)


sum(cooks.distance(m39)>1)

AIC(m39)

#Model 40: Layer 3 PCoA1 vs Nerest Neighbor CV with site type as fixed factor 
m40 <- glm(Axis1 ~ C150.enn_cv + site_type, data = layer3_final_data, family = gaussian)
summary(m40)

par(mfrow=c(2,2)) 
plot(m40)


sum(cooks.distance(m40)>1)

AIC(m40)

#Model 41: Layer 3 Species Richness vs Habitat Area  
m41 <- glm(n_species ~ C150.ca_m2, data = layer3_final_data, family = poisson)
summary(m41)

par(mfrow=c(2,2)) 
plot(m41)


sum(cooks.distance(m41)>1)

AIC(m41)

#Model 42: Layer 3 Species Richness vs Nerest Neighbor CV  
m42 <- glm(n_species ~ C150.enn_cv , data = layer3_final_data, family = poisson)
summary(m42)

par(mfrow=c(2,2)) 
plot(m42)


sum(cooks.distance(m42)>1)

AIC(m42)

#Model 43: Layer 3 Species Richness vs Habitat Area with site type as mixed effect
m43 <- glm(n_species ~ C150.ca_m2 + site_type, data = layer3_final_data, family = poisson)
summary(m43)

par(mfrow=c(2,2)) 
plot(m43)


sum(cooks.distance(m43)>1)

AIC(m43)

#Model 44: Layer 3 Species Richness vs Nerest Neighbor CV with site type as fixed effect 
m44 <- glm(n_species ~ C150.enn_cv + site_type, data = layer3_final_data, family = poisson)
summary(m44)

par(mfrow=c(2,2)) 
plot(m44)


sum(cooks.distance(m44)>1)

AIC(m44)





