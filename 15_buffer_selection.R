#Buffer selection process for each layer 
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

#Reading my data for species richness 

species_richness_data <- read.csv ("bioactivity_data.csv")

species_richness_data <- species_richness_data %>% 
  select (Monitor, n_species)

#Adding site type 
species_richness_data <- species_richness_data %>% 
  mutate(site_type = case_when(
    Monitor %in% c("FOCG1", "FOCG4", "FOCG15") ~ "conventional",
    Monitor %in% c("FOCG6", "FOCG10", "FOCG9", "FOCG8") ~ "intensive",
    Monitor %in% c("FOCG5", "FOCG7", "FOCG3") ~ "extensive",
    Monitor %in% c("FOCG12", "FOCG13", "FOCG11", "FOCG2", "FOCG14") ~ "garden",
    TRUE ~ "other"
  ))


#Now reading my layers with different buffers 
#Reading lsm data 
lsm_layer1_150 <- read.csv("lsm_layer1.1_150.csv")

lsm_layer1_175 <- read.csv("lsm_layer1.1_175.csv")

lsm_layer1_200 <- read.csv("lsm_layer1.1_200.csv")

lsm_layer2_150 <- read.csv("lsm_layer2.2_150.csv")

lsm_layer2_175 <- read.csv("lsm_layer2.2_175.csv")

lsm_layer2_200 <- read.csv("lsm_layer2.2_200.csv")

lsm_layer3_150 <- read.csv("lsm_layer3.3_150.csv")

lsm_layer3_175 <- read.csv("lsm_layer3.3_175.csv")

lsm_layer3_200 <- read.csv("lsm_layer3.3_200.csv")

#Exploring Layer 1 
#Buffer 150 meters
layer1_150 <- merge(species_richness_data, lsm_layer1_150, by = "Monitor")

#Model 1: Layer 1 at 150 buffer
m1 <- glm(n_species ~ C150.enn_cv, data = layer1_150, family = poisson)
summary(m1)

par(mfrow=c(2,2)) 
plot(m1)


sum(cooks.distance(m1)>1)

AIC(m1)

#Buffer 175 meters
layer1_175 <- merge(species_richness_data, lsm_layer1_175, by = "Monitor")

#Model 2: Layer 1 at 175 buffer
m2 <- glm(n_species ~ C175.enn_cv, data = layer1_175, family = poisson)
summary(m2)

par(mfrow=c(2,2)) 
plot(m2)


sum(cooks.distance(m2)>1)

AIC(m2)

#Buffer 200 meters
layer1_200 <- merge(species_richness_data, lsm_layer1_200, by = "Monitor")

#Model 3: Layer 1 at 200 buffer
m3 <- glm(n_species ~ C200.enn_cv, data = layer1_200, family = poisson)
summary(m3)

par(mfrow=c(2,2)) 
plot(m3)


sum(cooks.distance(m3)>1)

AIC(m3)

#Exploring Layer 2 
#Buffer 150 meters
layer2_150 <- merge(species_richness_data, lsm_layer2_150, by = "Monitor")

#Model 4: Layer 2 at 150 buffer
m4 <- glm(n_species ~ C150.enn_cv + site_type, data = layer2_150, family = poisson)
summary(m4)

par(mfrow=c(2,2)) 
plot(m4)


sum(cooks.distance(m4)>1)

AIC(m4)

#Buffer 175 meters
layer2_175 <- merge(species_richness_data, lsm_layer2_175, by = "Monitor")

#Model 5: Layer 2 at 175 buffer
m5 <- glm(n_species ~ C175.enn_cv + site_type, data = layer2_175, family = poisson)
summary(m5)

par(mfrow=c(2,2)) 
plot(m5)


sum(cooks.distance(m5)>1)

AIC(m5)

#Buffer 200 meters
layer2_200 <- merge(species_richness_data, lsm_layer2_200, by = "Monitor")

#Model 6: Layer 2 at 200 buffer
m6 <- glm(n_species ~ C200.enn_cv + site_type, data = layer2_200, family = poisson)
summary(m6)

par(mfrow=c(2,2)) 
plot(m6)


sum(cooks.distance(m6)>1)

AIC(m6)

#Exploring Layer 3
#Buffer 150 meters
layer3_150 <- merge(species_richness_data, lsm_layer3_150, by = "Monitor")

#Model 7: Layer 3 at 150 buffer
m7 <- glm(n_species ~ C150.enn_cv + site_type, data = layer3_150, family = poisson)
summary(m7)

par(mfrow=c(2,2)) 
plot(m7)


sum(cooks.distance(m7)>1)

AIC(m7)

#Buffer 175 meters
layer3_175 <- merge(species_richness_data, lsm_layer3_175, by = "Monitor")

#Model 8: Layer 3 at 175 buffer
m8 <- glm(n_species ~ C175.enn_cv + site_type, data = layer3_175, family = poisson)
summary(m8)

par(mfrow=c(2,2)) 
plot(m8)


sum(cooks.distance(m8)>1)

AIC(m8)

#Buffer 200 meters
layer3_200 <- merge(species_richness_data, lsm_layer3_200, by = "Monitor")

#Model 9: Layer 3 at 200 buffer
m9 <- glm(n_species ~ C200.enn_cv + site_type, data = layer3_200, family = poisson)
summary(m9)

par(mfrow=c(2,2)) 
plot(m9)


sum(cooks.distance(m9)>1)

AIC(m9)
