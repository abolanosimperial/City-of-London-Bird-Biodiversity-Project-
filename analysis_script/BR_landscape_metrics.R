#Exploring Landscape Metrics Analisis 

#House Keeping 
rm (list = ls())

#Setting My Working Directory 
setwd("~/Documents/Education /MSc_Ecology_Conservation_and_Evolution /Thesis/Analysis")

library(tidyverse)
library(vegan)
library(ggplot2)
library(stringr)
library(dplyr)
library(MASS)
library(pscl)



#Reading Black Redstark Activity 
br_activity <-read.csv("br_avg_calls_per_day.csv")


#Reading My landscape Metrics 
lsm_layer1_200 <- read.csv("lsm_layer1.1_200.csv")

lsm_layer2_150 <- read.csv("lsm_layer2.2_150.csv")

lsm_layer3_200 <- read.csv("lsm_layer3.3_200.csv")


#Merging data sets

layer1_br <- merge( br_activity, lsm_layer1_200, by = "Monitor")

layer2_br <- merge(br_activity, lsm_layer2_150, by = "Monitor")

layer3_br <- merge(br_activity, lsm_layer3_200, by = "Monitor")



#Exploring Correlation models with area: They are all over disperesed so will try negative binomial 
#Layer 1 
m1 <- glm( total_calls ~ C200.ca_m2, data = layer1_br, family = poisson)
summary(m1)

par(mfrow=c(2,2)) 
plot(m1)

#Layer 2 
m2 <- glm (total_calls ~ C150.ca_m2, data = layer2_br, family = poisson)
summary(m2)

par(mfrow=c(2,2)) 
plot(m2)

#Layer 3
m3 <- glm (total_calls ~ C200.ca_m2, data = layer3_br, family = poisson)
summary(m3)

par(mfrow=c(2,2)) 
plot(m3)

#Trying negative binomial: Getting some Heteroscadicity going to try a zero inflated negative binomial: 
m4 <- glm.nb(total_calls ~ C200.ca_m2, data = layer1_br)
summary(m4)

plot(m4)

m7 <- glm.nb (total_calls ~ C150.ca_m2, data = layer2_br)
summary(m7)


m8 <- glm.nb (total_calls ~ C200.ca_m2, data = layer3_br)
summary(m8)

plot(m8)


#Plotting Model 8 to see how it looks 
#Plot
layer3_a_br_plot <- ggplot(data = layer3_br, aes(x = C200.enn_cv, y = total_calls)) +
  geom_point(size = 2.8, alpha = 0.65, color = "black") +
  geom_smooth(method = "lm", se = TRUE, 
              color = "blue", fill = "lightblue", alpha = 0.2,
              linewidth = 1.2) +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    axis.line = element_line(color = "black"),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    plot.title = element_text(face = "bold", size = 14)
  ) +
  labs(
    x = "Habitat area", 
    y = "Total Vocalizations",
    title = "3"
  )

plot(layer3_a_br_plot)

#Trying Zero Inflated Models 
m5 <- zeroinfl(total_calls ~ C200.ca_m2, data = layer1_br, dist = "negbin")
summary(m5)

#Trying to subset the data set exclude gardens

roofs_layer3_br <- subset(layer3_br, site_loc == "roof")


m9 <- glm.nb (total_calls ~ C200.ca_m2, data = roofs_layer3_br)
summary(m9)

par(mfrow=c(2,2)) 
plot(m9)

# Get predicted values from the model
preds_plot1 <- ggpredict(m9, terms = c("C200.ca_m2"))


# Plot with ggplot
plot1 <- ggplot(preds_plot1, aes(x = x, y = predicted)) +
  geom_line(color = "blue", size = 1.2) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), fill = "lightblue", alpha = 0.3) +
  geom_point(data = roofs_layer3_br, aes(x = C200.ca_m2, y = total_calls), size = 2.8, alpha = 0.65) +
  labs(x = "Habiatat area (m2)", y = "Total Vocalizations") +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    axis.line = element_line(color = "black"),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    plot.title = element_text(face = "bold", size = 14)
  )

print(plot1)



#Just doing a linear model 

m10 <- glm (total_calls ~ C200.ca_m2, data = roofs_layer3_br)
summary(m10)

par(mfrow=c(2,2)) 
plot(m10)



#Trying to log transform the habitat area 

m11 <- glm.nb(total_calls ~ log(C200.ca_m2), data = roofs_layer3_br)
summary(m11)

#Didn't really find a significant effect with area just in layer 3 with a negarive binomila but i really don't understand how 
#Interprit ecologically 
#Layer 1 
m12 <- glm (total_calls ~ C200.enn_cv, data = layer1_br)
summary(m12)

par(mfrow=c(2,2)) 
plot(m12)

#Layer 2
m13 <- glm (total_calls ~ C150.enn_cv, data = layer2_br)
summary(m13)

plot(m13)


#layer 3 
m14 <- glm(total_calls ~ C200.enn_cv, data = layer3_br)
summary(m14)

plot(m14)


#Subsetting for only roofs
roofs_layer1_br <- subset(layer1_br, site_loc == "roof")

roofs_layer2_br <- subset(layer2_br, site_loc == "roof")

#Doing models for only roof sites 
#Layer 1
m15 <- glm(total_calls ~ C200.enn_cv, data = roofs_layer1_br)
summary(m15)

plot(m15)

#Layer 2 
m16 <- glm(total_calls ~ C150.enn_cv, data = roofs_layer2_br)
summary (m16)


#Layer 3 
m17 <- glm(total_calls ~ C200.enn_cv, data = roofs_layer3_br)
summary(m17)

plot(m17)

#Connectivity in layer 3 does support more vocalizations from Black Redstarts 
# Get predicted values from the model
preds_plot2 <- ggpredict(m17, terms = c("C200.enn_cv"))


# Plot with ggplot
plot2 <- ggplot(preds_plot2, aes(x = x, y = predicted)) +
  geom_line(color = "blue", size = 1.2) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), fill = "lightblue", alpha = 0.3) +
  geom_point(data = roofs_layer3_br, aes(x = C200.enn_cv, y = total_calls), size = 2.8, alpha = 0.65) +
  labs(x = "Nerest Neighbor", y = "Total Vocalizations") +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    axis.line = element_line(color = "black"),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    plot.title = element_text(face = "bold", size = 14)
  )

print(plot2)

#Does seem to be a trend but I don't think it is really representative or has ecological value. 
