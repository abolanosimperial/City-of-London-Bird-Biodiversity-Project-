#Making my plots for Draft 1 Results 
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
library(emmeans)
library(ggeffects)
library(MASS)
library(emmeans)
library(dplyr)
library(lme4)



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


#Calculating how much variation is explained by PCoA Axis one to justify separation. 
eigenvalues <- pcoa$eig
percent_explained <- 100 * eigenvalues / sum(eigenvalues)
axis1_percent <- percent_explained[1]
print(paste("Axis 1 explains", round(axis1_percent, 2), "% of variation"))

#Making my PCoA Data frame 
pcoa_df <- data.frame(
  Monitor = presence_absence_matrix$Monitor,
  site_type = presence_absence_matrix$site_type,
  Axis1 = pcoa$points[,1],
  Axis2 = pcoa$points[,2]
)


#Plot 1: Making Boxplot for PCoA Axis 1 (still need to add statistics to this plot)

#Statistical Model 
# Garden as a reference
pcoa_df$site_type <- as.factor(pcoa_df$site_type)

pcoa_df$site_type <- relevel(pcoa_df$site_type, ref = "garden")
m1 <- lm(Axis1 ~ site_type, data = pcoa_df)
summary(m1)

confint(m1, level = 0.95)

# Intensive as reference  
pcoa_df$site_type <- relevel(pcoa_df$site_type, ref = "intensive")
m2 <- lm(Axis1 ~ site_type, data = pcoa_df)
summary(m2)  
 
confint(m2, level = 0.95)

# Conventional as reference
pcoa_df$site_type <- relevel(pcoa_df$site_type, ref = "conventional")
m3 <- lm(Axis1 ~ site_type, data = pcoa_df)
summary(m3) 

confint(m3, level = 0.95)


#Making gadens reference for plot 
pcoa_df$site_type <- relevel(pcoa_df$site_type, ref = "garden")

#Plot 
#Labeling for significance 
label_plot1 <- data.frame(
  site_type = c("garden", "intensive", "conventional", "extensive"),
  Axis1 = c(0.3, 0.3, 0.3, 0.3),  # Adjust height as needed
  label = c("A", "A B", "B C", "C")
)

#Making Plot 
pcoa1_box_plot <- ggplot(data = pcoa_df, aes(x = site_type, y = Axis1)) +
  geom_boxplot(fill = "grey", color = "black") +
  labs( x = "Urban Habitat Type",
        y = "Community Composition (PCoA Axis 1)") +
  scale_x_discrete(labels = c("garden" = "Garden", 
                              "intensive" = "Intensive Roof",
                              "conventional" = "Conventional Roof", 
                              "extensive" = "Extensive Roof")) +
  theme_minimal() + 
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    axis.line = element_line(color = "black"),
    axis.title = element_text(size = 14),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.text = element_text(size = 12),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    plot.title = element_text(face = "bold", size = 14)
  ) +
  geom_point()  +
  geom_text(data = label_plot1, aes(x = site_type, y = Axis1, label = label),
            vjust = -0.5, size = 5)


print(pcoa1_box_plot)

ggsave("plot1.png", pcoa1_box_plot, 
       width = 8, height = 6, dpi = 300, bg = "white")

#Reading my connectivity/ stepping stone metric 

stepping_stone <- read.csv("all_sites_95th_percentile_below_site_results.csv")

stepping_stone <- stepping_stone %>%
  mutate(metric = replace_na(metric, 27.10800))

ss_pcoa <- merge(pcoa_df, stepping_stone, by = "Monitor")


#Making Plot 2: PCoA1 and Stepping stone height metric controlling for site type. 
#Statistical Model 
m4 <-glm(Axis1 ~ metric + site_type, data = ss_pcoa[ss_pcoa$site_type != "extensive", ])
summary(m4) 

confint(m4, level = 0.95)

ss_pcoa_nout <- subset(ss_pcoa, id != "10")

m5 <- glm(Axis1 ~ metric + site_type, data =  ss_pcoa_nout[ss_pcoa_nout$site_type != "extensive", ])
summary(m5)


ss_pcoa_nex <- subset(ss_pcoa, site_type != "extensive")


# Get predicted values from the model
preds_plot2 <- ggpredict(m4, terms = c("metric"))


# Plot with ggplot
plot2 <- ggplot(preds_plot2, aes(x = x, y = predicted)) +
  geom_line(color = "blue", size = 1.2) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), fill = "lightblue", alpha = 0.3) +
  geom_point(data = subset(ss_pcoa_nex, id != "10"), 
             aes(x = metric, y = Axis1, color = site_type), 
             size = 2.8, alpha = 0.65) +
  geom_point(data = subset(ss_pcoa_nex, id == "10"), 
             aes(x = metric, y = Axis1, color = site_type), 
             size = 3.5, alpha = 0.8, shape = 8) + 
  scale_color_manual(
    name = "Site Type",
    values = c("conventional" = "#0072B2", "intensive" = "#009E73"),
    labels = c("conventional" = "Conventional", "intensive" = "Intensive")
  ) +
  labs(x = "Stepping Stone Height Metric (m)", y = "Community Composition (PCoA Axis 1)") +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    axis.line = element_line(color = "black"),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.text = element_text(size = 13),
    legend.title = element_text(size = 14),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    plot.title = element_text(face = "bold", size = 14)
  )

print(plot2)

ggsave("plot2.png", plot2, 
       width = 8, height = 6, dpi = 300, bg = "white")




#Plot 3 Working on Landscape metric plot 
#Reading lsm data 

lsm_layer1_200 <- read.csv("lsm_layer1.1_200.csv")

lsm_layer2_150 <- read.csv("lsm_layer2.2_150.csv")

lsm_layer3_200 <- read.csv("lsm_layer3.3_200.csv")

#Reading Species Richness data 
bioactivity_data <- read.csv ("bioactivity.csv")

species_richness_data <- bioactivity_data %>% 
  dplyr::select(Monitor, n_species)

summary(species_richness_data$n_species)
sd(species_richness_data$n_species)

#Adding site type 
species_richness_data <- species_richness_data %>% 
  mutate(site_type = case_when(
    Monitor %in% c("FOCG1", "FOCG4", "FOCG15") ~ "conventional",
    Monitor %in% c("FOCG6", "FOCG10", "FOCG9", "FOCG8") ~ "intensive",
    Monitor %in% c("FOCG5", "FOCG7", "FOCG3") ~ "extensive",
    Monitor %in% c("FOCG12", "FOCG13", "FOCG11", "FOCG2", "FOCG14") ~ "garden",
    TRUE ~ "other"
  ))

#Merging Data 
layer1  <- merge(species_richness_data, lsm_layer1_200, by = "Monitor")

layer2 <- merge(species_richness_data, lsm_layer2_150, by = "Monitor")

layer3 <- merge(species_richness_data, lsm_layer3_200, by = "Monitor")

#Making Plot 3 
#Layer1
#Statistical Model 
m6 <- glm (n_species ~ C200.enn_cv, data = layer1, family = poisson)
summary (m6)

confint(m6, level = 0.95)

#Plot
layer1_nn_plot <- ggplot(data = layer1, aes(x = C200.enn_cv, y = n_species)) +
  geom_point(size = 2.8, alpha = 0.65, color = "black") +
  geom_smooth(method = "lm", se = TRUE, 
              color = "blue", fill = "lightblue", alpha = 0.2,
              linewidth = 1.2) +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    axis.line = element_line(color = "black"),
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 16),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    plot.title = element_text(face = "bold", size = 20)
  ) +
  labs(
    x = "Nearest Neighbor CV (m)", 
    y = "Species Richness",
    title = "1"
  )

print(layer1_nn_plot)

#Plot for layer 2 NN 

#Statisticcal Model 
m7 <- glm (n_species ~ C150.enn_cv, data = layer2, family = poisson)
summary (m7)

confint(m7, level = 0.95)

#Plot 

layer2_nn_plot <- ggplot(data = layer2, aes(x = C150.enn_cv, y = n_species)) +
  geom_point(size = 2.8, alpha = 0.65, color = "black") +
  geom_smooth(method = "lm", se = TRUE, 
              color = "blue", fill = "lightblue", alpha = 0.2,
              linewidth = 1.2, linetype = "dotted") +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    axis.line = element_line(color = "black"),
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 16),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    plot.title = element_text(face = "bold", size = 20)
  ) +
  labs(
    x = "Nearest Neighbor CV (m)", 
    y = "Species Richness",
    title = "2"
  )


print(layer2_nn_plot)


#Plot for Layer 3 NN

#Statistical model 
m8 <- glm (n_species ~ C200.enn_cv, data = layer3, family = poisson)
summary(m8)

confint(m8, level = 0.95)
#Plot 
layer3_nn_plot <- ggplot(data = layer3, aes(x = C200.enn_cv, y = n_species)) +
  geom_point(size = 2.8, alpha = 0.65, color = "black") +
  geom_smooth(method = "lm", se = TRUE, 
              color = "blue", fill = "lightblue", alpha = 0.2,
              linewidth = 1.2, linetype = "dotted") +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    axis.line = element_line(color = "black"),
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 16),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    plot.title = element_text(face = "bold", size = 20)
  ) +
  labs(
    x = "Nearest Neighbor CV (m)", 
    y = "Species Richness",
    title = "3"
  )


print(layer3_nn_plot)

#Merging all plots into one big plot for NN

habitat_nearest_neighbor_plot_all <- plot_grid(layer1_nn_plot, layer2_nn_plot, layer3_nn_plot, ncol = 2)

print(habitat_nearest_neighbor_plot_all)

ggsave("plot3.png", habitat_nearest_neighbor_plot_all, 
       width = 15, height = 14, dpi = 300, bg = "white")



#Plot 4: Black Redstart Average calls per day

br_activity <-read.csv("br_avg_calls_per_day.csv")


# Change reference level to garden
br_activity$site_type <- as.factor(br_activity$site_type)
br_activity$site_type <- relevel(br_activity$site_type, ref = "garden")


#Negative binomial 
m9 <- glm.nb(total_calls ~ site_type, data = br_activity)
summary(m9)

confint(m9, level = 0.95)

# Intensive as reference  
br_activity$site_type <- relevel(br_activity$site_type, ref = "intensive")
m10 <- glm.nb(total_calls ~ site_type, data = br_activity)
summary(m10)  

confint(m10, level = 0.95)

# Conventional as reference
br_activity$site_type <- relevel(br_activity$site_type, ref = "conventional")
m11 <- glm.nb(total_calls ~ site_type, data = br_activity)
summary(m11) 

confint(m11, level = 0.95)

#Making gardens reference for plot 
br_activity$site_type <- relevel(br_activity$site_type, ref = "garden")

#Making Box Plot 
#Plot 
#Labeling for significance 
label_plot4 <- data.frame(
  site_type = c("garden", "intensive", "conventional", "extensive"),
  avg_calls_per_day = c(25, 25, 25, 25), 
  label = c("A", "B", "B", "B")
)

#Making Plot 
br_activity_box_plot <- ggplot(data = br_activity, aes(x = site_type, y = avg_calls_per_day)) +
  geom_boxplot(fill = "grey", color = "black") +
  labs( x = "Urban Habitat Type",
        y = "Average Calls per Day") +
  scale_x_discrete(labels = c("garden" = "Garden", 
                              "intensive" = "Intensive Roof",
                              "conventional" = "Conventional Roof", 
                              "extensive" = "Extensive Roof")) + 
  theme_minimal() + 
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    axis.line = element_line(color = "black"),
    axis.title = element_text(size = 14),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.text = element_text(size = 12),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    plot.title = element_text(face = "bold", size = 14)
  ) +
  geom_point(position = position_jitter(width = 0.15, height = 0.2))  +
  geom_text(data = label_plot4, aes(x = site_type, y = avg_calls_per_day, label = label),
            vjust = -0.5, size = 5)


print(br_activity_box_plot)

ggsave("plot4.png", br_activity_box_plot, 
       width = 8, height = 6, dpi = 300, bg = "white")


#Making Plot 5 and 6 Black Redstart Activity with habitat area and connectivity 
#Merging data sets

layer1_br <- merge( br_activity, lsm_layer1_200, by = "Monitor")

layer2_br <- merge(br_activity, lsm_layer2_150, by = "Monitor")

layer3_br <- merge(br_activity, lsm_layer3_200, by = "Monitor")

#Making Plot 5 black redstart vocalizations vs habitat area 
#Layer 1: 
m12 <- glm.nb(total_calls ~ C200.ca_m2, data = layer1_br)
summary(m12)

confint.default(m12, level = 0.95)

#Plotting 
predict_m12 <- ggpredict(m12, terms = c("C200.ca_m2"))

#Plot
layer1_a_nb <- ggplot(predict_m12, aes(x = x, y = predicted)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), 
              fill = "lightblue", alpha = 0.2) + 
  geom_line(color = "blue", linewidth = 1.2) +
  geom_point(data = layer1_br, aes(x = C200.ca_m2, y = total_calls), 
             size = 2.8, alpha = 0.65, color = "black") + 
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    axis.line = element_line(color = "black"),
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 16),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    plot.title = element_text(face = "bold", size = 20)
  ) +
  labs(
    x = "Habitat area (m)", 
    y = "Total Vocalizations",
    title = "1"
  )

plot(layer1_a_nb)

#Layer 2: 
m13 <- glm.nb (total_calls ~ C150.ca_m2, data = layer2_br)
summary(m13)

confint.default(m13, level = 0.95)

#Plotting 
predict_m13 <- ggpredict(m13, terms = c("C150.ca_m2"))

#Plot
layer2_a_nb <- ggplot(predict_m13, aes(x = x, y = predicted)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), 
              fill = "lightblue", alpha = 0.2) + 
  geom_line(color = "blue", linewidth = 1.2,linetype = "dotted") +
  geom_point(data = layer2_br, aes(x = C150.ca_m2, y = total_calls), 
             size = 2.8, alpha = 0.65, color = "black") + 
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    axis.line = element_line(color = "black"),
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 16),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    plot.title = element_text(face = "bold", size = 20)
  ) +
  labs(
    x = "Habitat area (m)", 
    y = "Total Vocalizations",
    title = "2"
  )

plot(layer2_a_nb)

#Layer 3

m14 <- glm.nb (total_calls ~ C200.ca_m2, data = layer3_br)
summary(m14)

confint.default(m14, level = 0.95)

#Plotting 
predict_m14 <- ggpredict(m14, terms = c("C200.ca_m2"))

#Plot
layer3_a_nb <- ggplot(predict_m14, aes(x = x, y = predicted))  +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), 
              fill = "lightblue", alpha = 0.2) + 
  geom_line(color = "blue", linewidth = 1.2) +
  geom_point(data = layer3_br, aes(x = C200.ca_m2, y = total_calls), 
             size = 2.8, alpha = 0.65, color = "black") + 
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    axis.line = element_line(color = "black"),
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 16),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    plot.title = element_text(face = "bold", size = 20)
  ) +
  labs(
    x = "Habitat area (m)", 
    y = "Total Vocalizations",
    title = "3"
  )

plot(layer3_a_nb)

#Merging all plots into one big plot for habitat area Redstart Activity 

habitat_area_plot_black <- plot_grid(layer1_a_nb, layer2_a_nb, layer3_a_nb, ncol = 2)

print(habitat_area_plot_black)

ggsave("plot5.png", habitat_area_plot_black, 
       width = 15, height = 15, dpi = 300, bg = "white")



#Making Plot 6: Black Redstart Activity and connectivity 
#Layer 1: 
m15 <- glm.nb(total_calls ~ C200.enn_cv, data = layer1_br)
summary(m15)

confint.default(m15, level = 0.95)

#Plotting 
predict_m15 <- ggpredict(m15, terms = c("C200.enn_cv"))

#Plot
layer1_nn_nb <- ggplot(predict_m15, aes(x = x, y = predicted)) + 
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), 
              fill = "lightblue", alpha = 0.2) + 
  geom_line(color = "blue", linewidth = 1.2, linetype = "dotted") +
  geom_point(data = layer1_br, aes(x = C200.enn_cv, y = total_calls), 
             size = 2.8, alpha = 0.65, color = "black" ) + 
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    axis.line = element_line(color = "black"),
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 16),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    plot.title = element_text(face = "bold", size = 20)
  ) +
  labs(
    x = "Nearest Neighbor CV (m)", 
    y = "Total Vocalizations",
    title = "1"
  )

plot(layer1_nn_nb)

#Layer 2: 
m16 <- glm.nb(total_calls ~ C150.enn_cv, data = layer2_br)
summary(m16)

confint.default(m16, level = 0.95)

#Plotting 
predict_m16 <- ggpredict(m16, terms = c("C150.enn_cv"))

#Plot
layer2_nn_nb <- ggplot(predict_m16, aes(x = x, y = predicted)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), 
              fill = "lightblue", alpha = 0.2) + 
  geom_line(color = "blue", linewidth = 1.2, linetype = "dotted") +
  geom_point(data = layer2_br, aes(x = C150.enn_cv, y = total_calls), 
             size = 2.8, alpha = 0.65, color = "black") + 
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    axis.line = element_line(color = "black"),
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 16),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    plot.title = element_text(face = "bold", size = 20)
  ) +
  labs(
    x = "Nearest Neighbor CV (m)", 
    y = "Total Vocalizations",
    title = "2"
  )

plot(layer2_nn_nb)

#Layer 3: 
m17 <- glm.nb(total_calls ~ C200.enn_cv, data = layer3_br)
summary(m17)

confint.default(m17, level = 0.95)

#Plotting 
predict_m17 <- ggpredict(m17, terms = c("C200.enn_cv"))

#Plot
layer3_nn_nb <- ggplot(predict_m17, aes(x = x, y = predicted)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), 
              fill = "lightblue", alpha = 0.2) + 
  geom_line(color = "blue", linewidth = 1.2) +
  geom_point(data = layer3_br, aes(x = C200.enn_cv, y = total_calls), 
             size = 2.8, alpha = 0.65, color = "black") + 
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    axis.line = element_line(color = "black"),
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 16),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    plot.title = element_text(face = "bold", size = 20)
  ) +
  labs(
    x = "Nearest Neighbor CV (m)", 
    y = "Total Vocalizations",
    title = "3"
  )

plot(layer3_nn_nb)

#Merging all plots into one big plot for bearest neighbor Redstart Activity 

habitat_nearest_neighbor_plot_black <- plot_grid(layer1_nn_nb, layer2_nn_nb, layer3_nn_nb, ncol = 2)

print(habitat_nearest_neighbor_plot_black)

ggsave("plot6.png",habitat_nearest_neighbor_plot_black , 
       width = 15, height = 15, dpi = 300, bg = "white")


#Getting Average species number per site

average_species <- species_richness_data %>% 
  group_by(site_type) %>%
  summarise(avg_n_species = mean(n_species, na.rm = TRUE))


hist(species_richness_data$n_species)

shapiro.test(species_richness_data$n_species)

species_richness_hist <- species_richness_data %>%
  group_by(site_type) %>%
  hist(species_richness_data$n_species)

summary_stats_species_richness <- species_richness_data %>%
  group_by(site_type) %>%
  summarise(
    n = n(),
    mean = mean(n_species),
    sd = sd(n_species),
    min = min(n_species),
    max = max(n_species),
    .groups = 'drop'
  )

print(summary_stats_species_richness)

# Test normality for each site type
normality_tests <- species_richness_data %>%
  group_by(site_type) %>%
  summarise(
    shapiro_p = shapiro.test(n_species)$p.value,
    .groups = 'drop'
  )

print(normality_tests)

#Getting different stats for conventional roof since it is not normally distributed 
conventional_stats <- species_richness_data %>%
  filter(site_type == "conventional") %>%
  summarise(
    n = n(),
    median = median(n_species),
    Q1 = quantile(n_species, 0.25),
    Q3 = quantile(n_species, 0.75),
    IQR = IQR(n_species),
    min = min(n_species),
    max = max(n_species)
  )

print(conventional_stats)

