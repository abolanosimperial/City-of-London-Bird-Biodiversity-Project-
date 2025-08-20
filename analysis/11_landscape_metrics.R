#Getting LSM Metrics for Layers by height 0 m, 1-30 m, and 30m and above 

#House Keeping 
rm (list = ls())

library(sf)
library(terra)
library(landscapemetrics)
library(dplyr)
library(ggplot2)
library(tidyterra)

#Setting working Directory 
setwd("~/Documents/Education /MSc_Ecology_Conservation_and_Evolution /Thesis/GIS/F_GIS")

#Calculating for first layer 0-10
#Reading my Data 
#Green roof data 
green_roofs <- st_read("height_green_roofs_city_of_london_2022_2025.gpkg")

#There are no green roofs that are between 0-10 meters so they will be excluded from the analysis 

#Garden data 
garden <- st_read("urban_gardens_fixed.gpkg")

# Try converting to MULTIPOLYGON first
garden_multi <- st_cast(garden, "MULTIPOLYGON")
garden_simple <- st_cast(garden_multi, "POLYGON")

resolution <- 1 
crs_27700 <- crs("EPSG:27700")
extent_1 <- ext(garden_simple)
template_raster_1 <- rast(extent_1, resolution = resolution, crs = crs(crs_27700))

#Razterise 
garden_raster <- rasterize(garden_simple, template_raster_1, field = 1, background = 0)

plot(garden_raster)

#Now working with trees 
trees <- st_read("tree_data.gpkg")

#Remove Trees that we don't have a value for spread
trees <- trees %>%
  filter(!is.na(Spread__m_) & Spread__m_ > 0)

# Create circular buffers around trees based on spread values
tree_polygons <- st_buffer(trees, dist = trees$Spread__m_)



layer1 <- garden_raster

plot(layer1)

#Loading my study sites 
sites <- st_read("site_loc_h.gpkg")

sites_sf <- st_transform(sites, crs = st_crs(crs_27700))

#Creating the Buffer 200
sites_buffered_200 <- st_buffer(sites_sf, dist = 200)

sites_vect_200 <- vect(sites_buffered_200)


plot(layer1)
plot(sites_vect_200, add = TRUE)

#Calculating Landscape Metrics at Layer 1 
#Trying to Get Landscape Metric at 200 m buffer for each site with all urban areas 

lsm_1 <- sample_lsm(
  landscape = layer1, 
  y = sites_vect_200, 
  plot_id = sites_vect_200$id, 
  what = c('lsm_c_ca',
           'lsm_c_enn_cv')
)

# Drop the patch data and landscape rows
lsm_layer1 <- subset(lsm_1, level == 'class')
# Drop down to the forest class and also reduce to the three core fields
lsm_layer1 <- subset(lsm_layer1, class == 1, select=c(metric, value, plot_id))

# Rename the value field so that - when combined with the metric
# name - the local landscape details are recorded
names(lsm_layer1)[2] <- 'C200'

# Reshape that dataset so that each metric has its own column
lsm_layer1 <- reshape(data.frame(lsm_layer1), direction='wide', 
                      timevar='metric', idvar='plot_id')
lsm_layer1$C200.ca_m2 <- lsm_layer1$C200.ca * 10000

head(lsm_layer1, n=10)

#Attaching site names 

site_names <- read.csv("site_monitor_name.csv")

lsm_layer1 <- merge(lsm_layer1, site_names, by = "plot_id")

lsm_layer1$layer <- "1"

write.csv (lsm_layer1, "lsm_layer1.1_200.csv")

#Creating the Buffer 175
sites_buffered_175 <- st_buffer(sites_sf, dist = 175)

sites_vect_175 <- vect(sites_buffered_175)


plot(layer1)
plot(sites_vect_175, add = TRUE)

#Calculating Landscape Metrics at Layer 1 
#Trying to Get Landscape Metric at 175 m buffer for each site with all urban areas 

lsm_1_175 <- sample_lsm(
  landscape = layer1, 
  y = sites_vect_175, 
  plot_id = sites_vect_175$id, 
  what = c('lsm_c_ca',
           'lsm_c_enn_cv')
)

# Drop the patch data and landscape rows
lsm_layer1_175 <- subset(lsm_1_175, level == 'class')
# Drop down to the forest class and also reduce to the three core fields
lsm_layer1_175 <- subset(lsm_layer1_175, class == 1, select=c(metric, value, plot_id))

# Rename the value field so that - when combined with the metric
# name - the local landscape details are recorded
names(lsm_layer1_175)[2] <- 'C175'

# Reshape that dataset so that each metric has its own column
lsm_layer1_175 <- reshape(data.frame(lsm_layer1_175), direction='wide', 
                      timevar='metric', idvar='plot_id')
lsm_layer1_175$C175.ca_m2 <- lsm_layer1_175$C175.ca * 10000

head(lsm_layer1_175, n=10)

#Attaching site names 

site_names <- read.csv("site_monitor_name.csv")

lsm_layer1_175 <- merge(lsm_layer1_175, site_names, by = "plot_id")

lsm_layer1_175$layer <- "1"

write.csv (lsm_layer1_175, "lsm_layer1.1_175.csv")

#Creating the Buffer 150
sites_buffered_150 <- st_buffer(sites_sf, dist = 150)

sites_vect_150 <- vect(sites_buffered_150)


plot(layer1)
plot(sites_vect_150, add = TRUE)

#Calculating Landscape Metrics at Layer 1 
#Trying to Get Landscape Metric at 150 m buffer for each site with all urban areas 

lsm_1_150 <- sample_lsm(
  landscape = layer1, 
  y = sites_vect_150, 
  plot_id = sites_vect_150$id, 
  what = c('lsm_c_ca',
           'lsm_c_enn_cv')
)

# Drop the patch data and landscape rows
lsm_layer1_150 <- subset(lsm_1_150, level == 'class')
# Drop down to the forest class and also reduce to the three core fields
lsm_layer1_150 <- subset(lsm_layer1_150, class == 1, select=c(metric, value, plot_id))

# Rename the value field so that - when combined with the metric
# name - the local landscape details are recorded
names(lsm_layer1_150)[2] <- 'C150'

# Reshape that dataset so that each metric has its own column
lsm_layer1_150 <- reshape(data.frame(lsm_layer1_150), direction='wide', 
                          timevar='metric', idvar='plot_id')
lsm_layer1_150$C150.ca_m2 <- lsm_layer1_150$C150.ca * 10000

head(lsm_layer1_150, n=10)

#Attaching site names 

site_names <- read.csv("site_monitor_name.csv")

lsm_layer1_150 <- merge(lsm_layer1_150, site_names, by = "plot_id")

lsm_layer1_150$layer <- "1"

write.csv (lsm_layer1_150, "lsm_layer1.1_150.csv")


#Calculating For Second Layer 1-30 meters 
# Get all extents
garden_extent <- ext(garden_simple)
tree_extent <- ext(tree_polygons)
green_roof_extent <- ext(green_roofs)

# Create combined extent
combined_extent <- ext(c(
  min(garden_extent[1], tree_extent[1], green_roof_extent[1]),  # xmin
  max(garden_extent[2], tree_extent[2], green_roof_extent[2]),  # xmax
  min(garden_extent[3], tree_extent[3], green_roof_extent[3]),  # ymin
  max(garden_extent[4], tree_extent[4], green_roof_extent[4])   # ymax
))



#Calculating green roof raster 
green_roofs_2 <- subset(green_roofs, X_mean <= 30 & X_mean >= 1)

template_raster <- rast(combined_extent, resolution = resolution, crs = crs(crs_27700))

green_roof_raster_2 <- rasterize(green_roofs_2, template_raster, field = 1, background = 0)

plot(green_roof_raster_2)


#Calculating tree raster 
tree_polygons_2 <- subset(tree_polygons, Height__m_ <= 30 & Height__m_ >= 1)


#Razterise 
tree_raster_2 <- rasterize(tree_polygons_2, template_raster, field = 1, background = 0)

plot(tree_raster_2)


#Combining 
# Align the rasters to the same extent and resolution
layer2 <- mosaic(green_roof_raster_2, tree_raster_2, fun = "max")

plot(layer2)

#Getting sites and buffers 
sites_buffered_175 <- st_buffer(sites_sf, dist = 175)

sites_vect_175 <- vect(sites_buffered_175)


plot(layer2)
plot(sites_vect_175, add = TRUE)

#Calculating Landscape Metrics at Layer 2 
#Trying to Get Landscape Metric at 175

lsm_2 <- sample_lsm(
  landscape = layer2, 
  y = sites_vect_175, 
  plot_id = sites_vect_175$id, 
  what = c('lsm_c_ca',
           'lsm_c_enn_cv')
)

# Drop the patch data and landscape rows
lsm_layer2 <- subset(lsm_2, level == 'class')
# Drop down to the forest class and also reduce to the three core fields
lsm_layer2 <- subset(lsm_layer2, class == 1, select=c(metric, value, plot_id))

# Rename the value field so that - when combined with the metric
# name - the local landscape details are recorded
names(lsm_layer2)[2] <- 'C175'

# Reshape that dataset so that each metric has its own column
lsm_layer2 <- reshape(data.frame(lsm_layer2), direction='wide', 
                      timevar='metric', idvar='plot_id')
lsm_layer2$C175.ca_m2 <- lsm_layer2$C175.ca * 10000

head(lsm_layer2, n=10)

#Attaching site names 

lsm_layer2 <- merge(lsm_layer2, site_names, by = "plot_id")

lsm_layer2$layer <- "2"

head(lsm_layer2, n=15)

write.csv (lsm_layer2, "lsm_layer2.2_175.csv")

#Getting sites and buffers 200
#Calculating Landscape Metrics at Layer 2 
#Trying to Get Landscape Metric at 200

lsm_2_200 <- sample_lsm(
  landscape = layer2, 
  y = sites_vect_200, 
  plot_id = sites_vect_200$id, 
  what = c('lsm_c_ca',
           'lsm_c_enn_cv')
)

# Drop the patch data and landscape rows
lsm_layer2_200 <- subset(lsm_2_200, level == 'class')
# Drop down to the forest class and also reduce to the three core fields
lsm_layer2_200 <- subset(lsm_layer2_200, class == 1, select=c(metric, value, plot_id))

# Rename the value field so that - when combined with the metric
# name - the local landscape details are recorded
names(lsm_layer2_200)[2] <- 'C200'

# Reshape that dataset so that each metric has its own column
lsm_layer2_200 <- reshape(data.frame(lsm_layer2_200), direction='wide', 
                      timevar='metric', idvar='plot_id')
lsm_layer2_200$C200.ca_m2 <- lsm_layer2_200$C200.ca * 10000

head(lsm_layer2_200, n=10)

#Attaching site names 

lsm_layer2_200 <- merge(lsm_layer2_200, site_names, by = "plot_id")

lsm_layer2_200$layer <- "2"

head(lsm_layer2_200, n=15)


write.csv (lsm_layer2_200, "lsm_layer2.2_200.csv")


#Getting sites and buffers 150
#Calculating Landscape Metrics at Layer 2 
#Trying to Get Landscape Metric at 150

lsm_2_150 <- sample_lsm(
  landscape = layer2, 
  y = sites_vect_150, 
  plot_id = sites_vect_150$id, 
  what = c('lsm_c_ca',
           'lsm_c_enn_cv')
)

# Drop the patch data and landscape rows
lsm_layer2_150 <- subset(lsm_2_150, level == 'class')
# Drop down to the forest class and also reduce to the three core fields
lsm_layer2_150 <- subset(lsm_layer2_150, class == 1, select=c(metric, value, plot_id))

# Rename the value field so that - when combined with the metric
# name - the local landscape details are recorded
names(lsm_layer2_150)[2] <- 'C150'

# Reshape that dataset so that each metric has its own column
lsm_layer2_150 <- reshape(data.frame(lsm_layer2_150), direction='wide', 
                          timevar='metric', idvar='plot_id')
lsm_layer2_150$C150.ca_m2 <- lsm_layer2_150$C150.ca * 10000

head(lsm_layer2_150, n=10)

#Attaching site names 

lsm_layer2_150 <- merge(lsm_layer2_150, site_names, by = "plot_id")

lsm_layer2_150$layer <- "2"

head(lsm_layer2_150, n=15)


write.csv (lsm_layer2_150, "lsm_layer2.2_150.csv")


#Calculating For Third Layer 30 and above meters 
#Calculating green roof raster 
green_roofs_3 <- subset(green_roofs,  X_mean >= 30)

green_roof_raster_3 <- rasterize(green_roofs_3, template_raster, field = 1, background = 0)

plot(green_roof_raster_3)

#Calculating tree raster 
tree_polygons_3 <- subset(tree_polygons, Height__m_ >= 30)

#Rasterise (FIXED: using template_raster instead of template_raster_2)
tree_raster_3 <- rasterize(tree_polygons_3, template_raster, field = 1, background = 0)

plot(tree_raster_3)

#Combining (FIXED: removed unnecessary resampling)
layer3 <- mosaic(green_roof_raster_3, tree_raster_3, fun = "max")

plot(layer3)


plot(layer3)
plot(sites_vect_150, add = TRUE)

#Calculating Landscape Metrics at Layer 3 at 15o buffer
lsm_3 <- sample_lsm(
  landscape = layer3, 
  y = sites_vect_150, 
  plot_id = sites_vect_150$id, 
  what = c('lsm_c_ca',
           'lsm_c_enn_cv')
)

# Drop the patch data and landscape rows
lsm_layer3 <- subset(lsm_3, level == 'class')
# Drop down to the forest class and also reduce to the three core fields
lsm_layer3 <- subset(lsm_layer3, class == 1, select=c(metric, value, plot_id))

# Rename the value field
names(lsm_layer3)[2] <- 'C150'

# Reshape that dataset so that each metric has its own column
lsm_layer3 <- reshape(data.frame(lsm_layer3), direction='wide', 
                      timevar='metric', idvar='plot_id')
lsm_layer3$C150.ca_m2 <- lsm_layer3$C150.ca * 10000

#Attaching site names 
lsm_layer3 <- merge(lsm_layer3, site_names, by = "plot_id")

lsm_layer3$layer <- "3"


write.csv(lsm_layer3, "lsm_layer3.3_150.csv")

#Calculating Landscape Metrics at Layer 3 at 175 buffer
lsm_3_175 <- sample_lsm(
  landscape = layer3, 
  y = sites_vect_175, 
  plot_id = sites_vect_175$id, 
  what = c('lsm_c_ca',
           'lsm_c_enn_cv')
)

# Drop the patch data and landscape rows
lsm_layer3_175 <- subset(lsm_3_175, level == 'class')
# Drop down to the forest class and also reduce to the three core fields
lsm_layer3_175 <- subset(lsm_layer3_175, class == 1, select=c(metric, value, plot_id))

# Rename the value field
names(lsm_layer3_175)[2] <- 'C175'

# Reshape that dataset so that each metric has its own column
lsm_layer3_175 <- reshape(data.frame(lsm_layer3_175), direction='wide', 
                      timevar='metric', idvar='plot_id')
lsm_layer3_175$C175.ca_m2 <- lsm_layer3_175$C175.ca * 10000

#Attaching site names 
lsm_layer3_175 <- merge(lsm_layer3_175, site_names, by = "plot_id")

lsm_layer3_175$layer <- "3"


write.csv(lsm_layer3_175, "lsm_layer3.3_175.csv")

#Calculating Landscape Metrics at Layer 3 at 200 buffer
lsm_3_200 <- sample_lsm(
  landscape = layer3, 
  y = sites_vect_200, 
  plot_id = sites_vect_200$id, 
  what = c('lsm_c_ca',
           'lsm_c_enn_cv')
)

# Drop the patch data and landscape rows
lsm_layer3_200 <- subset(lsm_3_200, level == 'class')
# Drop down to the forest class and also reduce to the three core fields
lsm_layer3_200 <- subset(lsm_layer3_200, class == 1, select=c(metric, value, plot_id))

# Rename the value field
names(lsm_layer3_200)[2] <- 'C200'

# Reshape that dataset so that each metric has its own column
lsm_layer3_200 <- reshape(data.frame(lsm_layer3_200), direction='wide', 
                          timevar='metric', idvar='plot_id')
lsm_layer3_200$C200.ca_m2 <- lsm_layer3_200$C200.ca * 10000

#Attaching site names 
lsm_layer3_200 <- merge(lsm_layer3_200, site_names, by = "plot_id")

lsm_layer3_200$layer <- "3"


write.csv(lsm_layer3_200, "lsm_layer3.3_200.csv")


#Making a layer 3 looking at only extensive roofs 
extensive_roofs <- subset(green_roofs, roof_type == "e")

extensive_roofs_raster <- rasterize(extensive_roofs, template_raster, field = 1, background = 0)

plot(extensive_roofs_raster)

lsm_e_200 <- sample_lsm(
  landscape = extensive_roofs_raster, 
  y = sites_vect_200, 
  plot_id = sites_vect_200$id, 
  what = c('lsm_c_ca',
           'lsm_c_enn_cv')
)

# Drop the patch data and landscape rows
lsm_e_200 <- subset(lsm_e_200, level == 'class')
# Drop down to the forest class and also reduce to the three core fields
lsm_e_200 <- subset(lsm_e_200, class == 1, select=c(metric, value, plot_id))

# Rename the value field
names(lsm_e_200)[2] <- 'C200'

# Reshape that dataset so that each metric has its own column
lsm_e_200 <- reshape(data.frame(lsm_e_200), direction='wide', 
                          timevar='metric', idvar='plot_id')
lsm_e_200$C200.ca_m2 <- lsm_e_200$C200.ca * 10000

#Attaching site names 
lsm_e_200 <- merge(lsm_e_200, site_names, by = "plot_id")


write.csv(lsm_e_200, "lsm_e_200.csv")


#Making Plots of my Layers 
#Reading City of London 
city_london <- st_read("City_of_London_Boundery.gpkg")

city_london_sf <- st_transform(city_london, crs = st_crs(crs_27700))

city_london_vect <- vect(city_london_sf)

city_london_ext <- buffer(city_london_vect, 200)

plot(city_london)

layer1_c <- mask(crop(layer1, city_london_ext), city_london_vect)
layer2_c <- mask(crop(layer2, city_london_ext), city_london_vect)
layer3_c <- mask(crop(layer3, city_london_ext), city_london_vect)

plot(layer1_c)


# Convert values to factors with labels
layer1_labeled <- layer1_c
levels(layer1_labeled) <- c("Non-habitat", "Habitat")

layer1_plot <- ggplot() +
  geom_spatraster(data = layer1_labeled) +
  scale_fill_manual(values = c("Non-habitat" = "grey", "Habitat" = "#8B4513"),
                    name = "Classification",
                    na.value = "white",
                    na.translate = FALSE) +
  scale_x_continuous(expand = expansion(mult = 0.1)) +
  scale_y_continuous(expand = expansion(mult = 0.1)) +  
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    axis.title = element_text(size = 12),        
    axis.text = element_text(size = 12), 
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 14),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    plot.title = element_text(face = "bold", size = 14),
    plot.margin = margin(5, 5, 5, 5)
  ) + 
  labs(title = "1")
print(layer1_plot)



#ploting layer 2 
layer2_labeled <- layer2_c
levels(layer2_labeled) <- c("Non-habitat", "Habitat")

layer2_plot <- ggplot() +
  geom_spatraster(data = layer2_labeled) +
  scale_fill_manual(values = c("Non-habitat" = "grey", "Habitat" = "#228B22"),
                    name = "Classification",
                    na.value = "white",
                    na.translate = FALSE) +
  scale_x_continuous(expand = expansion(mult = 0.1)) +
  scale_y_continuous(expand = expansion(mult = 0.1)) +  
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    axis.title = element_text(size = 12),        
    axis.text = element_text(size = 12),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 14),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    plot.title = element_text(face = "bold", size = 14),
    plot.margin = margin(5, 5, 5, 5)
  ) + 
  labs(title = "2")

print(layer2_plot)

#Layer 3 
layer3_labeled <- layer3_c
levels(layer3_labeled) <- c("Non-habitat", "Habitat")

layer3_plot <- ggplot() +
  geom_spatraster(data = layer3_labeled) +
  scale_fill_manual(values = c("Non-habitat" = "grey", "Habitat" = "#20B2AA"),
                    name = "Classification",
                    na.value = "white",
                    na.translate = FALSE) +
  scale_x_continuous(expand = expansion(mult = 0.1)) +
  scale_y_continuous(expand = expansion(mult = 0.1)) +  
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    axis.title = element_text(size = 12),        
    axis.text = element_text(size = 12),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 14),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    plot.title = element_text(face = "bold", size = 14),
    plot.margin = margin(5, 5, 5, 5)
  ) + 
  labs(title = "3")

print(layer3_plot)

#Merging all plots to make one 
lsm_layer_plots <- plot_grid(layer1_plot, layer2_plot, layer3_plot, ncol = 2)

print(lsm_layer_plots)

ggsave("figure3.png", lsm_layer_plots, 
       width = 15, height = 10, dpi = 300, bg = "white")

