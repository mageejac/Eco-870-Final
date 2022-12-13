# Use level 1 data "deer_data_4

deer_data_4$habitat_group = as.factor(deer_data_4$habitat_group)
deer_data_4$habitat_group_ref <- relevel(deer_data_4$habitat_group, ref = "Developed/Open_Space")

# create glm
model_1 <- glm.nb(total_deer_group ~ habitat_group_ref, data = deer_data_4)

#set up CropScapeR

#URL for CropScapeR package: https://tmieno2.github.io/R-as-GIS-for-Economists/CropScapeR.html

if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  stars, # spatiotemporal data handling
  terra, # raster data handling
  raster, # raster data handling
  sf, # vector data handling
  dplyr, # data wrangling
  stringr, # string manipulation
  lubridate, # dates handling
  data.table, # data wrangling
  tidyr, # reshape
  tidyUSDA, # download USDA NASS data
  keyring, # API key management
  FedData, # download Daymet data
  daymetr, # download Daymet data
  ggplot2, # make maps
  tmap, # make maps
  future.apply, # parallel processing
  CropScapeR, # download CDL data
  prism, # download PRISM data
  exactextractr # extract raster values to sf
)  

#set up map themes

theme_set(theme_bw())

theme_for_map <- theme(
  axis.ticks = element_blank(),
  axis.text= element_blank(), 
  axis.line = element_blank(),
  panel.border = element_blank(),
  panel.grid.major = element_line(color='transparent'),
  panel.grid.minor = element_line(color='transparent'),
  panel.background = element_blank(),
  plot.background = element_rect(fill = "transparent",color='transparent')
)  

#load/ install packages

library(CropScapeR)
library(devtools)
devtools::install_github("cbw1243/CropScapeR")
library(CropScapeR)
library(ggplot2)
library(tidyverse)


#Generate study area map and extent

Study_Area <- tigris::counties(state = "MI", cb = TRUE) %>% 
  st_as_sf() %>% 
  filter(NAME %in% c("Ionia", "Clinton", "Eaton", "Ingham", "Shiawassee")) 
MICH_county <- tigris::counties("Michigan", cb = TRUE)
ggplot() +
  geom_sf(data = MICH_county) +
  geom_sf(data =Study_Area, fill = "lightblue") +
  theme_void()+
  ggspatial::annotation_scale(
    location = "bl",
    bar_cols = c("grey60", "white")) +
  ggspatial::annotation_north_arrow(location = "tr", which_north = "true")


#pull in USDA crop cover data for study area

(
  cdl_MICH_SA <- GetCDLData(
    aoi = Study_Area, 
    year = "2021", 
    type = "b",
  )
)


#look up projections

terra::crs(Study_Area)
terra::crs(cdl_MICH_SA)


#clip crop cover data to study area

cdl_MICH_SA_masked <- Study_Area %>% 
  #--- change the CRS first to that of the raster data ---#
  st_transform(., projection(cdl_MICH_SA)) %>% 
  #--- mask the values outside the sf (turn them into NA) ---#
  raster::mask(cdl_MICH_SA, .)

#plot Crop cover data for the study area

plot(cdl_MICH_SA_masked,
     legend.args=list(text='Cover Type', side=4, font=2, line=2.5, cex=0.8))
scalebar(50000, type="bar", divs = 2, label = c(0,25,50),below="km")
north("topleft", angle = 350)


data("linkdata")

#pull in crop codes for USDA crop layer data

crop_codes <- read.csv("https://raw.githubusercontent.com/mageejac/Eco-870-Final/main/cdl_codes_names.csv")

head(crop_codes)

# merge with data set

colnames(crop_codes)[which(names(crop_codes) == "Crop")] <- "habitat_group_ref"

deer_data_5 <- merge(crop_codes, deer_data_4, by ='habitat_group_ref')

colnames(deer_data_5)[which(names(deer_data_5) == "MasterCat")] <- "value"

deer_data_5$value <- as.factor(deer_data_5$value)

deer_data_5$value <- relevel(deer_data_5$value, ref = "121")

#generate second model to predict congregations across the landscape with crop code value

model_2 <- glm.nb(total_deer_group ~ value, data = deer_data_5)

summary(model_2)

boxplot(total_deer_group ~ value, data = deer_data_5)

# fitting predicted group values to raster

values_crop <-data.frame(value= c("121","1","5","24","36","61","63","64","152"))

p1<-predict(model_2, newdata=values_crop, se.fit=TRUE, type='response')

py2<- expand.grid(values_crop)
p2<-predict(model_2, newdata=py2, se.fit=TRUE, type='response')
predicted_group_size <- data.frame(py2, p2)

predicted_group_size

#plotting new raster with predicted values

names(cdl_MICH_SA_masked) = "value"

group_raster<- predict(cdl_MICH_SA_masked, model_2, type="response")

plot(group_raster,
     col= heat.colors(8, rev=TRUE),
     legend.args=list(text='Group Size', side=4, font=2, line=2.5, cex=0.8))
scalebar(50000, type="bar", divs = 2, label = c(0,25,50),below="km")
north("topleft", angle = 350)

