#Author: Mathis Messager
#Contact info: messamat@uw.edu
#Creation date: April 5th 2017
#Last updated: March 23rd 2018
#Objective: Produce a shapefile summarizing flood risk and water scarcity for Figure 4 of manuscript and discussion of results.

####################################################################################################################################
library(foreign)
library(rgdal)
library(dplyr)
library(ggplot2)

setwd("F:/gages_project/results")

quasi_ext <- read.csv("quasi-extinction/version_7/Quasiext.huc6_v7.csv")
quasi_ext [quasi_ext $rownames_HUC6 < 100000,'HUC6'] <- paste('0',as.character(quasi_ext [quasi_ext $rownames_HUC6 < 100000,'rownames_HUC6']),sep="")
quasi_ext [quasi_ext $rownames_HUC6 >= 100000,'HUC6'] <- as.character(quasi_ext [quasi_ext $rownames_HUC6 >= 100000,'rownames_HUC6'])
quasi_ext <- quasi_ext[,-c(1,2)]

fishdiv <- read.csv("fish/HUC6div.csv", colClasses=c("character", rep('numeric',4)))
ndc <- read.csv("water_Scarcity/HUC6_NDC_pr.csv", colClasses=c("character","character", rep('numeric', 12)))
ndc <- select(ndc, HUC6, NDC_HUC)
flood <- read.dbf("flood/HUC6_floodFEMA_data.dbf")

str(quasi_ext)
str(fishdiv)
str(ndc)
str(flood)


read.dbf("fish/HUC6div_category.dbf")

#Merge fishdiv, ndc, flood, quasi-ext with HUC6
data_merge <- merge(quasi_ext, ndc, by="HUC6", all.x =T) %>%
  merge(., flood, by="HUC6", all.x =T)

data_merge <- data_merge[data_merge$HUC6 < 200100,]


############################# LOW QUASI EXTINCT PROBABILITY ####################################
data_merge[data_merge$Quasiext.huc6 < 0.5 &
             data_merge$NDC_HUC <= 2 &
             data_merge$flood_FEMA < 0.05,
           "ndcflood_category"] <- "Low_Low_Low"
data_merge[data_merge$Quasiext.huc6 < 0.5 &
             data_merge$NDC_HUC <= 2 &
             data_merge$flood_FEMA >= 0.05,
           "ndcflood_category"] <- "Low_Low_High"


data_merge[data_merge$Quasiext.huc6 < 0.5 &
             data_merge$NDC_HUC > 2 &
             data_merge$flood_FEMA < 0.05,
           "ndcflood_category"] <- "Low_High_Low"
data_merge[data_merge$Quasiext.huc6 < 0.5 &
             data_merge$NDC_HUC > 2 &
             data_merge$flood_FEMA >= 0.05,
           "ndcflood_category"] <- "Low_High_High"

############################# HIGH QUASI EXTINCT PROBABILITY ####################################
data_merge[data_merge$Quasiext.huc6 >= 0.5 &
             data_merge$NDC_HUC <= 2 &
             data_merge$flood_FEMA < 0.05,
           "ndcflood_category"] <- "High_Low_Low"
data_merge[data_merge$Quasiext.huc6 >= 0.5 &
             data_merge$NDC_HUC <= 2 &
             data_merge$flood_FEMA >= 0.05,
           "ndcflood_category"] <- "High_Low_High"


data_merge[data_merge$Quasiext.huc6 >= 0.5 &
             data_merge$NDC_HUC > 2 &
             data_merge$flood_FEMA < 0.05,
           "ndcflood_category"] <- "High_High_Low"
data_merge[data_merge$Quasiext.huc6 >=0.5 &
             data_merge$NDC_HUC > 2 &
             data_merge$flood_FEMA >= 0.05,
           "ndcflood_category"] <- "High_High_High"


data_merge$ndcflood_category

# The input file geodatabase
folder = '/general'
#List layer in gdb
fc_list = ogrListLayers(folder)
print(fc_list)
# Read the feature class of HUC6
fc = readOGR(dsn=folder,layer="HUC6_conterm")
# Determine the FC extent, projection, and attribute information
class(fc)
summary(fc)
#Prepare data
fc_merge <- merge(fc, data_merge, by="HUC6",all.x=T)

#Write out merged category data
writeOGR(fc_merge, dsn=getwd(), layer="HUC6_ndcflood_category3", driver="ESRI Shapefile")

#Color pattern inspired from http://www.joshuastevens.net/cartography/make-a-bivariate-choropleth-map/