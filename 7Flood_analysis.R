library(foreign)
library(ggplot2)
library(plyr)
library(dplyr)

#Author: Mathis Messager
#Creation date: April 5th 2017
#Last updated: March 23rd 2018
#Objective: Calculate for each river basin (HUC6) the percentage of the population living within a flood zone
setwd("F:/gages_project/results/flood/")

#Importing censusflood_IDs, when in .dbf, crashed several times, so exported as .csv in arcmap
censusflood_IDs <- read.csv("censusflood_intersect_2_proj.csv",colClasses=c("numeric","character", "numeric", "numeric","numeric"))

#Data from intersection of FEMA data and census blocks with no urban land cover pixels
nourban_FEMA_inters <- read.dbf("censusnourban_FEMA_inters.dbf")
#Data on the total number of pixels in each census blocks
censusblock_urban <-read.dbf("censusblock_lcd_inters_tab.dbf")
#Data on the total number of urban pixels with FEMA data in each census block
censusblock_FEMAurban <-read.dbf("censusFEMAdat_lcd_inters_tab.dbf")
#Data on the total number of urban pixels that are in a flood zone in each census block
censusblock_Floodurban <-read.dbf("censusflood_urbansum.dbf")
#Intersection of census blocks with HUC6 such that when census block straddles the limit between two basins, it is in several records
censusblock_HUC6_inters <- read.dbf("Censusblock_HUC6_inters.dbf")
#General census block data
censusblock <- read.dbf("Censusblock_US_merge.dbf")

str(censusblock)
str(censusblock_urban)
str(censusblock_Floodurban)

#Join statistics on the number of urban pixels within each census block to censusblock data in order to gain BLOCKID10 field
#(zonal statistics were all computed using OBJECTID, which is not a key across all tables)
censusblock_urban_id <- merge(censusblock, censusblock_urban[,c("Value", "Count")], by.x="OBJECTID", by.y="Value", all.x = F)
str(censusblock_urban_id)
#Join urban pixel values with intersection of census blocks and HUC6
censusblock_HUC6_urban_id <- merge(censusblock_HUC6_inters[,-1], censusblock_urban_id[,c("OBJECTID","BLOCKID10","Count")], by="BLOCKID10", all.x = T)
mean(censusblock$AREA_GEO)
median(censusblock$AREA_GEO)

##################################################################################################
#Check whether some census blocks have no urban pixels yet some population
##################################################################################################
censusblock_urban_id_check <- merge(censusblock, censusblock_urban[,c("Value", "Count")], 
                                    by.x="OBJECTID", by.y="Value", all.x = T)
str(censusblock_urban_id_check)
censusblock_nourban <- censusblock_urban_id_check[which(is.na(censusblock_urban_id_check$Count) & 
                                                          censusblock_urban_id_check$POP10 >0 & 
                                                          censusblock_urban_id_check$STATEFP10 != 15),]
write.dbf(censusblock_nourban, "Censusblock_nourban.dbf")

##Many are in Hawaii (not included in analysis) or really small, some are buggy for their size
##################################################################################################
#Compute population in FEMA area and flood area weighted by area rather than by urban area
##################################################################################################

###########
#For flood zone intersection, first subset those census blocks that have no urban area
censusflood_nourban <- merge(censusblock_nourban[,c("POP10","BLOCKID10")], 
                             censusflood_IDs, by="BLOCKID10")
#Check whether any of the "nourban" census blocks are in that dataset 
which(censusblock_FEMAurban_id_HUC6$BLOCKID10 %in% censusflood_nourban$BLOCKID10)


#The intersection tool with the FEMA data sometimes leads to double counting because two assessments might have been made in the same location
#This cannot occur with the urban area method, but here might lead to a few spurious measurements. Therefore, delete any two records in a census block 
#that have identical intersecting areas (means that they stem from two flood polygons intersecting them identically
length(which(duplicated(censusflood_nourban[,c("AREA_INTERS", "BLOCKID10")])))
censusflood_nourban <- censusflood_nourban[-which(duplicated(censusflood_nourban[,c("AREA_INTERS", "BLOCKID10")])),]

#For each census block, compute the total area within a flood zone (as intersection led to several polygons for each census block)
censusflood_nourban_sum  <- ddply(censusflood_nourban, .(BLOCKID10), summarize, AREA_INTEsum=sum(AREA_INTERS, na.rm=T))
censusflood_nourban_unique <- censusflood_nourban[-which(duplicated(censusflood_nourban$BLOCKID10)),-ncol(censusflood_nourban)]
censusflood_nourban_unique <- merge(censusflood_nourban_unique, censusflood_nourban_sum, by="BLOCKID10")

#Make sure that area doesn't exceed census block area
censusflood_nourban_unique_error <- censusflood_nourban_unique[which(((censusflood_nourban_unique$AREA_INTEsum-censusflood_nourban_unique$AREA_GEOBL)/censusflood_nourban_unique$AREA_GEOBL) > 0.001),]

#Join with HUC6 data 
censusflood_nourban_unique_HUC6 <- merge(censusblock_HUC6_urban_id[,c("BLOCKID10", "HUC6", "AREA_BLOCK")], censusflood_nourban_unique, by ="BLOCKID10",all.x = F)

#Compute population within FEMA assessed area
colnames(censusflood_nourban_unique_HUC6)
censusflood_nourban_unique_HUC6$pop_floodzone <- with(censusflood_nourban_unique_HUC6, round(POP10.x*(AREA_BLOCK/AREA_GEOBLOCK)*(AREA_INTEsum/AREA_GEOBLOCK)))

####################################################################################
#Compute population within area with FEMA data, based on total intersecting area

#Merge FEMA urban data with reference data to have BLOCKID10 and HUC6 fields
censusblock_FEMAurban_id_HUC6 <- merge(censusblock_HUC6_urban_id, censusblock_FEMAurban[,c("Value", "Count")], 
                                       by.x="OBJECTID", by.y="Value",all.x = F)
#Check whether any of the "nourban" census blocks are in that dataset 
which(censusblock_FEMAurban_id_HUC6$BLOCKID10 %in% nourban_FEMA_inters$BLOCKID10)

#Check for duplicates due to overlapping FEMA zones in intersection
length(which(duplicated(nourban_FEMA_inters[,c("AREA_INTER", "BLOCKID10")])))
nourban_FEMA_inters <- nourban_FEMA_inters[-which(duplicated(nourban_FEMA_inters[,c("AREA_INTER", "BLOCKID10")])),]

#For each census block, compute the total area with FEMA data (as intersection led to several polygons for each census block)
#Used na.rm=T because a few intersected polygons are so small or spurious that they lead to 0 or NA area
nourban_FEMA_inters_sum  <- ddply(nourban_FEMA_inters, .(BLOCKID10), summarize, AREA_INTEsum=sum(AREA_INTER, na.rm=T))
nourban_FEMA_inters_unique <- nourban_FEMA_inters[-which(duplicated(nourban_FEMA_inters$BLOCKID10)),-ncol(nourban_FEMA_inters)]
nourban_FEMA_inters_unique <- merge(nourban_FEMA_inters_unique, nourban_FEMA_inters_sum, by="BLOCKID10")

#Make sure that area doesn't exceed census block area
FEMAnourban_error <- nourban_FEMA_inters_unique [which(((nourban_FEMA_inters_unique$AREA_INTEsum-nourban_FEMA_inters_unique$AREA_GEOBL)/nourban_FEMA_inters_unique$AREA_GEOBL) > 0.001),]

#Join with HUC6 data 
nourban_FEMA_inters_unique_HUC6 <- merge(censusblock_HUC6_urban_id[,c("BLOCKID10", "HUC6", "AREA_BLOCK")], nourban_FEMA_inters_unique, by ="BLOCKID10",all.x = F)

#Compute population within FEMA assessed area
nourban_FEMA_inters_unique_HUC6$pop_FEMAzone <- with(nourban_FEMA_inters_unique_HUC6, round(POP10*(AREA_BLOCK/AREA_GEOBL)*(AREA_INTEsum/AREA_GEOBL)))

######################################################################################################
#Compute number of people within an area with FEMA data living in a flood zone in each HUC6
######################################################################################################

censusblock_Floodurban_id <- merge(censusblock_Floodurban[,c("Value", "Count")], 
                                   censusflood_IDs[,c("OBJECTID", "BLOCKID10")], by.x="Value", by.y="OBJECTID",all.x = T)
length(which(duplicated(censusblock_Floodurban_id$BLOCKID10)))
dupli <- censusblock_Floodurban_id[which(duplicated(censusblock_Floodurban_id$BLOCKID10)),]

censusblock_Floodurban_id[censusblock_Floodurban_id$BLOCKID10 == '310550075081048',]

censusblock_Floodurban_id_sum <- ddply(censusblock_Floodurban_id, .(BLOCKID10), summarize, sum=sum(Count))

censusblock_Floodurban_id_HUC6 <- merge(censusblock_HUC6_urban_id, censusblock_Floodurban_id_sum, by ="BLOCKID10",all.x = F)
head(censusblock_Floodurban_id_HUC6,100)

#Test for outliers
floodurban_error <- censusblock_Floodurban_id_HUC6[which(censusblock_Floodurban_id_HUC6$sum > censusblock_Floodurban_id_HUC6$Count),]
floodurban_error$diff <- floodurban_error$sum - floodurban_error$Count
#qplot(floodurban_error$diff) + scale_y_log10() + scale_x_log10()
#Only marginal amount of errors, mostly due to erroneous polygons, so simply delete these records
censusblock_Floodurban_id_HUC6 <- censusblock_Floodurban_id_HUC6[-which(censusblock_Floodurban_id_HUC6$sum > censusblock_Floodurban_id_HUC6$Count),]

#Compute number of people living in a flood zone in each census block (AREA_BLOCK is that area that is within each HUC6 while AREA_GEOBL is the total area of the census block)
censusblock_Floodurban_id_HUC6$pop_floodzone<- with(censusblock_Floodurban_id_HUC6, round((POP10*sum/Count)*(AREA_BLOCK/AREA_GEOBL)))

error <- censusblock_Floodurban_id_HUC6[which(censusblock_Floodurban_id_HUC6$pop_floodzone > censusblock_Floodurban_id_HUC6$POP10),]
#Some NA errors due to polygon bugs. Will not affect anything as these are census blocks with 0 population
NAerror <- censusblock_Floodurban_id_HUC6[is.na(censusblock_Floodurban_id_HUC6$pop_floodzone),]
censusblock_Floodurban_id_HUC6 <- censusblock_Floodurban_id_HUC6[-is.na(censusblock_Floodurban_id_HUC6$pop_floodzone),]

#Join with data from census blocks that have no urban pixels
colnames(censusflood_nourban_unique_HUC6)
colnames(censusblock_Floodurban_id_HUC6)
censusblock_Floodurban_HUC6_all <- rbind(censusblock_Floodurban_id_HUC6[,c("HUC6", "pop_floodzone", "pop_totalzone")], censusflood_nourban_unique_HUC6[,c("HUC6", "pop_floodzone", "pop_totalzone")])

#Total number (and total population) in each watershed
censusblock_floodurban_HUC6sum <- ddply(censusblock_Floodurban_HUC6_all, .(HUC6), summarize, HUC6_pop_floodzone=sum(pop_floodzone, na.rm=T), HUC6_pop_totalzone=sum(pop_totalzone, na.rm=T))

######################################################################################################
#Compute number of people living in an area with FEMA data in each census block
######################################################################################################
#Check for inconsistencies i.e. are there always at least as many urban pixels in each census block as in FEMA zone within that census block
FEMAurban_error <- censusblock_FEMAurban_id_HUC6[which(censusblock_FEMAurban_id_HUC6$Count.y > censusblock_FEMAurban_id_HUC6$Count.x),]

str(censusblock_FEMAurban_id_HUc6)
#Compute number of people in each census block living within FEMA zone
censusblock_FEMAurban_id_HUC6$pop_FEMAzone<- with(censusblock_FEMAurban_id_HUC6, round((POP10*Count.y/Count.x)*(AREA_BLOCK/AREA_GEOBL)))

error <- censusblock_FEMAurban_id_HUC6[which(censusblock_FEMAurban_id_HUC6$pop_FEMAzone > censusblock_FEMAurban_id_HUC6$POP10),]

#Join with data from census blocks that have no urban pixels
colnames(nourban_FEMA_inters_unique_HUC6)
colnames(censusblock_FEMAurban_id_HUC6)
censusblock_FEMAurban_HUC6_all <- rbind(censusblock_FEMAurban_id_HUC6[,c("HUC6", "pop_FEMAzone", "pop_totalzone")], nourban_FEMA_inters_unique_HUC6[,c("HUC6", "pop_FEMAzone", "pop_totalzone")])

#Compute total number in watershed
censusblock_FEMAurban_HUC6sum <- ddply(censusblock_FEMAurban_HUC6_all, .(HUC6), summarize, HUC6_pop_FEMAzone=sum(pop_FEMAzone, na.rm=T), HUC6_pop_totalzone=sum(pop_totalzone, na.rm=T))

######################################################################################################
#Compute total population for each HUC6
######################################################################################################
colnames(censusblock_HUC6_urban_id)
HUC6_totalpop <- ddply(censusblock_HUC6_urban_id, .(HUC6), summarize, totalpop=sum(POP10*(AREA_BLOCK/AREA_GEOBL), na.rm=T))

######################################################################################################
#Compute final number for each HUC6
######################################################################################################
HUC6_floodFEMAurban <- merge(censusblock_FEMAurban_HUC6sum, censusblock_floodurban_HUC6sum, by="HUC6", all.x=T)
HUC6_floodFEMAurban <- merge(HUC6_totalpop, HUC6_floodFEMAurban, by="HUC6", all.x=T)

HUC6_floodFEMAurban[which(HUC6_floodFEMAurban$HUC6_pop_FEMAzone > HUC6_floodFEMAurban$totalpop),]
HUC6_floodFEMAurban[which(HUC6_floodFEMAurban$HUC6_pop_floodzone > HUC6_floodFEMAurban$totalpop),]
HUC6_floodFEMAurban[which(HUC6_floodFEMAurban$HUC6_pop_floodzone > HUC6_floodFEMAurban$HUC6_pop_FEMAzone.x),]

#Compute ratio of population in flood zone to population in area with FEMA data
HUC6_floodFEMAurban[HUC6_floodFEMAurban$HUC6_pop_FEMAzone > 0 &
                      !is.na(HUC6_floodFEMAurban$HUC6_pop_FEMAzone), 
                    "flood_FEMA_popratio"] <- with(HUC6_floodFEMAurban[HUC6_floodFEMAurban$HUC6_pop_FEMAzone>0 & !is.na(HUC6_floodFEMAurban$HUC6_pop_FEMAzone),],
                                                   HUC6_pop_floodzone/HUC6_pop_FEMAzone)
qplot(HUC6_floodFEMAurban$flood_FEMA_popratio)

#Compute ratio of population in area with FEMA data to total population
HUC6_floodFEMAurban[HUC6_floodFEMAurban$totalpop > 0 &
                      !is.na(HUC6_floodFEMAurban$HUC6_pop_FEMAzone),
                    "FEMA_total_popratio"] <- with(HUC6_floodFEMAurban[HUC6_floodFEMAurban$totalpop>0 & !is.na(HUC6_floodFEMAurban$HUC6_pop_FEMAzone),],
                                                   round(HUC6_pop_FEMAzone/totalpop, digits=3))
qplot(HUC6_floodFEMAurban$FEMA_total_popratio)

#Compute national ratio of population living in area with FEMA data
sum(HUC6_floodFEMAurban$HUC6_pop_FEMAzone)/sum(HUC6_floodFEMAurban$totalpop)

#Replace NAs by 0
HUC6_floodFEMAurban[is.na(HUC6_floodFEMAurban)] <- 0

######################################################################################################
#Export data
str(HUC6_floodFEMAurban)
colnames(HUC6_floodFEMAurban)
HUC6_floodFEMAurban <- select(HUC6_floodFEMAurban, -HUC6_pop_totalzone.y, -HUC6_pop_totalzone.x)
write.dbf(HUC6_floodFEMAurban, "HUC6_floodFEMA_data.dbf")