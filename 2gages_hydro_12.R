#Author: Mathis Messager
#Contact info: messamat@uw.edu
#First created: June 2016
#Last modified: February 23rd 2018

#Objective: This script extracts dates of recorded discharge for all stream gage stations in the US.
#It downloads all daily mean discharge data from the USGS water service (until January 2018 at the time of the most recent analysis)
#It does not take in account periods for which data were estimated due to malfunction or lack of maintenance but includes periods when ice precluded recording data

#January 2018 edit: re-run script in January 2018 to test whether 2016 drop in number of gages could be explained by artefacts in provisional data availability.
#February 2018 edit: update paths and delete extraneous parts that are not relevant to final publication (i.e. downloading instantaneous values and combining with daily data)

library(dataRetrieval)
library(ggplot2)
library(pryr)
library(plyr)
library(dplyr)
library(reshape)
library(reshape2)
library(data.table)
library(foreign)

######TO UPDATE##########
#set workspace
setwd("F:/gages_project/results/gages")

#Retrieve all basic info for gages that have discharge data for the entire US
#1-56 state codes (exclude Puerto Rico, Virgin Islands, Guam, etc.). For more info, type ?stateCd
state_list <- stateCd$STATE
US <- adply(state_list[1:51], 1, function(x) {whatNWISsites(stateCd = x, parameterCd = "00060")})
write.csv(US, "US_gages_dis20180111.csv", row.names=F)
US <- read.csv("US_gages_dis20180111.csv", colClasses = c("character", "character", "character", "character","character", "numeric", "numeric", "character", "character"))

###############################################################################################################################################
# Part 1: Download daily mean discharge data
###############################################################################################################################################
site_seq <- c(0,seq(250, nrow(US), 250), nrow(US)) #Process only 250 stations at a time. Otherwise, API craps out (depends on length of record, but 250 is a safe bet)

mapply(function(x,y) {
  print(y)
  US_sub <- US$site_no[(x+1):y]
  parametercd <- "00060" #Discharge parameter request to USGS API
  startdate <- ""
  enddate <- ""
  statcd <- "00003" #Daily mean
  
  #Download data from USGS API
  discharge <- readNWISdv(US_sub, parametercd, startdate, enddate, statcd)
  #Remove estimated values
  discharge <- discharge[!(grepl("A e", discharge$X_00060_00003_cd, perl=TRUE)), ]
  #Create a table
  tab_name <- paste("discharge_", as.character(x), as.character(y), sep = "_")
  assign(tab_name, discharge)
  write.csv(get(tab_name), paste("F:/gages_project/data/discharge_data_raw20180111/", tab_name, ".csv", sep = ""), row.names = F)
}, site_seq[-length(site_seq)], site_seq[-1])
  
dis_list <- list.files("F:/gages_project/data/discharge_data_raw20180111")
dis_list <- paste("F:/gages_project/data/discharge_data_raw20180111/", dis_list, sep = "/")
#Import all tables in a list of df and compute the number of days that each gage was active for each year
dis_rec <- ldply(dis_list, function(x) {
  print(x)
  disdat <- read.csv(x, colClasses = c("character","character", "character", "character", "character"))
  disdat$year <- substr(disdat$Date,1,4)
  ddply(disdat, .(site_no, year), summarise, datdays = length(unique((Date)))) 
})
which(duplicated(dis_rec))
length(unique(dis_rec$site_no))

write.dbf(dis_rec, "discharge_all_daily20180111.dbf")
#dis_rec <- read.dbf("discharge_all_daily20180111.dbf")

#Index and cast data
dt <- as.data.table(dis_rec)
dt[, index:=seq_along(year), by = site_no]
discharge_castdt <- dcast(dt, site_no ~ year, value.var = 'datdays')
#Format
discharge_castdtinfo <- merge(discharge_castdt, US, by = "site_no", all.x = T)
write.dbf(discharge_castdtinfo, "discharge_castdtinfo20180111.dbf")

#-------> Next step: "2Gages_Analysis_2.py"