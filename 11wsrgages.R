#Author: Mathis Messager
#Contact info: messamat@uw.edu

#Creation date: March 14th 2018

#Objective: Relate gages to wild and scenic rivers and examine their trajectory over time

library(chron)
library(reshape)
library(ggplot2)
library(plyr)
library(dplyr)
library(scales) 
library(grid)
library(gridExtra)
library(lubridate)
library(lattice)
library(Hmisc)
library(ggthemes)
library(foreign)

######TO UPDATE##########
#set workspace
setwd("F:/gages_project/results/gages")
#gage records
discharge_cast <- read.csv("discharge_yearly_cast_all_20180111.csv", colClasses = c("character", "character", "character", rep("numeric", 157)))
#gages on Wild and Scenic rivers
wsrgages <- read.dbf("gages_wildandscenic_select.dbf")

wsrgages_discharge <- merge(wsrgages, discharge_cast, by='site_no', all.y=F)
write.dbf(wsrgages_discharge, "gages_wildandscenic_discharge.dbf")

wsrgages_sum <- colSums(wsrgages_discharge[,10:166])
wsrgages_summary <- data.frame(ngages=wsrgages_sum, year=as.numeric(substr(colnames(wsrgages_discharge[,10:166]),2,5)))
wsrfig <- ggplot(wsrgages_summary, aes(x=year, y=ngages, group=1)) + 
  geom_path(aes(colour = ngages),lineend = 'round', size=1.5) +
  theme_classic() + 
  scale_color_distiller(palette = "RdYlBu", limit = c(0,180), name="Average length of record",
                        guide = guide_colorbar(direction = 'horizontal',title.position = "top"))+
  scale_x_continuous(breaks=c(1850,1900,1950,2000,2016), limits=c(1850,2017), expand=c(0,5)) +
  scale_y_continuous(name='Number of gages on Wild and Scenic river', expand=c(0,0)) +
  theme(legend.position = "None",
        text=element_text(size=14))
png(filename = "F:/gages_project/results/figures/WSR/WSR_gages.png", width = 6, height = 6, units = "in", res = 300)
wsrfig
dev.off()
