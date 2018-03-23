#Author: Mathis Messager
#Contact info: messamat@uw.edu

#Creation date: February 6th 2017
#Last updated: February 23rd 2018

#Objective:    Generates figure 1 of manuscript, a time series of the number of gages from the GRDC that have been continuously active 
#              for different time periods (15, 30 years, etc.) and the average length of continuous record by gages throughout the US

library(chron)
library(reshape)
library(ggplot2)
library(plyr)
library(scales) 
library(grid)
library(gridExtra)
library(lubridate)
library(lattice)
library(Hmisc)
library(ggthemes)
library(matrixStats)
library(miscTools)

#Function to extract legend from graph as a grob to be re-inserted later
get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

######TO UPDATE##########
#set workspace
setwd("F:/gages_project")

#Import GRDC data
GRDCdat <- read.csv("data/grdc/grdc_stations_201801/GRDC_Stations.csv")

##################################################################################
# TOTAL COUNT OF GAGES
##################################################################################
#Compute the total number of gages per year
gagerec <- data.frame(year=min(GRDCdat$t_start):max(GRDCdat$t_end), count=NA)
gagerec_activity <- data.frame(GRDCdat$grdc_no)
for (i in min(GRDCdat$t_start):max(GRDCdat$t_end)) {
  gagerec[gagerec$year == i,"count"] <- length(which(GRDCdat$t_start <= i & GRDCdat$t_end >= i))
  gagerec_activity[which(GRDCdat$t_start <= i & GRDCdat$t_end >= i),as.character(i)] <- with(GRDCdat[which(GRDCdat$t_start <= i & GRDCdat$t_end >= i),], i-t_start)
  gagerec_activity[which(!(GRDCdat$t_start <= i & GRDCdat$t_end >= i)),as.character(i)] <- 0
}

#####################################################################################################################
# COUNT OF GAGES BASED ON LENGTH OF RECORD
#####################################################################################################################
#Function to compute, for each year, the number of gages that have been active for more than a given number of years
#i.e. How many gages have been active for more than 15 years in 2016?
gagerec_activity[gagerec_activity==0] <- NA
count_years <- function(rec = gagerec_activity,number) {
  dis_rec_cast_num <- rec
  dis_rec_cast_num[dis_rec_cast_num < number| is.na(dis_rec_cast_num)] <- 0
  dis_rec_cast_num[dis_rec_cast_num >= number] <- 1
  apply(dis_rec_cast_num[,2:ncol(dis_rec_cast_num)], 2, sum)
}
#Compute a time series of the number of gages active for more than 1, 15, 30, 50, and 100 years
gagerec[,"count15"] <- count_years(number = 15)
gagerec[,"count30"] <- count_years(number = 30)
gagerec[,"count50"] <- count_years(number = 50)
gagerec[,"count100"] <- count_years(number = 100)
#Compute for each year, the average number of years gages in the network have been active
gagerec[,"avg_age"] <- colMeans(gagerec_activity[,2:(ncol(gagerec_activity))], na.rm = T)
gagerec[,"med_age"] <- colMedians(as.matrix(gagerec_activity[,2:(ncol(gagerec_activity))]), na.rm=T)

write.csv(gagerec, "results/figures/Figure_1/total_gages_1_16_2018_median.csv",row.names=F)
#####################################################################################################################
# FIGURE 1
#####################################################################################################################
#Create time series of number of gages and length of record
fig1_2 <- ggplot(gagerec[gagerec$year <= 2010,], aes(x=as.Date(paste(format(year, format= "%Y"), '-01-01', sep = "")))) + 
  theme_bw() +
  geom_area(aes(y=count), color = "#ffffff", fill = "#f0f0f0", alpha = 1/3, size = 0.5) + 
  geom_area(aes(y=count15), color = "#bdbdbd", fill = "#bdbdbd", alpha = 1/3, size = 0) + 
  geom_area(aes(y=count30), color = "#969696", fill = "#969696", alpha = 1/3, size = 0) + 
  geom_area(aes(y=count50), color = "#525252", fill = "#525252", alpha = 1/3, size = 0) + 
  geom_area(aes(y=count100), color = "#000000", fill = "#000000", alpha = 1/3, size = 0) + 
  geom_line(aes(y=count), color = "#000000", size=0.5) +
  geom_area(data= gagerec[gagerec$year > 2010,], aes(y=count), color = "#ffffff", fill = "#f0f0f0", alpha = 1/10, size = 0.5) + 
  geom_area(data= gagerec[gagerec$year > 2010,], aes(y=count15), color = "#bdbdbd", fill = "#bdbdbd", alpha = 1/10, size = 0) + 
  geom_area(data= gagerec[gagerec$year > 2010,], aes(y=count30), color = "#969696", fill = "#969696", alpha = 1/10, size = 0) + 
  geom_area(data= gagerec[gagerec$year > 2010,], aes(y=count50), color = "#525252", fill = "#525252", alpha = 1/10, size = 0) + 
  geom_area(data= gagerec[gagerec$year > 2010,], aes(y=count100), color = "#000000", fill = "#000000", alpha = 1/10, size = 0) + 
  geom_line(data= gagerec[gagerec$year > 2010,], aes(y=count), color = "#000000", size=0.5, alpha = 1/3) +
  #Average age time series
  #geom_path(aes(y=count, colour = med_age),size = 2.5,lineend = 'round') +
  scale_x_date(labels = date_format("%Y"),breaks = as.Date(paste0(c(seq(1800,2015,20),2010),"-01-01")), expand=c(0,0), limits=as.Date(paste0(c(1800,2016),'-01-01'))) +
  scale_y_continuous(limit=c(0, 9000),expand=c(0,0), breaks=c(0,1000,2000,3000,4000,5000,6000,7000,8000))+
  scale_size(range = c(0,4), guide = 'none') +
  annotate("text", x = c(rep(as.Date('1987-01-01'),4),as.Date('1982-06-01')), y = c(250, 910, 2100, 4000, 6000), 
           label = c("> 100 yrs","> 50 yrs","> 30 yrs","> 15 yrs", "All"),
           color = c("#000000", "#252525", "#525252","#737373",'#969696'),
           size = 4) +
  labs(x = "Year", y = "Global number of stream gages reporting to GRDC") +
  theme(axis.title=element_text(size=16), 
        axis.text = element_text(size = 12), 
        axis.text.x = element_text(angle=30,hjust=1),
        legend.position = c(.30,.60),
        legend.text = element_text(size = 12),
        legend.title = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.background = element_blank(),
        plot.background = element_blank(),
        legend.background = element_blank())
fig1_2

#Export
# png(filename = "F:/Miscellaneous/Hydro_classes/Figures/total_gages_7_7_2017.png", width = 6, height = 7, units = "in", res = 300)
# fig1_2
# dev.off()

pdf(file = "results/figures/Figure_1/total_gages_1_16_2018_GRDC.pdf", width = 6, height = 7)
fig1_2
dev.off()
