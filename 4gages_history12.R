#Author: Mathis Messager
#Contact info: messamat@uw.edu

#Creation date: February 6th 2017
#Last updated: February 23rd 2018

#Objective: 1. this script computes for each year and hydrological unit in the US (HUC4 and HUC6) the number of active gages 
#           i.e. those that recorded data for at least 180 days in the year
#           2. Generates figure 1 of manuscript, a time series of the number of gages that have been continuously active for different time periods (15, 30 years, etc.)
#              the average length of continuous record by gages throughout the US, and a turnover metric i.e. the Sorenson index calculated between each pair of consecutive years


#Edit on July 12th 2017: deleted early years/columns that did not have active gages (<1861) 
#as it led to non-continuous field names and confusing start year. Did not updated other codes accordingly so some inconsistencies might arise.
#Edit on January 11th 2018: compute number of seasonal gages (active at least one day in the year but less than 180 days)
#Edit on January 16th 2018: Use updated USGS gages count from January 2018 data retrieval
#Edit on February 23rd 2018: Update paths, delete turnover calculation and plotting

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

#Function to extract legend from graph as a grob to be re-inserted later
get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

######TO UPDATE##########
#set workspace
setwd("F:/gages_project/results/gages")

########################################################################################################################
#Count the number of gages in each HUC each year that have more than 6 months of data 
########################################################################################################################
#(see Python file: Gages_analysis.py for allgages_merge_HUCjoin.csv generation )
allgages_HUC <- read.csv("allgages_merge_HUCjoin.csv", colClasses = rep("character",9))
Qrec_completeness_HUC <- merge(dis_rec_all, allgages_HUC[,c('site_no','HUC4','HUC6')], by = "site_no", all.x = T)

###Cast data and replace NAs by 0s###
discharge_cast <- cbind(Qrec_completeness_HUC[,c("HUC4","HUC6")],Qrec_completeness_HUC[,1:162])

#Replace number of days by 0 when gage recorded for less than 180 days and 1 if at least 180 days
discharge_cast_sub <- discharge_cast[,4:(ncol(discharge_cast))]
discharge_cast_sub[discharge_cast_sub < 180| is.na(discharge_cast_sub)] <- 0
discharge_cast_sub[discharge_cast_sub >= 180] <- 1
discharge_cast[,4:(ncol(discharge_cast))] <- discharge_cast_sub

#Delete 1854, 1857 and 1859
discharge_cast <- discharge_cast[,-c(4,5,6)]

write.csv(discharge_cast[,-ncol(discharge_cast)], "discharge_yearly_cast_all_20180111.csv", row.names = F, na = "")
#discharge_cast <- read.csv("discharge_yearly_cast_all_20180111.csv", colClasses = c("character", "character", "character", rep("numeric", 157)))

#Count the number of gages in each HUC4 each year that have more than 6 months of data
HUC4_completegages <- ddply(Qrec_completeness_HUC[Qrec_completeness_HUC$datdays >= 180,], .(HUC4, year), summarise, gages_o180days = length(unique((site_no)))) 
HUC4_completegages_cast <- cast(HUC4_completegages, HUC4~year, value = 'gages_o180days')
HUC4_completegages_cast <- HUC4_completegages_cast[!(HUC4_completegages_cast$HUC4 == '' | is.na(HUC4_completegages_cast$HUC4)),]
HUC4_completegages_cast[is.na(HUC4_completegages_cast)] <- 0
write.csv(HUC4_completegages_cast, "HUC4_completegages_20180116.csv", row.names = F)

#count the number of gages in each HUC6 each year that have more than 6 months of data
HUC6_completegages <- ddply(Qrec_completeness_HUC[Qrec_completeness_HUC$datdays >= 180,], .(HUC6, year), summarise, gages_o180days = length(unique((site_no)))) 
HUC6_completegages_cast <- cast(HUC6_completegages, HUC6~year, value = 'gages_o180days')
HUC6_completegages_cast <- HUC6_completegages_cast[!(HUC6_completegages_cast$HUC6 == '' | is.na(HUC6_completegages_cast$HUC6)),]
HUC6_completegages_cast[is.na(HUC6_completegages_cast)] <- 0
write.csv(HUC6_completegages_cast, "HUC6_completegages_20180116.csv")
#HUC6_completegages_cast <- read.csv("HUC6_completegages_20180116.csv", colClasses = c("character","character", rep("numeric",155)), row.names = 1)

############# ANALYZE SEASONAL GAGES ##################
#Create separate file for gages that were active for at least 1 day but less than 180 days
#0 if either completely inactive or active more than 180 days
#1 if active > 1 day < 180 days
discharge_cast_seasonal <- cast(Qrec_completeness_HUC, site_no + HUC4 + HUC6 ~ year, value='datdays')
discharge_cast_seasonal[is.na(discharge_cast_seasonal)] <- 0
discharge_cast_sub_seasonal <- discharge_cast_seasonal[,4:(ncol(discharge_cast_seasonal)-1)]
discharge_cast_sub_seasonal[discharge_cast_sub_seasonal < 180 & discharge_cast_sub_seasonal >= 1] <- 1
discharge_cast_sub_seasonal[discharge_cast_sub_seasonal >= 180| is.na(discharge_cast_sub_seasonal)] <- 0
discharge_cast_seasonal[,4:(ncol(discharge_cast_seasonal)-1)] <- discharge_cast_sub_seasonal
write.csv(discharge_cast_seasonal[,-ncol(discharge_cast_seasonal)], "discharge_yearly_cast_all_seasonal.csv", row.names = F, na = "")
#discharge_cast_seasonal <- read.csv("discharge_yearly_cast_all_seasonal.csv", colClasses = c("character", "character", "character", rep("numeric", 156)))

#IGNORE
#Count number of years with over 10 yrs of data, create dummy variable to import in Python, and export -- for Gage-Dam pairing
# colnames(discharge_cast)
# discharge_cast$active_years <-  rowSums(discharge_cast[,4:ncol(discharge_cast)])
#nrow(discharge_cast[discharge_cast$active_years >= 10,])
#discharge_cast_copy <- rbind(discharge_cast, c(rep("character", 3), rep(999, 160)))
#write.csv(discharge_cast_copy, "discharge_yearly_cast_all_o10.csv", row.names = F, na = "")
 
##################################################################################
# TOTAL COUNT OF GAGES
##################################################################################
#Compute number of active gages
gages_Q_sum <- apply(discharge_cast[,4:ncol(discharge_cast)], 2, sum)
gages_Q_sum <- data.frame(year = substr(colnames(discharge_cast[,4:(ncol(discharge_cast))]),2,5), count = gages_Q_sum)

#Compute for each year and gage, the number of years that it has been continuously active
dis_rec_all <- read.dbf("discharge_all_daily20180111.dbf")
dis_rec_all <- dis_rec_all[with(dis_rec_all, order(site_no, year)),]
dis_rec_all[1,"year_acc"] <- 0
for (i in 2:nrow(dis_rec_all)) {
  if (dis_rec_all$site_no[i] == dis_rec_all$site_no[i-1]) {
    if (dis_rec_all$datdays[i] >= 180) { 
      dis_rec_all[i,"year_acc"] <- dis_rec_all[i-1,"year_acc"] + 1
    } else {dis_rec_all[i,"year_acc"] <- 0}
  } else {
    if (dis_rec_all$datdays[i] >= 180) { 
      dis_rec_all[i,"year_acc"] <- 1
    } else {
      dis_rec_all[i,"year_acc"] <- 0
    }
  }
}

#Compute number of seasonal gages
gages_Q_sum_seasonal <- apply(discharge_cast_seasonal[,4:ncol(discharge_cast_seasonal)], 2, sum)
gages_Q_sum_seasonal <- data.frame(year = colnames(discharge_cast_seasonal[,4:(ncol(discharge_cast_seasonal))]), count = gages_Q_sum_seasonal)

#Merge active and seasonal gages to compute percentage
gages_Q_sum_merge <- merge(gages_Q_sum, gages_Q_sum_seasonal, by='year')
colnames(gages_Q_sum_merge) <- c('year', 'count_active', 'count_seasonal')
gages_Q_sum_merge$perc_seasonal <- 100*gages_Q_sum_merge$count_seasonal/(gages_Q_sum_merge$count_active+gages_Q_sum_merge$count_seasonal)

#####################################################################################################################
# COUNT OF GAGES BASED ON LENGTH OF RECORD
#####################################################################################################################
#Function to compute, for each year, the number of gages that have been active for more than a given number of years
#i.e. How many gages have been active for more than 15 years in 2016?
dis_rec_cast <- cast(dis_rec_all, site_no ~ year, value='year_acc')
gages_Q_sum <- data.frame(year = colnames(dis_rec_cast[,2:(ncol(dis_rec_cast)-1)]))
count_years <- function(rec = dis_rec_cast[,-ncol(dis_rec_cast)],number) {
  dis_rec_cast_num <- rec
  dis_rec_cast_num[dis_rec_cast_num < number| is.na(dis_rec_cast_num)] <- 0
  dis_rec_cast_num[dis_rec_cast_num >= number] <- 1
  apply(dis_rec_cast_num[,2:ncol(dis_rec_cast_num)], 2, sum)
}
#Compute a time series of the number of gages active for more than 1, 15, 30, 50, and 100 years
gages_Q_sum[,"count"] <- count_years(number = 1)
gages_Q_sum[,"count15"] <- count_years(number = 15)
gages_Q_sum[,"count30"] <- count_years(number = 30)
gages_Q_sum[,"count50"] <- count_years(number = 50)
gages_Q_sum[,"count100"] <- count_years(number = 100)
#Compute for each year, the average number of years gages in the network have been active
gages_Q_sum[,"avg_age"] <- colMeans(dis_rec_cast[,2:(ncol(dis_rec_cast)-1)], na.rm = T)

write.csv(gages_Q_sum, "F:/gages_project/results/figures/Figure_2/total_gages_1_16_2018_average.csv")

#####################################################################################################################
# FIGURE 1
#####################################################################################################################
#Then, create time series of number of gages and length of record
fig1_2 <- ggplot(gages_Q_sum, aes(x=as.Date(paste(format(year, format= "%Y"), '-01-01', sep = "")))) + 
  theme_bw() +
  geom_area(data=gages_Q_sum,aes(y=count), color = "#ffffff", fill = "#f0f0f0", alpha = 1/3, size = 0) + 
  geom_area(data=gages_Q_sum,aes(y=count15), color = "#bdbdbd", fill = "#bdbdbd", alpha = 1/3, size = 0) + 
  geom_area(data=gages_Q_sum,aes(y=count30), color = "#969696", fill = "#969696", alpha = 1/3, size = 0) + 
  geom_area(data=gages_Q_sum,aes(y=count50), color = "#525252", fill = "#525252", alpha = 1/3, size = 0) + 
  geom_area(data=gages_Q_sum,aes(y=count100), color = "#000000", fill = "#000000", alpha = 1/3, size = 0) + 
  geom_line(data=gages_Q_sum,aes(y=count), color = "#000000", size=0.5) +
  scale_x_date(labels = date_format("%Y"),breaks = as.Date(paste0(c(seq(1860,2015,20),2015),"-01-01")), expand=c(0,0), limits=as.Date(paste0(c(1860,2016),'-01-01'))) +
  scale_y_continuous(limit=c(0, 9000),expand=c(0,0), breaks=c(0,1000,2000,3000,4000,5000,6000,7000,8000))+
  scale_size(range = c(0,4), guide = 'none') +
  annotate("text", x = as.Date('2004-01-01'), y = c(380, 2700, 3650, 5300, 7900), 
           label = c("> 100 yrs","> 50 yrs","> 30 yrs","> 15 yrs", "All"),
           color = c("#000000", "#252525", "#525252","#737373",'#969696'),
           size = 4) +
  labs(x = "Year", y = "Number of active gages in USGS hydrometric network") +
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
#fig1_2

#Export
pdf(file = "F:/gages_project/results/figures/Figure_2/total_gages_1_16_2018.pdf", width = 6, height = 7)
fig1_2
dev.off()

############
#Compute the average number of days with data in a year for gages that were not activated that year (because counting gages that were activated that year
#would bring down the average in years when many gages are added to the network
dis_rec_all <- dis_rec_all[with(dis_rec_all, order(site_no, year)),]
dis_rec_cast <- cast(dis_rec_all, site_no ~ year, value='year_acc')

for (i in 2:nrow(dis_rec_all)) {
  if ((dis_rec_all[i,"site_no"] == dis_rec_all[i-1,"site_no"]) & as.character(as.numeric(dis_rec_all[i,"year"])-1) == dis_rec_all[i-1,"year"]) {
    dis_rec_all[i,"prevyr"] <- 1
  } else {
    dis_rec_all[i,"prevyr"] <- 0
  }
}

#####################################################################################################################
# FIGURE FOR REVIEWER: number and percentage of seasonal gages over time
#####################################################################################################################
#Then, create time series of number of gages and length of record
fig1_seasonal <- ggplot(gages_Q_sum_merge, aes(x=as.Date(paste(format(year, format= "%Y"), '-01-01', sep = "")))) + 
  theme_bw() +
  geom_line(data=gages_Q_sum_merge,aes(y=count_seasonal), color = "#e41a1c", size=1) +
  scale_x_date(labels = date_format("%Y"),breaks = as.Date(paste0(c(seq(1860,2015,20),2016),"-01-01")), expand=c(0,0), limits=as.Date(paste0(c(1860,2016),'-01-01'))) +
  scale_y_continuous(limit=c(0, 600),expand=c(0,0), breaks=c(0,100,200,300,400,500,600))+
  scale_size(range = c(0,4), guide = 'none') +
  labs(x = "Year", y = "# of seasonal gages in USGS hydrometric network") +
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
fig1_seasonal

fig1_seasonal_perc <- ggplot(gages_Q_sum_merge, aes(x=as.Date(paste(format(year, format= "%Y"), '-01-01', sep = "")))) + 
  theme_bw() +
  geom_line(data=gages_Q_sum_merge,aes(y=perc_seasonal), color = "#377eb8", size=1) +
  scale_x_date(labels = date_format("%Y"),breaks = as.Date(paste0(c(seq(1860,2015,20),2016),"-01-01")), expand=c(0,0), limits=as.Date(paste0(c(1860,2016),'-01-01'))) +
  scale_y_continuous(limit=c(0, 100),expand=c(0,0), breaks=c(0,10,25,50,75,100))+
  scale_size(range = c(0,4), guide = 'none') +
  labs(x = "Year", y = "% of seasonal gages in USGS hydrometric network") +
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
fig1_seasonal_perc

#Export
pdf(file = "F:/Miscellaneous/Hydro_classes/Figures/seasonal_gages_1_11_2018.pdf", width = 6, height = 12)
grid.arrange(fig1_seasonal,  fig1_seasonal_perc, ncol=1)
dev.off()
