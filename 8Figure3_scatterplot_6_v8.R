#Author: Mathis Messager
#Contact info: messamat@uw.edu
#Creation date: April 5th 2017
#Last updated: March 23rd 2018
#Objective: Produce a scatterplot of fish biodiversity, water scarcity, and flood risk, against the quasi-extinction probability of HUC6

####################################################################################################################################
library(ggplot2)
library(grid)
library(gridExtra)
library(RColorBrewer)
library(foreign)

setwd("F:/gages_project/results")
#Read in quasi extinction probability
quasiext_HUC6 <- read.csv("quasi-extinction/version_7/Quasiext.huc6_v7.csv", row.names = 1)
str(quasiext_HUC6)
#Format HUC6 IDs to character (generally read in as numeric)
quasiext_HUC6[quasiext_HUC6$rownames_HUC6 < 100000,'HUC6'] <- paste('0',as.character(quasiext_HUC6[quasiext_HUC6$rownames_HUC6 < 100000,'rownames_HUC6']),sep="")
quasiext_HUC6[quasiext_HUC6$rownames_HUC6 >= 100000,'HUC6'] <- as.character(quasiext_HUC6[quasiext_HUC6$rownames_HUC6 >= 100000,'rownames_HUC6'])
write.dbf(quasiext_HUC6, "quasi-extinction/version_7/Quasiext.huc6_v7.dbf")

#Get current number of gages in each HUC
histgages_HUC6 <- read.csv("gages/HUC6_completegages_20180116.csv", row.names = 1, colClasses = c("character", "character", rep("numeric", 155)))

################################
# PLOT WATER SCARCITY
################################
#Get water scarcity data for each HUC
NDC_HUC6 <- read.csv("water_Scarcity/HUC6_NDC_pr.csv", colClasses= c("numeric","character","numeric","numeric",'numeric','character','character',rep('numeric',7)))

#Collate data
colnames(NDC_HUC6)
colnames(quasiext_HUC6)
NDC_HUC6_dat <- merge(quasiext_HUC6, NDC_HUC6, by="HUC6")
NDC_HUC6_dat <- merge(NDC_HUC6_dat, histgages_HUC6[,c("HUC6", "X2016")], by="HUC6")

#Plot of NDC against quasi-extinction probability with categories
med_NDC_HUC = median(NDC_HUC6_dat$NDC_HUC, na.rm=T)
NDC_HUC6_dat[NDC_HUC6_dat$Quasiext.huc6 >= 0.5 & NDC_HUC6_dat$NDC_HUC >= 2,"Category"] <- "HighNDC_HighExtProba"
NDC_HUC6_dat[NDC_HUC6_dat$Quasiext.huc6 >= 0.5 & NDC_HUC6_dat$NDC_HUC < 2,"Category"] <- "LowNDC_HighExtProba"
NDC_HUC6_dat[NDC_HUC6_dat$Quasiext.huc6<0.5 & NDC_HUC6_dat$NDC_HUC>=2 & !is.na(NDC_HUC6_dat$NDC_HUC),"Category"] <- "HighNDC_LowExtProba"
NDC_HUC6_dat[NDC_HUC6_dat$Quasiext.huc6< 0.5 & NDC_HUC6_dat$NDC_HUC < 2 & !is.na(NDC_HUC6_dat$NDC_HUC),"Category"] <- "LowNDC_LowExtProba"
write.dbf(NDC_HUC6_dat, "water_scarcity/HUC6NDC_category.dbf")

#Set up continuous color palette based on NDC value
#Choose example palette
my.col <- colorRampPalette(brewer.pal(9, "Blues"))
my.col2 <- colorRampPalette(brewer.pal(9, "Reds"))
#Create 100 intervals based on NDC values
int<- seq(min(NDC_HUC6_dat$NDC_HUC,na.rm=T),max(NDC_HUC6_dat$NDC_HUC,na.rm=T),diff(range(NDC_HUC6_dat$NDC_HUC, na.rm=T))/100)
#Assign each HUC6 NDC value to an interval and assign color from palette based on category
NDC_HUC6_dat$cat <- cut(NDC_HUC6_dat$NDC_HUC, breaks=int, labels=my.col(150)[51:150])
NDC_HUC6_dat$cat2 <- cut(NDC_HUC6_dat$NDC_HUC, breaks=int, labels=my.col2(150)[51:150])

#
HUC6_scarcity_plot <- ggplot(NDC_HUC6_dat, aes(x=Quasiext.huc6, y=NDC_HUC, size = X2016)) + 
  geom_point(aes(color=NDC_HUC), alpha=0.8) +
  geom_point(data=NDC_HUC6_dat[NDC_HUC6_dat$NDC_HUC > 10,],color='#d13d38', alpha=0.8) +
  # geom_point(data=NDC_HUC6_dat[NDC_HUC6_dat$NDC_HUC >= 1,],alpha=0.8, color='#D66460') + 
  # geom_point(data=NDC_HUC6_dat[NDC_HUC6_dat$NDC_HUC < 1,],alpha=0.8, color='#E8E8E8') + 
  geom_point(shape=1, alpha =1/10) +
  scale_color_gradient2(midpoint=1, low='#E8E8E8', high='#d13d38', space='Lab',limits = c(0, 10)) +
  geom_vline(xintercept= 0.5)+
  geom_hline(yintercept= 2)+
  theme_classic() +
  labs(x="Streamgage network decline risk (%)", y="Water scarcity Normalized Deficit Cumulated (NDC)") +
  scale_x_continuous(limits=c(0,1)) + 
  scale_y_continuous(limits=c(0,50),expand=c(0,0), breaks=c(0,10,20,30,40)) +
  scale_size(range=c(0.5,7), name="Number of gages in 2016", breaks=c(10,50,125), guide=FALSE) + 
  #guides(color = guide_legend(override.aes = list(size=5, shape=16))) + 
  theme(legend.justification = c(0, 1),
        legend.position='none',
        legend.margin = margin(-5,0,0,0),
        legend.title = element_text(face = "bold"),
        legend.background = element_blank(),
        text = element_text(size = 12),
        axis.text.x=element_blank(),
        axis.title.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text = element_text(size=14))
HUC6_scarcity_plot

# pdf("F:/Miscellaneous/Hydro_classes/Analysis/Figure3_1.pdf", width=5,height=8)
# grid.arrange(HUC6_scarcity_plot,HUC6_fishdiv_plot, ncol=1)
# dev.off()

################################
# PLOT FLOOD RISK
################################
HUC6_floodFEMAurban <- read.dbf("flood/HUC6_floodFEMA_data.dbf")

#Collate data
colnames(HUC6_floodFEMAurban)
colnames(quasiext_HUC6)
colnames(histgages_HUC6)
HUC6_floodFEMAurban_dat <- merge(quasiext_HUC6, HUC6_floodFEMAurban, by="HUC6")
HUC6_floodFEMAurban_dat <- merge(HUC6_floodFEMAurban_dat, histgages_HUC6[,c("HUC6", "X2016")], by="HUC6")

#Plot of percentage of people living in flood zone against quasi-extinction probability
ggplot(HUC6_floodFEMAurban_dat, aes(x=Quasiext.huc6, y=flood_FEMA)) + geom_point()
#Plot of number of people living in a flood zone against quasi-extinction probability
ggplot(HUC6_floodFEMAurban_dat, aes(x=Quasiext.huc6, y=HUC6_pop_floodzone)) + geom_point() + scale_y_sqrt()
#Plot of % people living in flood zone against quasi-extinction probability
med_floodpop = median(HUC6_floodFEMAurban_dat$flood_FEMA, na.rm=T)
HUC6_floodFEMAurban_dat[HUC6_floodFEMAurban_dat$Quasiext.huc6 >= 0.5 & HUC6_floodFEMAurban_dat$flood_FEMA >= 0.05,"Category"] <- "HighRisk_HighExtProba"
HUC6_floodFEMAurban_dat[HUC6_floodFEMAurban_dat$Quasiext.huc6 >= 0.5 & HUC6_floodFEMAurban_dat$flood_FEMA < 0.05,"Category"] <- "LowRisk_HighExtProba"
HUC6_floodFEMAurban_dat[HUC6_floodFEMAurban_dat$Quasiext.huc6<0.5 & HUC6_floodFEMAurban_dat$flood_FEMA>=0.05,"Category"] <- "HighRisk_LowExtProba"
HUC6_floodFEMAurban_dat[HUC6_floodFEMAurban_dat$Quasiext.huc6< 0.5 & HUC6_floodFEMAurban_dat$flood_FEMA < 0.05,"Category"] <- "LowRisk_LowExtProba"
HUC6_floodFEMAurban_dat[HUC6_floodFEMAurban_dat$flood_FEMA == 0,"Category"] <- "NoFEMAdata"
HUC6_floodFEMAurban_dat <- HUC6_floodFEMAurban_dat[as.numeric(HUC6_floodFEMAurban_dat$HUC6) < 200000,]
write.dbf(HUC6_floodFEMAurban_dat, "flood/HUC6floodrisk_category.dbf")

#Set up continuous color palette based on NDC value
#Choose example palette
my.col <- colorRampPalette(brewer.pal(11, "BrBG"))
#Create 100 intervals based on NDC values
int<- seq(min(HUC6_floodFEMAurban_dat$flood_FEMA,na.rm=T),max(HUC6_floodFEMAurban_dat$flood_FEMA,na.rm=T),diff(range(HUC6_floodFEMAurban_dat$flood_FEMA, na.rm=T))/100)
#Assign each HUC6 NDC value to an interval and assign color from palette based on category
HUC6_floodFEMAurban_dat$cat <- cut(HUC6_floodFEMAurban_dat$flood_FEMA, breaks=int, labels=rev(my.col(300)[1:100]))
HUC6_floodFEMAurban_dat$cat2 <- cut(HUC6_floodFEMAurban_dat$flood_FEMA, breaks=int, labels=my.col(300)[201:300])

HUC6_floodrisk_plot <-ggplot(HUC6_floodFEMAurban_dat, aes(x=100*Quasiext.huc6, y=flood_FEMA, size = X2016)) + 
  geom_point(alpha=0.8, aes(color=flood_FEMA)) + 
  geom_point(data=HUC6_floodFEMAurban_dat[HUC6_floodFEMAurban_dat$flood_FEMA >= 0.10,],alpha=0.8, color='#51a6c1') + 
  # geom_point(data=HUC6_floodFEMAurban_dat[HUC6_floodFEMAurban_dat$flood_FEMA >= 0.05,],alpha=0.8, color='#69ABC0') + 
  # geom_point(data=HUC6_floodFEMAurban_dat[HUC6_floodFEMAurban_dat$flood_FEMA < 0.05,],alpha=0.8, color='#E8E8E8') + 
  geom_point(shape=1, alpha =1/10) +
  geom_point(data=HUC6_floodFEMAurban_dat[HUC6_floodFEMAurban_dat$flood_FEMA==0,],alpha=0.5, color="#1a1a1a") + 
  scale_color_gradient2(low='#E8E8E8', high='#51a6c1', space='Lab',limits = c(0, 0.10)) +
  #scale_color_identity()+
  geom_vline(xintercept= 50)+
  geom_hline(yintercept= 0.05)+
  theme_classic() +
  labs(x="Streamgage network decline risk (%)", y=expression("Flood risk % population living within flood zone")) +
  scale_x_continuous(limits=c(0,100)) + 
  scale_y_continuous(limits=c(0,0.4),expand=c(0,0), breaks=c(0,0.1,0.2,0.3,0.4)) +
  #Top-right, top-left, lower-right, lower-left (142,1,82; 39,100,25; 222,119,174; 127,188,65)
  #scale_size(range=c(0.5,7), name="Number of gages in 2016", breaks=c(10,50,125), guide=FALSE) + 
  guides(color = guide_legend(override.aes = list(size=5, shape=16))) + 
  theme(legend.justification = c(0, 1),
        legend.margin = margin(-20,0,0,0),
        legend.title = element_text(face = "bold"),
        legend.background = element_blank(),
        legend.position='none',
        text=element_text(size=12),
        axis.text.x=element_blank(),
        axis.title.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text = element_text(size=14))
#HUC6_floodrisk_plot

################################
# PLOT FISH DIVERSITY
################################
fishdiv_HUC6 <- read.csv("fish/HUC6div.csv", 
                         colClasses=c("character","numeric","numeric", "numeric","numeric"))

#Collate data
colnames(fishdiv_HUC6)
colnames(quasiext_HUC6)
colnames(histgages_HUC6)
fishdiv_HUC6_dat <- merge(quasiext_HUC6, fishdiv_HUC6, by.y="HUC6_id", by.x="HUC6")
fishdiv_HUC6_dat <- merge(fishdiv_HUC6_dat, histgages_HUC6[,c("HUC6", "X2016")], by="HUC6")

#Plot of Endemism Weighted unit richness (EWU TE) for Threatened and Endangered Species against quasi-extinction probability
ggplot(fishdiv_HUC6_dat, aes(x=Quasiext.huc6, y=TE_EWU_numb)) + geom_point()
#Plot of number of Threatened and Endangered Species against quasi-extinction probability
ggplot(fishdiv_HUC6_dat, aes(x=Quasiext.huc6, y=TE_Count)) + geom_point()
#Prepare data for plot of Endemism Weighted unit richness against quasi-extinction probability
#Classify watersheds in high and low endemism units based on the median EWU and in high and low extinction probability based on 0.5 quasi-ext proba
med_EWU = median(fishdiv_HUC6_dat$EWU, na.rm=T)
fishdiv_HUC6_dat[fishdiv_HUC6_dat$Quasiext.huc6 >= 0.5 & fishdiv_HUC6_dat$EWU >= med_EWU,"Category"] <- "HighEndemism_HighExtProba"
fishdiv_HUC6_dat[fishdiv_HUC6_dat$Quasiext.huc6 >= 0.5 & fishdiv_HUC6_dat$EWU < med_EWU,"Category"] <- "LowEndemism_HighExtProba"
fishdiv_HUC6_dat[fishdiv_HUC6_dat$Quasiext.huc6<0.5 & fishdiv_HUC6_dat$EWU>=med_EWU & !is.na(fishdiv_HUC6_dat$EWU),"Category"] <- "HighEndemism_LowExtProba"
fishdiv_HUC6_dat[fishdiv_HUC6_dat$Quasiext.huc6< 0.5 & fishdiv_HUC6_dat$EWU < med_EWU & !is.na(fishdiv_HUC6_dat$EWU),"Category"] <- "LowEndemism_LowExtProba"
write.dbf(fishdiv_HUC6_dat, "fish/HUC6div_category.dbf")


my.col <- colorRampPalette(brewer.pal(9, "YlGn"))
my.col2 <- colorRampPalette(brewer.pal(9, "RdPu"))

int<- seq(min(fishdiv_HUC6_dat$EWU,na.rm=T),max(fishdiv_HUC6_dat$EWU,na.rm=T),diff(range(fishdiv_HUC6_dat$EWU, na.rm=T))/100)
fishdiv_HUC6_dat$cat <- cut(fishdiv_HUC6_dat$EWU, breaks=int, labels=my.col(150)[51:150])
fishdiv_HUC6_dat$cat2 <- cut(fishdiv_HUC6_dat$EWU, breaks=int, labels=my.col2(150)[51:150])

geom_point(aes(color=NDC_HUC), alpha=0.8) +
  geom_point(data=NDC_HUC6_dat[NDC_HUC6_dat$NDC_HUC > 10,],color='#d13d38', alpha=0.8) +
  # geom_point(data=NDC_HUC6_dat[NDC_HUC6_dat$NDC_HUC >= 1,],alpha=0.8, color='#D66460') + 
  # geom_point(data=NDC_HUC6_dat[NDC_HUC6_dat$NDC_HUC < 1,],alpha=0.8, color='#E8E8E8') + 
  geom_point(shape=1, alpha =1/10) +
  scale_color_gradient2(midpoint=1, low='#E8E8E8', high='#d13d38', space='Lab',limits = c(0, 10))

#Plot EWU categories
fishdiv_HUC6_dat2 <- fishdiv_HUC6_dat
med <- median(fishdiv_HUC6_dat2$EWU,na.rm=T)
HUC6_fishdiv_plot <-ggplot(fishdiv_HUC6_dat,aes(x=100*Quasiext.huc6, y=EWU, size = X2016)) + 
  geom_point(alpha=0.8, aes(color=EWU)) + 
  geom_point(data=fishdiv_HUC6_dat[fishdiv_HUC6_dat$EWU > 1,], alpha=0.8, color='#006d2c') + 
  # geom_point(data=fishdiv_HUC6_dat[fishdiv_HUC6_dat$EWU >= med,], alpha=0.8, color='#006d2c',shape=16) + 
  # geom_point(data=fishdiv_HUC6_dat[fishdiv_HUC6_dat$EWU < med,], alpha=0.8, color="#E8E8E8",shape=16) + 
  geom_point(shape=1, alpha =1/10) +
  scale_color_gradient2(low='#E8E8E8', high='#006d2c', space='Lab',limits = c(0, 1)) +
  geom_vline(xintercept= 50)+
  geom_hline(yintercept= med)+
  theme_classic() +
  labs(x="Streamgage network decline risk (%)", y="Fish diversity Endemism weighted richness/units (EWU)") +
  scale_x_continuous(limits=c(0,100)) + 
  scale_y_continuous(limits=c(0,3.7),expand=c(0,0), breaks=c(0,1,2,3)) +
  scale_size(range=c(0.5,7), name="Number of gages in 2016", breaks=c(10,50,100,174)) + 
  #guides(color = guide_legend(override.aes = list(size=5, shape=16))) + 
  theme(legend.justification = c(0, 1),
        legend.position=c(0.55,1.05),
        legend.margin = margin(-5,0,0,0),
        legend.title = element_text(face = "bold"),
        legend.background = element_blank(),
        text = element_text(size = 12),
        axis.text = element_text(size=14))
HUC6_fishdiv_plot

####################
# Export plot
####################
#Make sure thaT the top graph is aligned to the bottom graph's x-axis
gt_scarcity<- ggplot_gtable(ggplot_build(HUC6_scarcity_plot))
gt_fish <- ggplot_gtable(ggplot_build(HUC6_fishdiv_plot))
gt_flood <- ggplot_gtable(ggplot_build(HUC6_floodrisk_plot))
#gtable_show_layout(g)
#g$widths
maxWidth = unit.pmax(gt_scarcity$widths, gt_fish$widths,gt_flood$widths)
gt_scarcity$widths <- maxWidth
gt_fish$widths <- maxWidth
gt_flood$widths <- maxWidth

maxHeight = unit.pmax(gt_scarcity$heights, gt_fish$heights,gt_flood$heights)
gt_scarcity$heights <- maxHeight
gt_fish$heights <- maxHeight
gt_flood$heights <- maxHeight

pdf("Figures/Figure_3/Figure3_scatterplot_6_v7.pdf", width=5,height=12)
grid.arrange(gt_scarcity, gt_flood, gt_fish, ncol=1)
dev.off()