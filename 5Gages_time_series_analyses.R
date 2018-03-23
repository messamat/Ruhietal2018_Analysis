### Tracking the pulse of the Earth's fresh waters
## Ruhi, Messager & Olden 2018 (Nature Sustainability)
## Code for time series analyses on gage dynamics

# Load packages
library(MARSS)
library(ggplot2)
library(gridExtra)

# Read in data
allHUC6<-as.matrix(read.table("HUC6_gages.csv", sep=";", row.names=1, header=TRUE))
allHUC6<-allHUC6[,c(86:155)] # select 1947-2016 period
allHUC6<-log(allHUC6+1) # log-transform and z-score data
the.mean = apply(allHUC6,1,mean,na.rm=TRUE)
the.sigma = sqrt(apply(allHUC6,1,var,na.rm=TRUE))
allHUC6 = (allHUC6-the.mean)*(1/the.sigma)
rownames_HUC6<-rownames(allHUC6)
head(allHUC6) 

# Specify conditions, fit AR models
pd = 0.5 # We will compute risk of hitting a 50 percent decline threshold
xd = -log(pd)
nyears=6 # Project 6 years into the future, i.e. 2017-2022 (end of the USGS strategic plan 2012-2022)
tyrs = 1:nyears  # Range of the simulated interval
Risk.huc6<-c()
u.huc6<-c()
Q.huc6<-c()
# 'for' loop, we will run one AR model per HUC6
for(huc in 1:dim(allHUC6)[1]){
  HUC6<-as.matrix(t(allHUC6[huc,]))
  modelfit<-MARSS(y=HUC6, model=list(R="zero"))
  u<-modelfit$coef[1]
  Q<-modelfit$coef[2]
  Pi<-c()
  p.ever = ifelse(u<=0,1,exp(-2*u*xd/Q)) #Q=sigma2
  for (i in 1:nyears){
    Pi[i] = p.ever * pnorm((-xd+abs(u)*tyrs[i])/sqrt(Q*tyrs[i]))+
      exp(2*xd*abs(u)/Q)*pnorm((-xd-abs(u)*tyrs[i])/sqrt(Q*tyrs[i]))
  }
  Risk.huc6[huc]<-Pi[nyears]
  u.huc6[huc]<-u # trend
  Q.huc6[huc]<-Q # process error variance
}
Decline.risks<-as.data.frame(cbind(rownames_HUC6,Risk.huc6, u.huc6, Q.huc6)) # compile resuts
write.csv(Decline.risks, "Declinerisks.csv") # export

# Plot parameter distributions
names(Decline.risks)<-c("rownames_HUC6","Risk","u","Q")
head(Decline.risks)

Uplot<-ggplot(Decline.risks, aes(as.numeric(as.character(u))), fill="gray")+
  geom_density(fill="gray")+
  geom_vline(xintercept=0, color="red", linetype="dotted")+
  scale_x_continuous(name = "Estimated U parameter")+
  scale_y_continuous(name = "Density")+
  ggtitle("(A) Long-term trend")+
  theme_bw()

Qplot<-ggplot(Decline.risks, aes(as.numeric(as.character(Q))), fill="gray")+
  geom_density(fill="gray")+
  geom_vline(xintercept=0, color="red", linetype="dotted")+
  scale_x_continuous(name = "Estimated Q parameter")+
  scale_y_continuous(name = "Density")+
  ggtitle("(B) Process error variance")+
  theme_bw()

Riskplot<-ggplot(Decline.risks, aes(as.numeric(as.character(Risk))), fill="gray")+
  geom_density(fill="gray")+
  geom_vline(xintercept=0, color="red", linetype="dotted")+
  scale_x_continuous(name = "Decline risk estimate")+
  scale_y_continuous(name = "Density")+
  ggtitle("(C) Resulting near-term risk")+
  theme_bw()
grid.arrange(Uplot,Qplot,Riskplot)
## End