#KHAO DINSOR####
require(rstanarm)
require(dplyr)
##within-season analysis
#only use seaside counts

#raw data####
kdata=read.csv("https://raw.githubusercontent.com/patdumandan/Khao-Dinsor-migration/master/BB-OHB-CS_daily.csv")
kdata=kdata%>%select(-X)%>%
  mutate(bph=Count/obs_effort)

##add julian date####
kdata$date <- as.Date(with(kdata, paste(Year, Month, Day,sep="-")), "%Y-%m-%d")

kdata$julian<- format(kdata$date, "%j")

#add meteorological wd vector####
kdata=kdata%>%
  mutate(metwind= case_when(met_wind=="N"~ 0,  
                            met_wind=="ESE"~ 112.5,
                            met_wind=="NNE"~ 22.5,
                            met_wind=="NE"~ 45,
                            met_wind=="ENE"~ 67.5,
                            met_wind=="E"~ 90,
                            met_wind=="ESE"~ 112.5,
                            met_wind=="SE"~135,
                            met_wind=="SSE"~157.5,
                            met_wind=="S"~180,
                            met_wind=="SSW"~ 202.5,
                            met_wind=="SW"~225,
                            met_wind=="WSW"~247.5,
                            met_wind=="W"~270,
                            met_wind=="WNW"~292.5,
                            met_wind=="NW"~315,
                            met_wind=="NNW"~337.5))

#u and v wind components####
kdata$uwind=(-(kdata$wind_speed)* sin ((3.14/180)*kdata$metwind))
kdata$vwind=(-(kdata$wind_speed)* cos ((3.14/180)*kdata$metwind))

#standardize data####
kdata$yearsd = (kdata$Year - mean(kdata$Year))/(2 *sd(kdata$Year))
kdata$tempsd = (kdata$temperature - mean(kdata$temperature))/(2 *sd(kdata$temperature))
kdata$uwindsd = (kdata$uwind - mean(kdata$uwind))/(2 *sd(kdata$uwind))
kdata$vwindsd = (kdata$vwind - mean(kdata$vwind))/(2 *sd(kdata$vwind))
kdata$barosd= (kdata$baro_pressure - mean(kdata$baro_pressure))/(2 *sd(kdata$baro_pressure))

#Notes####
#+ v wind is from the south
# + u wind is from the west

#add lag terms####
kdata=kdata%>%mutate(lag_temp=lag(tempsd, order_by=date),
                     lag_baro=lag(barosd, order_by = date),
                     lag_uwind=lag(uwindsd, order_by = date),
                     lag_vwind=lag(vwindsd, order_by=date),
                     lag_count=lag(Count, order_by = date)
)

#MODELS####
bb=kdata%>%filter(species=="Black Baza")
cs=kdata%>%filter(species=="Chinese Sparrowhawk")
ohb=kdata%>%filter(species=="Oriental Honey-buzzard")

mod_bb_lag=stan_glm(Count~yearsd+
                      lag_temp+lag_uwind+lag_vwind+lag_baro,
                    data=bb, family = neg_binomial_2, offset=obs_effort)
mod_bb=stan_glm(Count~yearsd+tempsd+uwindsd+vwindsd+barosd,
                data=bb, family = neg_binomial_2, offset=obs_effort)

mod_cs_lag=stan_glm(Count~yearsd+
                      lag_temp+lag_uwind+lag_vwind+lag_baro,
                    data=cs, family = neg_binomial_2, offset=obs_effort)
mod_cs=stan_glm(Count~yearsd+tempsd+uwindsd+vwindsd+barosd,
                data=cs, family = neg_binomial_2, offset=obs_effort)

mod_ohb_lag=stan_glm(Count~yearsd+
                       lag_temp+lag_uwind+lag_vwind+lag_baro,
                     data=ohb, family = neg_binomial_2, offset=obs_effort)
mod_ohb=stan_glm(Count~yearsd+tempsd+uwindsd+vwindsd+barosd,
                 data=ohb, family = neg_binomial_2, offset=obs_effort)

#plot model output:
bbplot=as.matrix(mod_bb)
bbp=mcmc_areas(bbplot, pars=c("tempsd","uwindsd", "vwindsd", "barosd",
                          "yearsd"), prob=.80, prob_outer=0.95,
           point_est = "median")+
  ggplot2::scale_y_discrete(labels = c("temperature", "U wind", "V wind", "barometric pressure", "year"))+vline_0(size = 0.25, color = "darkgray", linetype = 2)+
  ggtitle ("Black Baza")+ 
  theme(plot.title=element_text(hjust=0,size=15),panel.border=element_blank(),
        axis.line = element_line(colour = "black"),axis.title=element_text(size=12, 
      color="black"),axis.text=element_text(size=12, hjust=0, color="black"),
      axis.text.x=element_text(hjust=0.5, color="black"))+xlim(-12,5)
bbp

#plot model output:
csplot=as.matrix(mod_cs)
csp=mcmc_areas(csplot, pars=c("tempsd","uwindsd", "vwindsd", "barosd",
                          "yearsd"), prob=.80, prob_outer=0.95,
           point_est = "median")+
  ggplot2::scale_y_discrete(labels = c("temperature", "U wind", "V wind", "barometric pressure", "year"))+vline_0(size = 0.25, color = "darkgray", linetype = 2)+
  ggtitle ("Chinese Sparrowhawk")+ 
  theme(plot.title=element_text(hjust=0,size=15),panel.border=element_blank(),
        axis.line = element_line(colour = "black"),axis.title=element_text(size=12, 
      color="black"),axis.text=element_text(size=12, hjust=0, color="black"),
      axis.text.x=element_text(hjust=0.5, color="black"))+xlim(-12,5)
csp

ohbplot=as.matrix(mod_ohb)
ohbp=mcmc_areas(ohbplot, pars=c("yearsd","tempsd","uwindsd", "vwindsd", "barosd"
), prob=.80, prob_outer=0.95,
point_est = "median")+ggplot2::scale_y_discrete(labels = c("year", "temperature", "U wind", "V wind", "barometric pressure"))+vline_0(size = 0.25, color = "darkgray", linetype = 2)+
  ggtitle ("Oriental Honey-buzzard")+ 
  theme(plot.title=element_text(hjust=0,size=15),panel.border=element_blank(),
        axis.line = element_line(colour = "black"),axis.title=element_text(size=12, color="black"),axis.text=element_text(size=12, hjust=0, color="black"),
        axis.text.x=element_text(hjust=0.5, color="black"))+xlim(-12,5)
ohbp

plot_grid(bbp,
          csp+theme(axis.text.y = element_blank(),
                   axis.ticks.y = element_blank(),
                   axis.title.y = element_blank()),
          ohbp+theme(axis.text.y = element_blank(),
                     axis.ticks.y = element_blank(),
                     axis.title.y = element_blank()), 
                     nrow=1, labels="auto", align="v")

#P(positive/negative effects)####

bbpos=as.data.frame(mod_bb)
length(which(bbpos$uwindsd<0))/length(bbpos$uwindsd)
length(which(bbpos$vwindsd<0))/length(bbpos$vwindsd)
ength(which(bbpos$tempsd<0))/length(bbpos$tempsd)
length(which(bbpos$barosd<0))/length(bbpos$barosd)

cspos=as.data.frame(mod_cs)
length(which(cspos$uwindsd>0))/length(cspos$uwindsd)
length(which(cspos$vwindsd>0))/length(cspos$vwindsd)
length(which(cspos$tempsd>0))/length(cspos$tempsd)
length(which(cspos$barosd<0))/length(cspos$barosd)

ohbpos=as.data.frame(mod_ohb)
length(which(ohbpos$uwindsd>0))/length(ohbpos$uwindsd)
length(which(ohbpos$vwindsd>0))/length(ohbpos$vwindsd)
length(which(ohbpos$tempsd>0))/length(ohbpos$tempsd)
length(which(ohbpos$barosd<0))/length(ohbpos$barosd)

#seasonal timing plots####


bb_1=plot(bb$Count~bb$julian, ylab="number of individuals", xlab="Julian date", pch=19, main="Black Baza")
cs_1=plot(cs$Count~cs$julian, ylab="number of individuals", xlab="Julian date", pch=19, main="Chinese Sparrowhawk")
ohb_1=plot(ohb$Count~ohb$julian, ylab="number of individuals", xlab="Julian date", pch=19, main="Oriental Honey-buzzard")

#weather plots####
grid.arrange(temp_plot, bp_plot)
plot(kdata$temperature~kdata$julian, 
               ylab="temperature(Celsius)", xlab="Julian date", 
               main="temperature", pch=19)

plot(kdata$baro_pressure~kdata$julian, ylab="barometric pressure (hPa)", xlab="Julian date", main="barometric pressure", pch=19)
plot(kdata$u_wind~kdata$julian, ylab="U wind (W-E)", xlab="Julian date", main="U wind", pch=19)
plot(kdata$v_wind~kdata$julian, ylab="V wind (N-S)", xlab="Julian date", main="V wind", pch=19)
