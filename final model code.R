#KHAO DINSOR#
require(rstanarm)
require(dplyr)
##within-season analysis
#only use seaside counts

kdata=read.csv(file.choose(), h=T)
str(kdata)

plot(kdata$Count~kdata$Barometric.Pressure)

#standardize data
kdata$years = (kdata$Year - mean(kdata$Year))/(2 *sd(kdata$Year))
kdata$temp = (kdata$temperature - mean(kdata$temperature))/(2 *sd(kdata$temperature))
kdata$uwind = (kdata$u_wind - mean(kdata$u_wind))/(2 *sd(kdata$u_wind))
kdata$vwind = (kdata$v_wind - mean(kdata$v_wind))/(2 *sd(kdata$v_wind))
kdata$baro= (kdata$baro_pressure - mean(kdata$baro_pressure))/(2 *sd(kdata$baro_pressure))
kdata$mon=as.character(kdata$Month)
str(kdata)

#+ v wind is from the south
# + u wind is from the west

###subset for BB######
bb=subset(kdata, Species=="BB")
bb$logcount=log(bb$Count)
boxplot(bb$Count~bb$met_wind, ylab=" number of individuals", xlab="wind direction")

mod_bb=stan_glmer.nb(Count~years+temp+uwind+vwind+baro+(1|mon),
                   data=bb)
summary(mod_bb)
length(which(bbpost$temp>0))/length(bbpost$temp)

#subset for CS#####
cs=subset(kdata, Species=="CS")
plot(cs$Count~cs$met_wind)
hist(bb$Count)

mod_cs=stan_glmer.nb(Count~years+temp+uwind+vwind+baro+(1|mon),
                     data=cs)
summary(mod_cs)

#subset for OHB####

ohb=subset(kdata, Species=="OHB")
plot(bb$Count~bb$barometric.pressure)
hist(bb$Count)

mod_ohb=stan_glmer.nb(Count~years+temp+uwind+vwind+baro+(1|mon),
                     data=ohb)
summary(mod_ohb)
bbpost=as.data.frame(mod_bb)


#plots
jpeg("wind2.jpeg",width = 7, height = 6, units = 'in', res = 300)
plot(x, y) # Make plot
dev.off()
mcmc_areas(ohbp, pars=c("temp","uwind", "vwind", "baro",
                                   "years"), prob=.80, prob_outer=0.95,
                 point_est = "median")+
  ggplot2::scale_y_discrete(labels = c("temperature", "U wind", "V wind", "barometric pressure", "years"))+vline_0(size = 0.25, color = "darkgray", linetype = 2)+
  ggtitle ("Oriental Honey-buzzard")+ 
  theme(plot.title=element_text(hjust=0,size=15),panel.border=element_blank(),
        axis.line = element_line(colour = "black"),axis.title=element_text(size=12, color="black"),axis.text=element_text(size=12, hjust=0, color="black"),axis.text.x=element_text(hjust=0.5, color="black"))+xlim(-6.5,5)
bbplot

ohbp=as.matrix(mod_ohb)

ggplot(kdata, aes(x = met_wind, y = Count)) +
  geom_boxplot(notch = FALSE, aes(ymin = y0, lower = y25, middle = y50, upper = y75, ymax = y100)) +
  facet_grid( Species ~ . ) +
  labs(x = 'wind direction', y = 'number of individuals') +
  theme_bw() +scale_y_log10()+
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )
graph

