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
kdata$uwind = (kdata$u.wind - mean(kdata$u.wind))/(2 *sd(kdata$u.wind))
kdata$vwind = (kdata$v.wind - mean(kdata$v.wind))/(2 *sd(kdata$v.wind))
kdata$baro= (kdata$barometric.pressure - mean(kdata$barometric.pressure))/(2 *sd(kdata$barometric.pressure))
kdata$mon=as.character(kdata$Month)
str(kdata)

#+ v wind is from the south
# + u wind is from the west

###subset for BB######
bb=subset(kdata, Species=="BB")
plot(bb$Count~bb$barometric.pressure)
hist(bb$Count)

mod_bb=stan_glmer.nb(Count~years+temp+uwind+vwind+baro+(1|mon),
                   data=bb)
summary(mod_bb)
length(which(bbpost$temp>0))/length(bbpost$temp)

#subset for CS#####
cs=subset(kdata, Species=="CS")
plot(bb$Count~bb$barometric.pressure)
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
