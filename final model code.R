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
str(kdata)

###subset for BB######
bb=subset(kdata, Species=="BB")
plot(bb$Count~bb$barometric.pressure)
hist(bb$Count)

mod_bb=stan_glm.nb(Count~years+temp+uwind+vwind+baro,
                   data=bb)
summary(mod_bb)