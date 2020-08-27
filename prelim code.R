#KHAO DINSOR####
#within-season analysis

day=read.csv(file.choose(), h=T)
str(day)
day$Day.No.=as.integer(day$Day.No.)
library(lme4)
library(MASS)
library(rsq)
hist(day$Count)
plot(day$Count~day$Day.No.)
plot(day$Count~day$Site)
mod1=glm(day$Count~day$Day.No.+day$Site, family="poisson")
summary(mod1)
anova(mod1)
rsq(mod1)
#counts during the season, varied for each site

#weather correlates
daywe=read.csv(file.choose(),h=T)
daywe$Cloud.Cover=as.numeric(daywe$Cloud.Cover)
str(daywe)
mod1.5=glm(Count~u.wind+v.wind+Day+Temperature+daywe$CLOUD.COVER..+daywe$site, family="poisson")
rsq(mod1.5)
summary(mod1.5)
moddaywe=glmer(daywe$Count~z.day+u.wind+v.wind+z.temp+Cloud.Cover+(1|season/site), family="poisson", data=daywe)
coef(moddaywe)
summary(moddaywe) #0.18 variability in intercept between sites (daily count), varying intercept
#mean varying start ooints
library(MuMIn)
r.squaredGLMM(moddaywe)

exp(7.211128) #eagle cliff base count
exp(8.079039) #seaside base count
plot(moddaywe)

#visualize
library(ggplot2)
str(daywe)
pred1=predict(moddaywe)

daywe$random.int.pred=predict(moddaywe)
pred1<- predict(moddaywe, newdata =daywe$random.int.pred)
daywe$random.int.pred
model.with.data <- ggplot(data=daywe, aes(x=Day, y=pred1, group = season, colour = season)) +
  geom_line() + 
  geom_point(aes(x=Day, y=Count, group = site, colour = site)) +
  labs(x="Day", y="Predicted Count") +
  ggtitle("Varying Slope and Intercept Count Prediction") 
model.with.data
dim(daywe)


#mutate
na.omit(daywe)

gdp_pop_continent
#withn-day analysis
hour=read.csv(file.choose(), h=T)
str(hour)
hist(hour$average)
plot(hour$average~hour$hour)
plot(hour$average~hour$SITE)
mod2=glm(hour$average~hour$date+hour$hour*hour$SITE, family="poisson")
summary(mod2)
rsq(mod2)

#counts during the day, varied for each site