#Code for manuscript: Among-species differences in seasonal timing and weather correlates of 
#autumn raptor migration at Khao Dinsor, Thailand, 2015-2016####

#package####
require(rstanarm)
require(dplyr)

##raw data####
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

#calculate u and v wind components####
kdata$uwind=(-(kdata$wind_speed)* sin ((3.14/180)*kdata$metwind))
kdata$vwind=(-(kdata$wind_speed)* cos ((3.14/180)*kdata$metwind))

#Notes####
#+ v wind is from the south
# + u wind is from the west

#standardize data####
kdata$yearsd = (kdata$Year - mean(kdata$Year))/(2 *sd(kdata$Year))
kdata$tempsd = (kdata$temperature - mean(kdata$temperature))/(2 *sd(kdata$temperature))
kdata$uwindsd = (kdata$uwind - mean(kdata$uwind))/(2 *sd(kdata$uwind))
kdata$vwindsd = (kdata$vwind - mean(kdata$vwind))/(2 *sd(kdata$vwind))
kdata$barosd= (kdata$baro_pressure - mean(kdata$baro_pressure))/(2 *sd(kdata$baro_pressure))

#add lag terms####
kdata=kdata%>%mutate(lag_temp=lag(tempsd, order_by=date),
                     lag_baro=lag(barosd, order_by = date),
                     lag_uwind=lag(uwindsd, order_by = date),
                     lag_vwind=lag(vwindsd, order_by=date),
                     lag_count=lag(Count, order_by = date)
)