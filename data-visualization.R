#Code for manuscript: Among-species differences in seasonal timing and weather correlates of 
#autumn raptor migration at Khao Dinsor, Thailand, 2015-2016####
require(bayesplot)
require(ggplot2)
require(cowplot)

#Fig. 1. seasonal timing plots####
bbplot=ggplot(bb, aes(y=Count, x=julian))+
  geom_col()+
  ylab("Number of Individuals")+ xlab("Julian Date")+ggtitle("Black Baza")+
  scale_x_continuous(limits=c(244.0, 320.0))+
  theme(plot.title=element_text(hjust=0,size=15),panel.border=element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),axis.title=element_text(size=12, color="black"),axis.text=element_text(size=12, hjust=0, color="black"),
        axis.text.x=element_text(hjust=0.5, color="black"),
        axis.title.x = element_text(face="bold"),
        axis.title.y = element_text(face="bold"),
        text= element_text(family="serif"))

bbplot  

csplot=ggplot(cs, aes(y=Count, x=julian))+
  geom_col()+
  ylab("Number of Individuals")+ xlab("Julian Date")+ggtitle("Chinese Sparrowhawk")+
  scale_x_continuous(limits=c(244.0, 320.0))+
  theme(plot.title=element_text(hjust=0,size=15),panel.border=element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),axis.title=element_text(size=12, color="black"),axis.text=element_text(size=12, hjust=0, color="black"),
        axis.text.x=element_text(hjust=0.5, color="black"),
        axis.title.x = element_text(face="bold"),
        axis.title.y = element_text(face="bold"),
        text= element_text(family="serif"))

csplot  

ohbplot=ggplot(ohb, aes(y=Count, x=julian))+
  geom_col()+
  ylab("Number of Individuals")+ xlab("Julian Date")+ggtitle("Oriental Honey-buzzard")+
  scale_x_continuous(limits=c(244.0, 320.0))+
  theme(plot.title=element_text(hjust=0,size=15),panel.border=element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),axis.title=element_text(size=12, color="black"),axis.text=element_text(size=12, hjust=0, color="black"),
        axis.text.x=element_text(hjust=0.5, color="black"),
        axis.title.x = element_text(face="bold"),
        axis.title.y = element_text(face="bold"),
        text= element_text(family="serif"))

ohbplot  

cowplot:::plot_grid(bbplot,
                    csplot+theme(
                      axis.title.y = element_blank()),
                    ohbplot+theme(
                      axis.title.y = element_blank()), 
                    nrow=1, labels="auto", align="v")

#Fig. 2. weather correlates plots####
tempplot=ggplot(kdata, aes(y=temperature, x=julian))+
  geom_point()+
  ylab("Temperature(\u00B0C)")+ xlab("Julian Date")+ggtitle("Temperature")+
  scale_x_continuous(limits=c(244.0, 320.0))+
  theme(plot.title=element_text(hjust=0,size=15),panel.border=element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),axis.title=element_text(size=12, color="black"),axis.text=element_text(size=12, hjust=0, color="black"),
        axis.text.x=element_text(hjust=0.5, color="black"),
        axis.title.x = element_text(face="bold"),
        axis.title.y = element_text(face="bold"),
        text= element_text(family="serif"))

tempplot

baroplot=ggplot(kdata, aes(y=baro_pressure, x=julian))+
  geom_point()+
  ylab("Barometric Pressure (hPa)")+ xlab("Julian Date")+ggtitle("Barometric Pressure")+
  scale_x_continuous(limits=c(244.0, 320.0))+
  theme(plot.title=element_text(hjust=0,size=15),panel.border=element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),axis.title=element_text(size=12, color="black"),axis.text=element_text(size=12, hjust=0, color="black"),
        axis.text.x=element_text(hjust=0.5, color="black"),
        axis.title.x = element_text(face="bold"),
        axis.title.y = element_text(face="bold"),
        text= element_text(family="serif"))

baroplot

windplot=ggplot(kdata, aes(y=metwind, x=julian))+
  geom_point()+
  ylab("Meteorological Wind (\u00B0)")+ xlab("Julian Date")+ggtitle("Meteorological Wind")+
  scale_x_continuous(limits=c(244.0, 320.0))+
  theme(plot.title=element_text(hjust=0,size=15),panel.border=element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),axis.title=element_text(size=12, color="black"),axis.text=element_text(size=12, hjust=0, color="black"),
        axis.text.x=element_text(hjust=0.5, color="black"),
        axis.title.x = element_text(face="bold"),
        axis.title.y = element_text(face="bold"),
        text= element_text(family="serif"))


cowplot:::plot_grid(tempplot,
                    baroplot,
                    windplot, 
                    nrow=1, labels="auto", align="hv")

#plot model output:
color_scheme_set("gray")
bbplot=as.matrix(mod_bb)
bbp=mcmc_areas(bbplot, pars=c("tempsd","uwindsd", "vwindsd", "barosd",
                              "yearsd"), prob=.80, prob_outer=0.95,
               point_est = "median")+
  ggplot2::scale_y_discrete(labels = c("Temperature", "U Wind", "V Wind", "Barometric Pressure", "Year"))+vline_0(size = 0.25, color = "darkgray", linetype = 2)+
  ggtitle ("Black Baza")+ xlab("Coefficient Estimate")+
  theme(plot.title=element_text(hjust=0,size=15),panel.border=element_blank(),
        axis.line = element_line(colour = "black"),axis.text=element_text(color="black", family="serif"),
        axis.text.x=element_text(hjust=0.5, color="black"),
        axis.title.x=element_text(hjust=0.5, color="black", face="bold"))+
  xlim(-12,5)
bbp

#Fig. 3: plot model outpu####
csplot=as.matrix(mod_cs)
csp=mcmc_areas(csplot, pars=c("tempsd","uwindsd", "vwindsd", "barosd",
                              "yearsd"), prob=.80, prob_outer=0.95,
               point_est = "median")+
  ggplot2::scale_y_discrete(labels = c("Temperature", "U Wind", "V Wind", "Barometric Pressure", "Year"))+vline_0(size = 0.25, color = "darkgray", linetype = 2)+
  ggtitle ("Chinese Sparrowhawk")+ xlab("Coefficient Estimate")+
  theme(plot.title=element_text(hjust=0,size=15),panel.border=element_blank(),
        axis.line = element_line(colour = "black"),axis.text=element_text(color="black", family="serif"),
        axis.text.x=element_text(hjust=0.5, color="black"),
        axis.title.x=element_text(hjust=0.5, color="black", face="bold"))+
  xlim(-12,5)
csp

ohbplot=as.matrix(mod_ohb)
ohbp=mcmc_areas(ohbplot, pars=c("tempsd","uwindsd", "vwindsd", "barosd","yearsd"
), prob=.80, prob_outer=0.95,
point_est = "median")+ggplot2::scale_y_discrete(labels = c("Temperature", "U Wind", "V wind", "Barometric Pressure", "Year"))+vline_0(size = 0.25, color = "darkgray", linetype = 2)+
  ggtitle ("Oriental Honey-buzzard")+ 
  xlab("Coefficient Estimate")+
  theme(plot.title=element_text(hjust=0,size=15),panel.border=element_blank(),
        axis.line = element_line(colour = "black"),axis.text=element_text(color="black", family="serif"),
        axis.text.x=element_text(hjust=0.5, color="black"),
        axis.title.x=element_text(hjust=0.5, color="black", face="bold"))+
  xlim(-12,5)
ohbp

cowplot:::plot_grid(bbp,
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
