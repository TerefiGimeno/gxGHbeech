# load required libraries
library(aqp)
library(soilDB)
library(latticeExtra)
library(plyr)
library(reshape2)
library(soilwater)

# you can get the data like this:
# s <- fetchKSSL(pedon_id = 'S08NV003003')

#The parameters are estimated based on soil texture
A.control.params <- data.frame(theta_r=0.0539, theta_s=0.4478, alpha=-3.321462, npar=0.29385)
A.dry.params <- data.frame(theta_r=0.0539, theta_s=0.4481, alpha=-3.321462, npar=0.29349)
B.dry.params <- data.frame(theta_r=0.0544, theta_s=0.4171, alpha=-3.414283, npar=0.32481)
C.control.params <- data.frame(theta_r=0.0699, theta_s=0.4926, alpha=-3.579129, npar=0.151)

clay.params <- data.frame(theta_r=0.0616, theta_s=0.3695, alpha=-3.641996, npar=0.14248)

A.control.model <- KSSL_VG_model(A.control.params)
C.control.model <- KSSL_VG_model(C.control.params)
B.dry.model <- KSSL_VG_model(B.dry.params)
clay.model <- KSSL_VG_model(clay.params)

A.control.plot <- xyplot(phi ~ theta, data=A.control.model$VG_curve, type=c('l', 'g'), scales=list(alternating=3, x=list(tick.number=10), y=list(log=10, tick.number=10)), yscale.components=yscale.components.logpower, ylab=expression("Suction " (kPa)), xlab=expression("Volumetric Water Content " (cm^3/cm^3)), par.settings = list(plot.line=list(col='RoyalBlue', lwd=2)))
clay.plot <- xyplot(phi ~ theta, data=clay.model$VG_curve, type=c('l', 'g'), scales=list(alternating=3, x=list(tick.number=10), y=list(log=10, tick.number=10)), yscale.components=yscale.components.logpower, ylab=expression("Suction " (kPa)), xlab=expression("Volumetric Water Content " (cm^3/cm^3)), par.settings = list(plot.line=list(col='RoyalBlue', lwd=2)))
B.dry.plot <- xyplot(phi ~ theta, data=B.dry.model$VG_curve, type=c('l', 'g'), scales=list(alternating=3, x=list(tick.number=10), y=list(log=10, tick.number=10)), yscale.components=yscale.components.logpower, ylab=expression("Suction " (kPa)), xlab=expression("Volumetric Water Content " (cm^3/cm^3)), par.settings = list(plot.line=list(col='RoyalBlue', lwd=2)))
C.control.plot <- xyplot(phi ~ theta, data=C.control.model$VG_curve, type=c('l', 'g'), scales=list(alternating=3, x=list(tick.number=10), y=list(log=10, tick.number=10)), yscale.components=yscale.components.logpower, ylab=expression("Suction " (kPa)), xlab=expression("Volumetric Water Content " (cm^3/cm^3)), par.settings = list(plot.line=list(col='RoyalBlue', lwd=2)))

update(A.control.plot, main='Estimated Water Retention Curve\nS08NV003003\n0-9cm', sub='van Genuchten Model Parameters fit by USDA-ARS Rosetta')
update(clay.plot, main='Estimated Water Retention Curve\nS08NV003003\n0-9cm', sub='van Genuchten Model Parameters fit by USDA-ARS Rosetta')
update(B.dry.plot, main='Estimated Water Retention Curve\nS08NV003003\n0-9cm', sub='van Genuchten Model Parameters fit by USDA-ARS Rosetta')
update(C.control.plot, main='Estimated Water Retention Curve\nS08NV003003\n0-9cm', sub='van Genuchten Model Parameters fit by USDA-ARS Rosetta')

grid.arrange(A.control.plot,B.dry.plot,C.control.plot,ncol=3)

##attribute bulk density (Teresa calculated them)
Soil2$BD[Soil2$Soil=='A'&Soil2$Treat=='CONTROL']<-1.308
Soil2$BD[Soil2$Soil=='B'&Soil2$Treat=='CONTROL']<-1.408
Soil2$BD[Soil2$Soil=='C'&Soil2$Treat=='CONTROL']<-1.208
Soil2$BD[Soil2$Soil=='A'&Soil2$Treat=='DRY']<-1.307
Soil2$BD[Soil2$Soil=='A'&Soil2$Treat=='BAG']<-1.307
Soil2$BD[Soil2$Soil=='B'&Soil2$Treat=='DRY']<-1.405
Soil2$BD[Soil2$Soil=='C'&Soil2$Treat=='DRY']<-1.171
  
Soil2$VWC<-Soil2$GWC*Soil2$BD

Soil2$phi[Soil2$Soil=='A'] <- A.control.model$VG_inverse_function(Soil2$VWC[Soil2$Soil=='A'])
Soil2$phi[Soil2$Soil=='B'] <- B.dry.model$VG_inverse_function(Soil2$VWC[Soil2$Soil=='B'])
Soil2$phi[Soil2$Soil=='C'] <- clay.model$VG_inverse_function(Soil2$VWC[Soil2$Soil=='C'])

Soil3<-Soil2

Soil3<-subset(Soil2,!phi<(-300000))
Soil3$phi[Soil3$phi>5000] <- 5000

##VWC short

VWCshort<-Soil2[,c('Plant','VWC','Soil','Date','GWC','Treat')]

###residual water
VWCshort$ext_vwc[VWCshort$Soil=='A'] <- 0.0559
VWCshort$ext_vwc[VWCshort$Soil=='B'] <- 0.05534
VWCshort$ext_vwc[VWCshort$Soil=='C'] <- 0.1395

###saturated VWC
VWCshort$sat_vwc[VWCshort$Soil=='A'] <- 0.4478
VWCshort$sat_vwc[VWCshort$Soil=='B'] <- 0.4171
VWCshort$sat_vwc[VWCshort$Soil=='C'] <- 0.4926

###VWC at permanent wilting point
VWCshort$wilt_vwc[VWCshort$Soil=='A'] <- 0.109
VWCshort$wilt_vwc[VWCshort$Soil=='B'] <- 0.102
VWCshort$wilt_vwc[VWCshort$Soil=='C'] <- 0.24

### VWC relative to VWC at permanent wilting point
VWCshort$fraction<-(VWCshort$VWC-VWCshort$wilt_vwc)
