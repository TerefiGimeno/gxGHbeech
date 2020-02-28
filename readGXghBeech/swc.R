source('hydroBread/basicFun.R')
swc <- read.csv("hydroBdata/potWeights.csv")
swc$Date <- lubridate::ymd(as.character(swc$Date))
dryPots <- read.csv("hydroBdata/finalSoilWeight.csv")
dryPots$pot_plus_plate_weight <- ifelse(is.na(dryPots$pot_plus_plate_weight),
                                        dryPots$plate_weight + dryPots$pot_weight,
                                        dryPots$pot_plus_plate_weight)
dryPots$finalSWC <- (dryPots$Soil_wet_weight-dryPots$Soil_dry_weight)*100/
  (dryPots$Soil_dry_weight-dryPots$paper_bag_g)
dryPots$distance_soil_to_pot_border_cm <- ifelse(is.na(dryPots$distance_soil_to_pot_border_cm),
                                                 mean(dryPots$distance_soil_to_pot_border_cm, na.rm=T),
                                                 dryPots$distance_soil_to_pot_border_cm)
dryPots$soilVolume <- ((2-dryPots$distance_soil_to_pot_border_cm*0.1)/3)*(1.5+1.1+sqrt(1.5*1.1))
dryPots$dryWeight <- ifelse(is.na(dryPots$cryoSampleSoil_wetWeight_g),
                            dryPots$Soil_dry_weight-dryPots$paper_bag_g,
                            dryPots$Soil_dry_weight - dryPots$paper_bag_g + 
                              dryPots$cryoSampleSoil_wetWeight_g -
                              dryPots$cryoSampleSoil_wetWeight_g*0.01*dryPots$finalSWC)
dryPots$dryWeightCORR <- ifelse(is.na(dryPots$cryoSampleSoil_wetWeight_g), dryPots$Soil_dry_weight-dryPots$paper_bag_g,
                               dryPots$Soil_dry_weight - dryPots$paper_bag_g + 
                                 dryPots$cryoSampleSoil_wetWeight_g/(0.01*dryPots$finalSWC+1))
dryPots$bulkDen <- dryPots$dryWeight*0.001/dryPots$soilVolume
dryPots$bulkDenCORR <- dryPots$dryWeightCORR*0.001/dryPots$soilVolume
swc <- merge(swc, dryPots[,c('Plant','dryWeight','dryWeightCORR','plant_weight','pot_plus_plate_weight',
                             'bulkDen','bulkDenCORR')],
             by='Plant', all=T)
swc$GWC <- (swc$weight_g - swc$plant_weight - swc$pot_plus_plate_weight - swc$dryWeight)*100/swc$dryWeight
swc$VWC <- swc$GWC*swc$bulkDen
swc$GWCcorr <- (swc$weight_g - swc$plant_weight - swc$pot_plus_plate_weight - swc$dryWeightCORR)*100/swc$dryWeightCORR
swc$VWCcorr <- swc$GWCcorr*swc$bulkDenCORR
swc$nday <- as.numeric(swc$Date-as.Date("2018-05-16"))

#analysis of co-variance
modelGWC <- lm(log(GWC)~nday*Treat*Soil, data=swc)
summary(aov(log(GWC)~nday*Treat*Soil, data=swc))
modelVWC <- lm(log(VWC)~nday*Treat*Soil, data=swc)
summary(aov(log(VWC)~nday*Treat*Soil, data=swc))

# predic daily VWC per treatment
modelVWCDRY <- lm(log(VWC)~nday*Soil, data=subset(swc, Treat='DRY'))
predVWC <- data.frame(row.names=c(1:(3*max(swc$nday))))
predVWC$nday <- rep(c(1:max(swc$nday)),times=3)
predVWC$Soil <- c(rep('A', times=max(swc$nday)), rep('B', times=max(swc$nday)), rep('C', times=max(swc$nday)))
predVWC$predVWC <- predict.lm(modelVWCDRY, predVWC)

# individual-models for each plant to calculate ET normalized per unit of leaf area
dry <- subset(swc, Treat=="DRY")[,c('Plant','Soil','nday','GWC')]
plants <- c(unique(dry$Plant))
dryL <- list()
for (i in 1:length(plants)){
  dryL[[i]] <- subset(dry, Plant==plants[i])
}
dryIndvModelsL <- list()
for (i in 1:length(dryL)){
  dryIndvModelsL[[i]] <- lm(log(GWC)~nday, data=dryL[[i]])
}
toPredict <- c(min(dry$nday):max(dry$nday))
dryPred <- list()
for (i in 1:length(dryL)){
  dryPred[[i]] <- data.frame(row.names=(min(dry$nday)):(max(swc$nday)))
  dryPred[[i]]$nday <- toPredict
  dryPred[[i]]$Plant <- rep(dryL[[i]][1,'Plant'], times=(max(swc$nday)+1))
  dryPred[[i]]$predGWC <- exp(predict.lm(dryIndvModelsL[[i]], dryPred[[i]]))
  dryPred[[i]] <- doBy::orderBy(~nday, dryPred[[i]])
  dryPred[[i]]$difGWC <- (dryPred[[i]]$predGWC - dplyr::lag(dryPred[[i]]$predGWC))*-0.01
}

# calculate daily ET per pot
ET <- do.call(rbind, dryPred)
leafArea <- read.csv("hydroBdata/leafArea.csv")
ET <- merge(merge(ET, leafArea[,c('Plant','Soil','Treat','totalLeafArea_cm2')], by='Plant', all.x=T, all.y=F), 
            dryPots[,c('Plant','dryWeight')], by='Plant', all.x=T, all.y=F)
ET$ET_mol.m2.day <- ET$difGWC*ET$dryWeight/(18*ET$totalLeafArea_cm2/10000)

# calculate mean daily ET per soil type
ETsumm <- doBy::summaryBy(ET_mol.m2.day ~ Soil + nday, FUN=c(mean.na, s.err), data=ET)

# plot mean ET per soil type over time
myPal <- palette(c('deeppink','chocolate4','cornflowerblue'))
windows(12,8)
par(mfrow=c(1,1), las=1)
par(mar=c(6,6,2,2))
plot(subset(ETsumm, Soil=='A')[,'nday'], subset(ETsumm, Soil=='A')[,'ET_mol.m2.day.mean.na'],
     col=myPal[1], lwd=2, ylim=c(0,135), type='l', xlab='Time (days)',
     ylab=expression(italic(E)[t]~(mol~m^-2~day^-1)), cex.lab=1.5)
soils <- c('A','B','C')
for (i in 1:length(soils)){
  lines(subset(ETsumm, Soil==soils[i])[,'nday'], subset(ETsumm, Soil==soils[i])[,'ET_mol.m2.day.mean.na'],
  col=myPal[i], lwd=2)
  Hmisc::errbar(subset(ETsumm, Soil==soils[i])[,'nday'], subset(ETsumm, Soil==soils[i])[,'ET_mol.m2.day.mean.na'],
                subset(ETsumm, Soil==soils[i])[,'ET_mol.m2.day.mean.na'] 
                + subset(ETsumm, Soil==soils[i])[,'ET_mol.m2.day.s.err'],
                subset(ETsumm, Soil==soils[i])[,'ET_mol.m2.day.mean.na'] - 
                  subset(ETsumm, Soil==soils[i])[,'ET_mol.m2.day.s.err'],
                pch=1,  col=myPal[i],  cex=1.2, add=T)
  points(subset(ETsumm, Soil==soils[i])[,'nday'], subset(ETsumm, Soil==soils[i])[,'ET_mol.m2.day.mean.na'],
         pch=19, col=myPal[i], cex=1.2)
  points(subset(ETsumm, Soil==soils[i])[,'nday'], subset(ETsumm, Soil==soils[i])[,'ET_mol.m2.day.mean.na'],
         col='black', pch=21, cex=1.2) 
}
legend('topright', legend=c('Sand','Sand & Rock', 'Sandy Clay Loam'), lty=c(1,1,1), col=myPal, lwd=2, bty='n')

# Plot of VWC per pot and soil type
myPal <- c('red','blue','green','magenta','black','grey','yellow','forestgreen','navyblue','cyan')
plantsA <- unique((subset(swc, Soil=='A'))[,'Plant'])
plantsB <- unique((subset(swc, Soil=='B'))[,'Plant'])
plantsC <- unique((subset(swc, Soil=='C'))[,'Plant'])
dryLA <- list()
for (i in 1:length(plantsA)){
  dryLA[[i]] <- subset(dry, Plant==plantsA[i])
}
dryLB <- list()
for (i in 1:length(plantsB)){
  dryLB[[i]] <- subset(dry, Plant==plantsB[i])
}
dryLC <- list()
for (i in 1:length(plantsC)){
  dryLC[[i]] <- subset(dry, Plant==plantsC[i])
}
windows(12,8)
par(mfrow=c(1,1))
plot(dryLA[[1]][,'GWC']~dryLA[[1]][,'nday'], pch=19, col=myPal[1],
     ylim=c(0,50), ylab='GWC (%)', xlab='Time (days)')
for (i in 1:length(dryPred)){
  lines(dryPred[[i]][,'predGWC']~dryPred[[i]][,'nday'])
}
for(i in 2:length(dryLA)){
  points(dryLA[[i]][,'GWC']~dryLA[[i]][,'nday'], pch=19, col=myPal[i])
}
for(i in 1:length(dryLB)){
  points(dryLB[[i]][,'GWC']~dryLB[[i]][,'nday'], pch=15, col=myPal[i])
}
for(i in 1:length(dryLB)){
  points(dryLC[[i]][,'GWC']~dryLC[[i]][,'nday'], pch=17, col=myPal[i])
}


#one-way ANOVA of maximum and minimum water holding capacity
summary(aov(GWC~Soil, data=subset(swc, nday==0)))
TukeyHSD(aov(GWC~Soil, data=subset(swc, nday==0)))
summary(aov(VWC~Soil, data=subset(swc, nday==0)))
TukeyHSD(aov(VWC~Soil, data=subset(swc, nday==0)))
summary(aov(GWC~Soil, data=subset(swc, nday==35 & Treat=="DRY")))
TukeyHSD(aov(GWC~Soil, data=subset(swc, nday==35 & Treat=="DRY")))
summary(aov(VWC~Soil, data=subset(swc, nday==35 & Treat=="DRY")))
TukeyHSD(aov(VWC~Soil, data=subset(swc, nday==35 & Treat=="DRY")))
one <- doBy::summaryBy(GWC + VWC ~ Soil, data=subset(swc, nday==0), FUN=c(mean, s.err, length))
one$nday <- c(rep(0, times=3))
two <- doBy::summaryBy(GWC + VWC ~ Soil, data=subset(swc, nday==35 & Treat=="DRY"), FUN=c(mean, s.err, length))
two$nday <- c(rep(35, times=3))
write.csv(rbind(one, two), file="hydroBoutput/summarySWC.csv", row.names=F)

#this doesn't work and I don't know why
# modelGWC <- gnls(GWC ~ a*Soil*exp(-b*Treat*Soil*nday), data=swc, start=list(a=c(rep(50, times=3)), b=c(0.05, times=6)),
                 #param=list(a~Soil, b~Treat*Soil))
# modelVWC <- gnls(VWC ~ a*Soil*exp(-b*Treat*Soil*nday), data=swc, start=list(a=c(rep(50, times=3)), b=c(0.05, times=6)),
                 #param=list(a~Soil, b~Treat*Soil))
# summary(nls(VWC~a*exp(-b*nday), data=subset(swc, Soil=="A" & Treat=='drought'), start=list(a=50, b=0.05)))
# confint(nls(VWC~a*exp(-b*nday), data=subset(swc, Soil=="A" & Treat=='drought'), start=list(a=50, b=0.05)))
# summary(nls(VWC~a*exp(-b*nday), data=subset(swc, Soil=="B" & Treat=='drought'), start=list(a=50, b=0.05)))
# confint(nls(VWC~a*exp(-b*nday), data=subset(swc, Soil=="B" & Treat=='drought'), start=list(a=50, b=0.05)))
# summary(nls(VWC~a*exp(-b*nday), data=subset(swc, Soil=="C" & Treat=='drought'), start=list(a=50, b=0.05)))
# confint(nls(VWC~a*exp(-b*nday), data=subset(swc, Soil=="C" & Treat=='drought'), start=list(a=50, b=0.05)))

windows(12,8)
par(mfrow=c(2,1))
plot(log(swc$GWC)~swc$Date, col=as.factor(swc$Soil), pch=19)
plot(swc$VWC~swc$nday, col=as.factor(swc$Soil), pch=19, ylab="VWC (%)", xlab="Time (days)", cex.lab=1.5)

rm(dryPots, leafArea, one, two, dry, plants, dryL, dryIndvModelsL, plantsA, plantsB, plantsC,
   dryLA, dryLB, dryLC, toPredict)