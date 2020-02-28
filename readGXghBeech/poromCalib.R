poromCalib <- read.csv("hydroBdata/poromCalib.csv")
poromCalib$Date <- lubridate::ymd(as.character(poromCalib$Date))
poromCalib$id <- paste0(poromCalib$Plant, "_", poromCalib$rep)
licorS <- subset(poromCalib, instrument=="LICOR 6400")[,c('id','stomata')]
handH <- subset(poromCalib, instrument!="LICOR 6400")[,c('id','stomata')]
corr <- merge(licorS, handH, by='id', all=T)
names(licorS)[2] <- 'licor'
licor <- subset(poromCalib, instrument=="LICOR 6400")[,c('Date','Plant','rep','stomata')]
colnames(licor)[ncol(licor)] <- "cond_6400"
one <- subset(poromCalib, instrument=="Porometer ECOFUN1")[,c('Date','Plant','rep','stomata')]
two <- subset(poromCalib, instrument=="Porometer ECOFUN2")[,c('Date','Soil','Treat','Plant','rep','stomata')]
colnames(one)[ncol(one)] <- "cond_one"
colnames(two)[ncol(two)] <- "cond_two"
poromCalib <- merge(merge(two, one, by=c('Date', 'Plant', 'rep'), all=T), licor, by=c('Date', 'Plant', 'rep'), all=T)
# discad one outliner with a disportionatly large effect
outlierPosition <- which(poromCalib$cond_one>=300 & poromCalib$cond_two<=150)
poromCalib <- poromCalib[c(1:(outlierPosition-1),(outlierPosition+1):nrow(poromCalib)),]
dates <- c(unique(poromCalib$Date))
poromCalibL <- list()
for (i in 1:length(dates)){
  poromCalibL[[i]] <- subset(poromCalib, Date==dates[i])
}

#hand-held porometer calib agaings LI-COR 6400
modelPorom1 <- lm(cond_one~cond_6400, data=poromCalibL[[2]])
modelPorom2 <- lm(cond_two~cond_6400, data=poromCalibL[[2]])
windows(10,10)
par(mfrow=c(1,1))
plot(poromCalibL[[2]][,'cond_one']~poromCalibL[[2]][,'cond_6400'], xlim=c(150, 300), ylim=c(225, 440),
     ylab='Cond Porom (mmol/m2/s)', xlab='Cond 6400 (mmol/m2/s)', pch=19, col='blue', cex=1.6)
points(poromCalibL[[2]][,'cond_two']~poromCalibL[[2]][,'cond_6400'], pch=19, col='forestgreen',
       cex=1.6)
abline(modelPorom1, col='blue')
abline(modelPorom2, col='forestgreen')
legend('topleft', legend=c('Porom 1', 'Porom 2'), pch=c(19,19), col=c('blue','forestgreen'), bty='n')
legend('bottomright', legend=c('Porom 1, Porom 2',expression(R^2~'= 0.61, 0.55'), 'Slope = 0.92, 1.26'), bty='n')

# day-specific cross-calibration
calibFits <- data.frame(row.names=1:length(poromCalibL))
fits <- list()
for(i in 1:length(poromCalibL)){
  fits[[i]] <- summary(lm(cond_one~cond_two, data=poromCalibL[[i]]))
}
for (i in 1:length(fits)){
  calibFits$Date[i] <- as.character(poromCalibL[[i]][1,'Date'])
  calibFits$r.sqr[i] <- fits[[i]][[8]]
  calibFits$int[i] <- fits[[i]][[4]][1]
  calibFits$se.int[i] <- fits[[i]][[4]][3]
  calibFits$p.int[i] <- fits[[i]][[4]][7]
  calibFits$slp[i] <- fits[[i]][[4]][2]
  calibFits$se.slp[i] <- fits[[i]][[4]][4]
  calibFits$p.slp[i] <- fits[[i]][[4]][8]
  calibFits$rep[i] <- nrow(poromCalibL[[i]])
}
calibFits$Date <- lubridate::ymd(calibFits$Date)
calibFits$intercept <- ifelse(calibFits$p.int>=0.05, 0, calibFits$int)
calibFits$slope <- ifelse(calibFits$p.slp>=0.05, 1, calibFits$slp)

# comparision of individual dates models with global model
myPal <- c('black','red','blue','forestgreen','magenta')
windows(10,10)
plot(poromCalibL[[1]][,'cond_one']~poromCalibL[[1]][,'cond_two'], pch=19, col=myPal[1], 
     ylab='gs Porom 1 (mmmol/m2/s)', xlab='gs Porom 2 (mmol/m2/s)', ylim=c(0,675), xlim=c(0,675))
for (i in 2:length(poromCalibL)){
  points(poromCalibL[[i]][,'cond_one']~poromCalibL[[i]][,'cond_two'], pch=19, col=myPal[i])
}
legend('topleft', legend=calibFits$Date, pch=19, col=myPal, bty='n')
abline(1,1, lty=2)
for(i in 1:length(poromCalibL)){
  abline(lm(cond_one~cond_two, data=poromCalibL[[i]]), col=myPal[i])
}
globalModel <- lm(cond_two~cond_one, data=poromCalib)
abline(globalModel, lwd=2)

porom <- read.csv('hydroBdata/conductancePorometers.csv')
porom$Date <- lubridate::ymd(as.character(porom$Date))
porom <- merge(porom, calibFits[,c('Date','intercept','slope')], by='Date', all.x=T, all.y=F)
porom$nday <- as.numeric(porom$Date - as.Date('2018-05-16'))
porom$slopeGlobal <- globalModel$coefficients[2]
porom$predIndvDay <- ifelse(porom$instrument=="Porometer ECOFUN2", porom$intercept + porom$stomata*porom$slope,
                            porom$stomata) 
porom$predGlobal <- ifelse(porom$instrument=="Porometer ECOFUN2", porom$stomata*porom$slopeGlobal, porom$stomata) 

# visualize predicted vs. observed for porometer 2
k <- subset(porom, instrument=="Porometer ECOFUN2")
windows(10,10)
plot(k$predIndvDay~k$stomata, ylim=c(0,590), xlim=c(0,590), pch=19,
     ylab='gs Porom 2 pred (mmol/m2/s)', xlab='gs Porom 2 obs (mmol/m2/s)')
points(k$predGlobal~k$stomata, pch=19, col='red')
legend('topleft', pch=19, col=c(1:2), legend=c('Day-fit','Global-fit'), bty='n')
abline(1,1, lty=2)

# assess instrument differences
summary(aov(stomata~Treat*Soil*nday, data=subset(porom, Treat!="BAG")))
source('hydroBread/basicFun.R')
poromSumm <- doBy::summaryBy(stomata ~ Treat + Soil + nday, FUN=c(mean.na, s.err), data=subset(porom, Treat!='BAG'))
write.csv(poromSumm, file='hydroBoutput/poromSumm.csv', row.names = F)

#no singificant differences between instruments: use uncorrected raw data from both instruments
rm(licor, one, two, outlierPosition, dates, poromCalibL, modelPorom1, modelPorom2, globalModel,
   k, myPal, calibFits, fits, i)
