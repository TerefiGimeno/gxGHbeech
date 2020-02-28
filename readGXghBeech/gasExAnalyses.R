source('hydroBread/readLibsHydro.R')
gasExHB <- read.csv('hydroBdata/summaryGasExHydroBeechGH.csv')
gammaStar <- 42.75#at 25C according to Bernacchi et al. (2001)
gasExHB$nday <- as.numeric(ymd(gasExHB$Date) - as.Date('2018-5-16'))
source('hydroBread/Vcmax1pt.R')
source('hydroBread/swc.R')
#estimate Vcmax normalized at 25 C with the one-point method (DeKawe et al. 2016 New Phytol)
gasExHB$Vcmax <- Vcmax1pt(Photo=gasExHB$Photo, Ci=gasExHB$Ci, Tleaf=gasExHB$Tleaf)
gasExHB$Vcmax <- ifelse(gasExHB$Photo<0, NA, gasExHB$Vcmax)
gasExHB$Vcmax <- ifelse(gasExHB$Vcmax<0 | gasExHB$Vcmax>100, NA, gasExHB$Vcmax)
swcSumm <- doBy::summaryBy(VWC + GWC ~ Soil + Treat + nday, data=subset(swc, nday==35 | nday==22), FUN=c(mean.na, s.err))
gasExHB <- merge(gasExHB, swcSumm, by=c('Soil', 'Treat', 'nday'), all.x=T, all.y=F)
gasExHB$gc <- gasExHB$Cond/1.6
gasExHB$Dmmol <- gasExHB$VpdL*1000/gasExHB$Press
gasExHB$MESindex <- sqrt(gasExHB$Photo/(1.6*gasExHB$Dmmol*(gasExHB$CO2S-gammaStar)))
gasExHB$CAPindex <- gasExHB$Photo/(sqrt(gasExHB$Dmmol)*(gasExHB$CO2S-gammaStar))
forPred <- subset(gasExHB, notes=='low light' & Treat!='BAG' & PARi>=80)
forPred$Treat2 <- forPred$Treat
forPred[which(forPred$Date<=20180530 & forPred$Treat=='DRY'),'Treat2'] <- 'CONTROL'
gasExHB <- subset(gasExHB, experiment == 'vpdCur')
gasExHB <- subset(gasExHB, Soil!='B')
write.csv(doBy::summaryBy(Photo + Cond ~ Soil + Treat, data=subset(gasExHB, isMax=='yes'), FUN=c(mean.na, s.err)),
          file='hydroBoutput/summaryPhotoCond.csv', row.names = F)
write.csv(doBy::summaryBy(WP + VpdL ~ Soil + Treat + round, data=gasExHB, FUN=c(mean.na, s.err)),
          file='hydroBoutput/summaryWPvpd.csv', row.names = F)


summary(aov(Photo~Soil*Treat, data=subset(gasExHB, isMax=='yes')))
summary(aov(Cond~Soil*Treat, data=subset(gasExHB, isMax=='yes')))
summary(lm(log(Vcmax)~WP*Treat, data=gasExHB))



