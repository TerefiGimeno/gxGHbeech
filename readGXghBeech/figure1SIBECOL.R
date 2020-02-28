source("hydroBread/calcE.R")
source("hydroBread/wpGasEx.R")
swcSumm <- doBy::summaryBy(VWC + GWC ~ Soil + Treat + nday, data=swc, FUN=c(mean.na, s.err, length),
                           keep.names = T)
myPal <- palette(c('deeppink','cornflowerblue'))

windows(12,8)
par(mfrow=c(2,1), las=1)
par(mar=c(0,5,5,2))

cont <- list()
cont[[1]] <- subset(swcSumm, Treat=='CONTROL' & Soil=='A')
cont[[2]] <- subset(swcSumm, Treat=='CONTROL' & Soil=='C')

dro <- list()
dro[[1]] <- subset(swcSumm, Treat=='DRY' & Soil=='A')
dro[[2]] <- subset(swcSumm, Treat=='DRY' & Soil=='C')

plot(cont[[1]][,'nday'], cont[[1]][,'VWC.mean.na'], lty=2, col=myPal[1], lwd=2, ylim=c(0,60),
     ylab='VWC (%)', xlab=' ',cex.lab=2, type='l', xlim=c(0,35), axes=F)
axis(2, at=seq(0, 60, 10), labels = seq(0, 60, 10), las=1)
box()

for (i in 1:length(cont)){
  lines(cont[[i]][,'nday'], cont[[i]][,'VWC.mean.na'], col=myPal[i], lwd=2, lty=2)
  Hmisc::errbar(cont[[i]][,'nday'], cont[[i]][,'VWC.mean.na'], cont[[i]][,'VWC.mean.na'] + cont[[i]][,'VWC.s.err'],
                cont[[i]][,'VWC.mean.na'] - cont[[i]][,'VWC.s.err'], pch=1,  col=myPal[i], add=T, cex=1.5)
  points(cont[[i]][,'nday'], cont[[i]][,'VWC.mean.na'], pch=19, col='white', cex=1.5)
  points(cont[[i]][,'nday'], cont[[i]][,'VWC.mean.na'], pch=21, col='black', cex=1.5)
}
for (i in 1:length(cont)){
  lines(dro[[i]][,'nday'], dro[[i]][,'VWC.mean.na'], col=myPal[i], lwd=2, lty=1)
  Hmisc::errbar(dro[[i]][,'nday'], dro[[i]][,'VWC.mean.na'], dro[[i]][,'VWC.mean.na'] + dro[[i]][,'VWC.s.err'],
                dro[[i]][,'VWC.mean.na'] - dro[[i]][,'VWC.s.err'], pch=19,  col=myPal[i],  cex=1.5, add=T)
  points(dro[[i]][,'nday'], dro[[i]][,'VWC.mean.na'], col=myPal[i], pch=19, cex=1.5)
  points(dro[[i]][,'nday'], dro[[i]][,'VWC.mean.na'], col='black', pch=21, cex=1.5)
}

par(mar=c(5,5,0,2))
cont <- list()
cont[[1]] <- subset(wpo, Treat=='CONTROL' & Soil=='A')
cont[[2]] <- subset(wpo, Treat=='CONTROL' & Soil=='C')

dro <- list()
dro[[1]] <- subset(wpo, Treat=='DRY' & Soil=='A')
dro[[2]] <- subset(wpo, Treat=='DRY' & Soil=='C')

ml50 <- list()
ml50[[1]] <- subset(wpo, Treat=='50mL' & Soil=='A')
ml50[[2]] <- subset(wpo, Treat=='50mL' & Soil=='C')

plot(cont[[1]][,'nday'], cont[[1]][,'pd_wp.mean.na'], lty=2, col=myPal[1], lwd=2, ylim=c(-4.5,0), type='l',
     xlab='Time (days)', ylab=expression(Psi[pd]~(MPa)), cex.lab=2, xlim=c(0,35))
for (i in 1:length(cont)){
  lines(cont[[i]][,'nday'], cont[[i]][,'pd_wp.mean.na'], col=myPal[i], lwd=2, lty=2)
  Hmisc::errbar(cont[[i]][,'nday'], cont[[i]][,'pd_wp.mean.na'], cont[[i]][,'pd_wp.mean.na']+cont[[i]][,'pd_wp.s.err'],
                cont[[i]][,'pd_wp.mean.na'] - cont[[i]][,'pd_wp.s.err'], pch=1,  col=myPal[i],  cex=1.5, add=T)
  points(cont[[i]][,'nday'], cont[[i]][,'pd_wp.mean.na'], pch=19, col='white', cex=1.5)
  points(cont[[i]][,'nday'], cont[[i]][,'pd_wp.mean.na'], col='black', pch=21, cex=1.5) 
}
for (i in 1:length(cont)){
  lines(dro[[i]][,'nday'], dro[[i]][,'pd_wp.mean.na'], col=myPal[i], lwd=2, lty=1)
  Hmisc::errbar(dro[[i]][,'nday'], dro[[i]][,'pd_wp.mean.na'], dro[[i]][,'pd_wp.mean.na']+dro[[i]][,'pd_wp.s.err'],
                dro[[i]][,'pd_wp.mean.na'] - dro[[i]][,'pd_wp.s.err'], pch=19,  col=myPal[i],  cex=1.5, add=T)
  points(dro[[i]][,'nday'], dro[[i]][,'pd_wp.mean.na'], pch=19, col=myPal[i], cex=1.5)
  points(dro[[i]][,'nday'], dro[[i]][,'pd_wp.mean.na'], col='black', pch=21, cex=1.5)
}
for (i in 1:length(ml50)){
  Hmisc::errbar(ml50[[i]][,'nday'], ml50[[i]][,'pd_wp.mean.na'], ml50[[i]][,'pd_wp.mean.na']+ml50[[i]][,'pd_wp.s.err'],
                ml50[[i]][,'pd_wp.mean.na'] - ml50[[i]][,'pd_wp.s.err'], pch=17,  col=myPal[i],  cex=1.5, add=T)
  points(ml50[[i]][,'nday'], ml50[[i]][,'pd_wp.mean.na'], pch=17, col=myPal[i], cex=1.5)
  points(ml50[[i]][,'nday'], ml50[[i]][,'pd_wp.mean.na'], col='black', pch=24, cex=1.5)
}
legend('bottomleft', lty=c(1,1,2,1,NA), pch=c(NA,NA,1,19,17), col=c('deeppink','cornflowerblue','black','black','darkgrey'),
       legend=c('Coarse Sand', 'Sandy Clay Loam','Control','Dry','50 mL'), bty='n', lwd=2, cex=1.4)