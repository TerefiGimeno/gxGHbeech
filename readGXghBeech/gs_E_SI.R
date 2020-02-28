source('hydroBread/calcE.R')

windows(10,12)
par(mfrow=c(3,1), las=1, cex=1.2)
par(mar=c(0,6,6,2))
plot(climateShort$VpdMor~climateShort$nday, ylim=c(0,3), type='l', col='forestgreen', lwd=2.5,
     ylab=expression(italic(D)[mor]~(kPa)), xlab=' ', axes=F, cex.lab=1.1, xlim=c(-1,36))
axis(2, at=seq(0, 3, 1), labels = seq(0, 3, 1), las=1)
axis(3, at=seq(0, 35, 5), labels = seq(0, 35, 5), las=1)
mtext('Time (days)', crt=90, side=3, line=3, las=0, cex=1.1)
box()
legend('topleft', legend='(a)', bty='n')
par(mar=c(0,6,0,2))
plot(subset(poromSumm, Treat=='CONTROL')[,'Et']~subset(poromSumm, Treat=='CONTROL')[,'nday'],
       ylab=expression(italic(E)~(mmol~m^-2~s^-1)), ylim=c(-0.05,5.5), xlim=c(-1,36),
     type='l', lwd=1.8, cex.lab=1.1, axes=F, xlab='', lty=2)
Hmisc::errbar(subset(poromSumm, Treat=='CONTROL')[,'nday'], subset(poromSumm, Treat=='CONTROL')[,'Et'],
              subset(poromSumm, Treat=='CONTROL')[,'Et']+subset(poromSumm, Treat=='CONTROL')[,'EtSE'],
              subset(poromSumm, Treat=='CONTROL')[,'Et']-subset(poromSumm, Treat=='CONTROL')[,'EtSE'],
              cex=1.2, add=T, pch=21, col='black', bg='white')
points(subset(poromSumm, Treat=='CONTROL')[,'nday'], subset(poromSumm, Treat=='CONTROL')[,'Et'],
       pch=21, col='black', bg='white', cex=1.2)
lines(subset(poromSumm, Treat=='DRY')[,'nday'], subset(poromSumm, Treat=='DRY')[,'Et'],
       lwd=1.8)
Hmisc::errbar(subset(poromSumm, Treat=='DRY')[,'nday'], subset(poromSumm, Treat=='DRY')[,'Et'],
              subset(poromSumm, Treat=='DRY')[,'Et']+subset(poromSumm, Treat=='DRY')[,'EtSE'],
              subset(poromSumm, Treat=='DRY')[,'Et']-subset(poromSumm, Treat=='DRY')[,'EtSE'],
              cex=1.2, add=T, pch=21, col='black', bg='black')
axis(2, at=seq(0, 5, 2), labels = seq(0, 5, 2), las=1)
box()
legend('topleft', legend='(b)', bty='n')
legend('bottomleft', legend=c('Control', 'Drought'), lty=c(2,1), pch=c(1, 19), bty='n')
par(mar=c(4,6,0,2))
plot(subset(poromSumm, Treat=='CONTROL')[,'gs']~subset(poromSumm, Treat=='CONTROL')[,'Date'],
     ylab=expression(italic(g)[s]~(mmol~m^-2~s^-1)), ylim=c(40,570), xlim=as.Date(c('2018-05-15','2018-06-21')),
     type='l', lwd=1.8, cex.lab=1.1, xlab='', lty=2)
Hmisc::errbar(subset(poromSumm, Treat=='CONTROL')[,'Date'], subset(poromSumm, Treat=='CONTROL')[,'gs'],
              subset(poromSumm, Treat=='CONTROL')[,'gs']+subset(poromSumm, Treat=='CONTROL')[,'gsSE'],
              subset(poromSumm, Treat=='CONTROL')[,'gs']-subset(poromSumm, Treat=='CONTROL')[,'gsSE'],
              cex=1.2, add=T, pch=21, col='black', bg='white')
points(subset(poromSumm, Treat=='CONTROL')[,'Date'], subset(poromSumm, Treat=='CONTROL')[,'gs'],
       pch=21, col='black', bg='white', cex=1.2)
lines(subset(poromSumm, Treat=='DRY')[,'Date'], subset(poromSumm, Treat=='DRY')[,'gs'],
      lwd=1.8)
Hmisc::errbar(subset(poromSumm, Treat=='DRY')[,'Date'], subset(poromSumm, Treat=='DRY')[,'gs'],
              subset(poromSumm, Treat=='DRY')[,'gs']+subset(poromSumm, Treat=='DRY')[,'gsSE'],
              subset(poromSumm, Treat=='DRY')[,'gs']-subset(poromSumm, Treat=='DRY')[,'gsSE'],
              cex=1.2, add=T, pch=21, col='black', bg='black')
legend('topleft', legend='(c)', bty='n')

