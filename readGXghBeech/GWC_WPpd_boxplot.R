source("hydroBread/calcE.R")
source("hydroBread/quickWP.R")
wp <- subset(wp, Treat=='DRY' | Treat == 'CONTROL')
k <- subset(wp, nday == 1)
k$Treat <- c(rep('CONTROL', times=nrow(k)))
wp <- rbind(wp, k)
wp <- doBy::orderBy(~Treat+Soil+nday, data=wp)
wp[which(is.na(wp$pd_wp) & wp$nday == 15 & wp$Treat =='CONTROL'), 'pd_wp'] <- 100
swc$GWC2 <- swc$GWC
swc$GWC <- swc$GWC2/100
swc$VWC2 <- swc$VWC
swc$VWC <- swc$VWC/100

myPal <- palette(c('#59B4E9', '#F0E442', 'darkolivegreen4'))
windows(15,10)

par(mfrow=c(2, 2), mar=c(0,6,5,0), las=1, cex=1.1)
boxplot(GWC ~ Soil*nday, data = subset(swc, Treat == 'CONTROL'), col=myPal, 
        outline=FALSE, xaxt='n', boxwex=0.45,
        at=sort(c(unique(swc$nday)-0.5, unique(swc$nday), unique(swc$nday)+0.5)), 
        ylab=expression('GWC'~(g~g^-1)), xlab='', cex.lab=1.2, ylim=c(0, 0.53),
        main = 'Control')
par(mar=c(0,0,5,6))
boxplot(GWC ~ Soil*nday, data = subset(swc, Treat == 'DRY'), col=myPal, 
        outline=FALSE, xaxt='n', boxwex=0.45,
        at=sort(c(unique(swc$nday)-0.5, unique(swc$nday), unique(swc$nday)+0.5)), 
        ylab='', xlab='', cex.lab=1.2, ylim=c(0, 0.53),
        main = 'Drought', axes = F)
box()
legend('topright', legend=c('Sand','Sand & Rock','Sandy Clay Loam'), pch=c(rep(19,3)),
       col=myPal, bty='n', cex = 1.2)

par(mar=c(5,6,0,0))
boxplot(pd_wp ~ Soil*nday, data = subset(wp, Treat == 'CONTROL'), col=myPal, 
        outline=FALSE, xaxt='n', boxwex=0.45,
        at=sort(c(unique(wp$nday)-0.5, unique(wp$nday), unique(wp$nday)+0.5)), 
        ylab=expression(Psi[pd]~(MPa)), xlab='Days since beginning drought treatment',
        cex.lab=1.2, ylim=c(-5, 0))
axis(1, at=seq(0, 35, 5), labels=seq(0, 35, 5))
par(mar=c(5,0,0,6))
boxplot(pd_wp ~ Soil*nday, data = subset(wp, Treat == 'DRY'), col=myPal, 
        outline=FALSE, xaxt='n', boxwex=0.45,
        at=sort(c(unique(wp$nday)-0.5, unique(wp$nday), unique(wp$nday)+0.5)), 
        ylab='', xlab='Days since beginning drought treatment',
        cex.lab=1.2, ylim=c(-5, 0), axes = F)
axis(1, at=seq(0, 35, 5), labels=seq(0, 35, 5))
box()
