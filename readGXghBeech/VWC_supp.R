source("hydroBread/calcE.R")
source("hydroBread/quickWP.R")
wpo <- subset(wpSumm, Treat=='DRY' | Treat == 'CONTROL')
k <- subset(wpo, nday == 1)
k$Treat <- c(rep('CONTROL', times=nrow(k)))
wpo <- rbind(wpo, k)
wpo <- doBy::orderBy(~Treat+Soil+nday, data=wpo)
swc$GWC2 <- swc$GWC
swc$GWC <- swc$GWC2/100
swc$VWC2 <- swc$VWC
swc$VWC <- swc$VWC/100
swcSumm <- doBy::summaryBy(VWC + GWC ~ Soil + Treat + nday, data=swc, FUN=c(mean.na, s.err, length))

myPal <- palette(c('firebrick4','dodgerblue3','darkolivegreen4'))

windows(15,10)
par(mfrow=c(2,2), cex=1.15)

cont <- list()
for (i in 1:length(levels(swcSumm$Soil))){
  cont[[i]] <- subset(swcSumm, Treat=='CONTROL' & Soil==levels(swcSumm$Soil)[i])
}
par(mar=c(0,5,5,0))
plot(cont[[1]][,'nday'], cont[[1]][,'GWC.mean.na'], type='l', col='black', lwd=0.75, ylim=c(0,0.45),
     ylab=expression(GWC~(g~g^-1)), xlab=' ',cex.lab=1.3, xlim=c(0,36), axes=F, main = 'Control')
axis(2, at=seq(0, 0.45, 0.1), labels = seq(0, 0.45, 0.1), las=1)
box()
for (i in 1:length(cont)){
  lines(cont[[i]][,'nday'], cont[[i]][,'GWC.mean.na'], lwd=0.9)
  Hmisc::errbar(cont[[i]][,'nday'], cont[[i]][,'GWC.mean.na'], cont[[i]][,'GWC.mean.na'] + cont[[i]][,'GWC.s.err'],
                cont[[i]][,'GWC.mean.na'] - cont[[i]][,'GWC.s.err'], pch=21,  col='black', bg=myPal[i], cex=1.4, add=T)
  points(cont[[i]][,'nday'], cont[[i]][,'GWC.mean.na'], pch=21, col='black', bg=myPal[i], cex=1.4)
}
# legend('topleft', legend='(a) Control', text.font = 2, bty='n', cex = 1.2)
legend('bottomleft', legend=c('Sand','Sand & Rock','Sandy Clay Loam'), pch=c(rep(19,3)),
       col=myPal, bty='n', cex = 1.2)

dro <- list()
for (i in 1:length(levels(swcSumm$Soil))){
  dro[[i]] <- subset(swcSumm, Treat=='DRY' & Soil==levels(swcSumm$Soil)[i])
}

par(mar=c(0,0,5,5))
plot(dro[[1]][,'nday'], dro[[1]][,'GWC.mean.na'], col='black', lwd=0.75, ylim=c(0,0.45),
     ylab=' ', xlab=' ',cex.lab=1.3, type='l', xlim=c(0,36), axes=F, main = 'Drought')
axis(2, at=seq(0, 0.45, 0.1), labels=NA, las=1)
box()
for (i in 1:length(dro)){
  lines(dro[[i]][,'nday'], dro[[i]][,'GWC.mean.na'], col='black', lwd=0.9)
  Hmisc::errbar(dro[[i]][,'nday'], dro[[i]][,'GWC.mean.na'], dro[[i]][,'GWC.mean.na'] + dro[[i]][,'GWC.s.err'],
                dro[[i]][,'GWC.mean.na'] - dro[[i]][,'GWC.s.err'], pch=25,  col='black', bg=myPal[i],  cex=1.2, add=T)
  points(dro[[i]][,'nday'], dro[[i]][,'GWC.mean.na'], col='black', bg=myPal[i], pch=25, cex=1.2)
}
# legend('topleft', legend='(b) Drought', cex=1.2,  bty = 'n', text.font=2)

cont <- list()
for (i in 1:length(levels(wpo$Soil))){
  cont[[i]] <- subset(wpo, Treat=='CONTROL' & Soil==levels(swcSumm$Soil)[i])
}

par(mar=c(5,5,0,0))
plot(cont[[1]][,'nday'], cont[[1]][,'pd_wp.mean.na'], type='l', col='black', lwd=0.75, ylim=c(-4.5,0.5),
     xlab='Days since beginning drought treatment', ylab = '', cex.lab=1, xlim=c(0,36), axes=F)
axis(1, at=seq(0, 36, 5), labels = seq(0, 36, 5))
mtext(expression(Psi[pd]~(MPa)), side = 2, line = 3, cex = 1.3)
axis(2, at=seq(-4, 0.5, 1), labels = seq(-4, 0.5, 1), las=1)
box()
for (i in 1:length(cont)){
  lines(cont[[i]][,'nday'], cont[[i]][,'pd_wp.mean.na'], lwd=0.9, col='black')
  Hmisc::errbar(cont[[i]][,'nday'], cont[[i]][,'pd_wp.mean.na'], cont[[i]][,'pd_wp.mean.na']+cont[[i]][,'pd_wp.s.err'],
                cont[[i]][,'pd_wp.mean.na'] - cont[[i]][,'pd_wp.s.err'], pch=21, bg=myPal[i], col='black', cex=1.4, add=T)
  points(cont[[i]][,'nday'], cont[[i]][,'pd_wp.mean.na'], col='black', bg=myPal[i], pch=21, cex=1.4) 
}
# legend('topleft', legend='(c) Control', text.font = 2, bty='n', cex = 1.2)

par(mar=c(5,0,0,5))
dro <- list()
for (i in 1:length(levels(wpo$Soil))){
  dro[[i]] <- subset(wpo, Treat=='DRY' & Soil==levels(swcSumm$Soil)[i])
}
plot(dro[[1]][,'nday'], dro[[1]][,'pd_wp.mean.na'], type='l', col='black', lwd=0.75, ylim=c(-4.5,0.5),
     xlab='Days since beginning drought treatment', ylab='', cex.lab=1, xlim=c(0,36), axes=F)
axis(1, at=seq(0, 36, 5), labels = seq(0, 36, 5))
axis(2, at=seq(-4, 0, 1), labels = NA)
box()
for (i in 1:length(dro)){
  lines(dro[[i]][,'nday'], dro[[i]][,'pd_wp.mean.na'], lwd=0.9)
  Hmisc::errbar(dro[[i]][,'nday'], dro[[i]][,'pd_wp.mean.na'], dro[[i]][,'pd_wp.mean.na']+dro[[i]][,'pd_wp.s.err'],
                dro[[i]][,'pd_wp.mean.na'] - dro[[i]][,'pd_wp.s.err'], pch=25, bg=myPal[i], col='black', cex=1.2, add=T)
  points(dro[[i]][,'nday'], dro[[i]][,'pd_wp.mean.na'], col='black', bg=myPal[i], pch=25, cex=1.2) 
}
# legend('topleft', legend='(d) Drought', text.font = 2, bty='n', cex = 1.2)
