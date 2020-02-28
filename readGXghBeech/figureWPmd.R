source("hydroBread/quickWP.R")
wpo <- subset(wpSumm, Treat=='DRY' | Treat == 'CONTROL')
cheat <- subset(wpo, Treat=='DRY' & nday==1)
cheat$Treat <- rep('CONTROL', 3)
wpo <- rbind(wpo, cheat)
wpo <- subset(wpo, md_wp.mean.na<=0)

myPal <- palette(c('firebrick4','dodgerblue3','darkolivegreen4'))

cont <- list()
for (i in 1:length(levels(wpo$Soil))){
  cont[[i]] <- subset(wpo, Treat=='CONTROL' & Soil==levels(wpo$Soil)[i])
  cont[[i]] <- doBy::orderBy(~nday, cont[[i]])
}
dro <- list()
for (i in 1:length(levels(wpo$Soil))){
  dro[[i]] <- subset(wpo, Treat=='DRY' & Soil==levels(wpo$Soil)[i])
  dro[[i]] <- doBy::orderBy(~nday, dro[[i]])
}

windows(12,8)
par(mfrow=c(2,1), las=1)
par(mar=c(0,4.5,4.5,2))

plot(cont[[1]][,'nday'], cont[[1]][,'md_wp.mean.na'], lty=2, col=myPal[1], lwd=2, ylim=c(-4.7,-0.5),
     ylab=expression(Psi[md]~(MPa)),  xlab='Time (days)',cex.lab=1.5, type='l', xlim=c(0,35), axes=F)
axis(2, at=seq(-5, -0.5, 1), labels = seq(-5, -0.5, 1), las=1)
box()

for (i in 1:length(cont)){
  lines(cont[[i]][,'nday'], cont[[i]][,'md_wp.mean.na'], col=myPal[i], lwd=2, lty=2)
  Hmisc::errbar(cont[[i]][,'nday'], cont[[i]][,'md_wp.mean.na'], cont[[i]][,'md_wp.mean.na'] + cont[[i]][,'md_wp.s.err'],
                cont[[i]][,'md_wp.mean.na'] - cont[[i]][,'md_wp.s.err'], pch=1,  col=myPal[i],  cex=1.3, add=T)
  points(cont[[i]][,'nday'], cont[[i]][,'md_wp.mean.na'], pch=21, col='black', bg=myPal[i], cex=1.3)
}
for (i in 1:length(cont)){
  lines(dro[[i]][,'nday'], dro[[i]][,'md_wp.mean.na'], col=myPal[i], lwd=2, lty=1)
  Hmisc::errbar(dro[[i]][,'nday'], dro[[i]][,'md_wp.mean.na'], dro[[i]][,'md_wp.mean.na'] + dro[[i]][,'md_wp.s.err'],
                dro[[i]][,'md_wp.mean.na'] - dro[[i]][,'md_wp.s.err'], pch=25,  col='black',  cex=1.3, add=T)
  points(dro[[i]][,'nday'], dro[[i]][,'md_wp.mean.na'], pch=25, col='black', bg=myPal[i], cex=1.3)
}
legend('bottomleft', lty=1, col=myPal,
       legend=c('Coarse Sand', 'Coarse Sand & Rock', 'Sandy Clay Loam'), bty='n', lwd=2)
legend('topright', legend='(a)', bty='n', cex=1.2)

par(mar=c(4.5,4.5,0,2))
plot(cont[[1]][,'nday'], cont[[1]][,'delta.mean.na'], lty=2, col=myPal[1], lwd=2, ylim=c(-0.3,2),
     ylab=expression(Delta*Psi[pd-md]~(MPa)),  xlab='Time (days)',cex.lab=1.5, type='l', xlim=c(0,35))
for (i in 1:length(cont)){
  lines(cont[[i]][,'nday'], cont[[i]][,'delta.mean.na'], col=myPal[i], lwd=2, lty=2)
  Hmisc::errbar(cont[[i]][,'nday'], cont[[i]][,'delta.mean.na'], cont[[i]][,'delta.mean.na'] + cont[[i]][,'delta.s.err'],
                cont[[i]][,'delta.mean.na'] - cont[[i]][,'delta.s.err'], pch=1,  col='black',  cex=1.3, add=T)
  points(cont[[i]][,'nday'], cont[[i]][,'delta.mean.na'], pch=21, col='black', bg=myPal[i], cex=1.3)
}
for (i in 1:length(cont)){
  lines(dro[[i]][,'nday'], dro[[i]][,'delta.mean.na'], col=myPal[i], lwd=2, lty=1)
  Hmisc::errbar(dro[[i]][,'nday'], dro[[i]][,'delta.mean.na'], dro[[i]][,'delta.mean.na'] + dro[[i]][,'delta.s.err'],
                dro[[i]][,'delta.mean.na'] - dro[[i]][,'delta.s.err'], pch=25,  col=myPal[i],  cex=1.3, add=T)
  points(dro[[i]][,'nday'], dro[[i]][,'delta.mean.na'], pch=25, col='black', bg=myPal[i], cex=1.3)
}
legend('bottomleft', lty=c(2,1), pch=c(21,25), legend=c('Control', 'Dry'), bty='n', lwd=2, cex=1.3)
legend('topright', legend='(b)', cex=1.2, bty='n')