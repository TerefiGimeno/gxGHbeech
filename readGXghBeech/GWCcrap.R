source('hydroBread/basicFun.R')
source('hydroBread/swc.R')

swcSumm <- doBy::summaryBy(GWC ~ Soil + nday + Treat, data = swc, FUN=c(mean.na, s.err), keep.names = T)
swcSumm$nday2 <- swcSumm$nday
swcSumm$nday <- ifelse(swcSumm$nday==0, 1, swcSumm$nday)

gwcExt <- read.csv('hydroBdata/GWC_ext.csv')
gwcExt$gwc <- gwcExt$GWC*100
gwcExt$DateInt <- gwcExt$Date
gwcExt$Date <- lubridate::ymd(as.character(gwcExt$Date))
gwcExt$nday <- as.numeric(gwcExt$Date - as.Date('2018-05-16'))
gwcExt <- merge(gwcExt, swcSumm, by=c('Soil', 'nday', 'Treat'), all.x = T, all.y = F)

gwcExtSumm <- doBy::summaryBy(gwc ~ Soil + nday + Treat, data = gwcExt, FUN=c(mean.na, s.err), keep.names = T)

A <- subset(gwcExt, Soil == 'A')
B <- subset(gwcExt, Soil == 'B')
C <- subset(gwcExt, Soil == 'C')
summary(lm(gwc~GWC.mean.na, data=A))
summary(lm(gwc~GWC.mean.na, data=B))
summary(lm(gwc~GWC.mean.na, data=C))
plot(A$gwc~A$GWC.mean.na, ylim=c(-0.01,50), pch=19, col=as.factor(A$Treat), ylab='GWC ext (%)', xlab='GWC (%)')
points(B$gwc~B$GWC.mean.na, pch=15, col=as.factor(B$Treat))
points(C$gwc~C$GWC.mean.na, pch=17, col=as.factor(C$Treat))

#pretty graph
summG <- merge(subset(gwcExtSumm, Treat=='CONTROL' | Treat=='DRY'), swcSumm, by=c('Soil','nday','Treat'), all=T)

cont <- list()
for (i in 1:length(levels(summG$Soil))){
  cont[[i]] <- subset(summG, Treat=='CONTROL' & Soil==levels(summG$Soil)[i])
}
dro <- list()
for (i in 1:length(levels(summG$Soil))){
  dro[[i]] <- subset(summG, Treat=='DRY' & Soil==levels(swcSumm$Soil)[i])
}
myPal <- palette(c('deeppink','chocolate4','cornflowerblue'))
windows(10,12)
par(mfrow=c(2,1), las=1)
par(mar=c(0,6,2,2))
plot(cont[[1]][,'nday'], cont[[1]][,''], lty=2, col=myPal[1], lwd=2, ylim=c(-4.5,0), type='l',
     axes=F, xlab='', ylab=expression(Psi[pd]~(MPa)), cex.lab=1.5, xlim=c(1,37))
axis(2, at=seq(-4.5, 0, 0.5), labels = seq(-4.5, 0, 0.5), las=1)
box()
for (i in 1:length(cont)){
  lines(cont[[i]][,'nday'], cont[[i]][,'pd_wp.mean.na'], col=myPal[i], lwd=2, lty=2)
  Hmisc::errbar(cont[[i]][,'nday'], cont[[i]][,'pd_wp.mean.na'], cont[[i]][,'pd_wp.mean.na']+cont[[i]][,'pd_wp.s.err'],
                cont[[i]][,'pd_wp.mean.na'] - cont[[i]][,'pd_wp.s.err'], pch=1,  col=myPal[i],  cex=1.2, add=T)
  points(cont[[i]][,'nday'], cont[[i]][,'pd_wp.mean.na'], pch=19, col='white', cex=1.2)
  points(cont[[i]][,'nday'], cont[[i]][,'pd_wp.mean.na'], col='black', pch=21, cex=1.2) 
}
for (i in 1:length(cont)){
  lines(dro[[i]][,'nday'], dro[[i]][,'pd_wp.mean.na'], col=myPal[i], lwd=2, lty=1)
  Hmisc::errbar(dro[[i]][,'nday'], dro[[i]][,'pd_wp.mean.na'], dro[[i]][,'pd_wp.mean.na']+dro[[i]][,'pd_wp.s.err'],
                dro[[i]][,'pd_wp.mean.na'] - dro[[i]][,'pd_wp.s.err'], pch=19,  col=myPal[i],  cex=1.2, add=T)
  points(dro[[i]][,'nday'], dro[[i]][,'pd_wp.mean.na'], pch=19, col=myPal[i], cex=1.2)
  points(dro[[i]][,'nday'], dro[[i]][,'pd_wp.mean.na'], col='black', pch=21, cex=1.2)
}
legend('bottomleft', lty=1, col=c('deeppink','chocolate4','cornflowerblue'),
       legend=c('Sand', 'Sand & Rock', 'Sandy Clay Loam'), bty='n', lwd=2)
legend(-2.5, 0.3, legend='(a)', cex=1.7,  bty = 'n')