myTreats <- c('CONTROL','50mL','DRY')
myPal <- palette(c('deeppink','cornflowerblue'))
plot(subset(gasExHB, Soil=='A' & Treat=='CONTROL')[,'Vcmax']~
       subset(gasExHB, Soil=='A' & Treat=='CONTROL')[,'WP'], pch=)