poromCal <- read.csv("hydroBdata/poromCalib.csv")
poromCal$leafID <- paste0(poromCal$plantID, "_", poromCal$rep)
poromCal <- merge(subset(poromCal, instrument=='Porometer ECOFUN1')[,c("leafID",'cond_mmol.m2.s1')],
                  subset(poromCal, instrument=='Porometer ECOFUN2')[,c("leafID",'cond_mmol.m2.s1')],
                  by='leafID', all=T)
poromCal$soilType <- substr(poromCal$leafID, 1, 1)
poromCal$plantID <- ifelse(nchar(poromCal$leafID)==9, substr(poromCal$leafID, 2, 3),
                           substr(poromCal$leafID, 2, 4),)
