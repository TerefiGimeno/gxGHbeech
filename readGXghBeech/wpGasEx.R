library(lubridate)
source('hydroBread/basicFun.R')
wp <- read.csv('hydroBoutput/wpWide.csv')
wp$Date <-ymd(as.character(wp$Date))
wp$nday <- as.numeric(wp$Date-as.Date("2018-05-16"))
wp$delta <- wp$pd_wp-wp$md_wp

wpSumm <- doBy::summaryBy(pd_wp + md_wp + delta ~ nday + Soil + Treat, FUN=c(mean.na, s.err, length),
                          data=wp)
wp2 <- subset(wp, Treat!='BAG')
wpo <- doBy::summaryBy(pd_wp + md_wp + delta ~ nday + Soil + Treat, FUN=c(mean.na, s.err, length),
                          data=wp2)