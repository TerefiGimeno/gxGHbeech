source('hydroBread/gasExAnalyses.R')
gasExHB$Treat2 <- gasExHB$Treat
gasExHB[which(gasExHB$Treat=='50mL'),'Treat2'] <- 'DRY'
gasExHB <- subset(gasExHB, notes!='low light')
#function to fit the CAP version from Dewar et al. 2017
#Gamma* is 42.75, from Bernacchi et al. 2001
fitUSO <- function(df){
  myFit <- nls(gc ~ g0+(1+xi/sqrt(Dmmol))*(Photo/(CO2S-42.75)), start=list(g0=0,xi=3), data=df)
  return(myFit)
}
fitMES <- function(df){
  myFit <- lm(gc~MESindex, data=df)
  return(myFit)
}
mylevels <- c('CONTROL','DRY')
gasexL <- list()
for (i in 1:length(mylevels)){
  gasexL[[i]] <- subset(subset(gasExHB, Soil=='A'), Treat2==mylevels[i])
}
for (i in 1:length(mylevels)){
  gasexL[[i+length(mylevels)]] <- subset(subset(gasExHB, Soil=='C'), Treat2==mylevels[i])
}
modelFits <- lapply(gasexL, fitUSO)
modelFitsMES <- lapply(gasexL, fitMES)
resultsFitsInt <- data.frame(row.names = 1:length(gasexL))
for (i in 1:length(gasexL)){
  resultsFitsInt$Soil[i] <- gasexL[[i]][1, 'Soil']
  resultsFitsInt$Treat[i] <- gasexL[[i]][1, 'Treat']
  resultsFitsInt$g0[i] <- coefficients(modelFits[[i]])[1]
  resultsFitsInt$g0_err[i] <- summary(modelFits[[i]])$coefficients[3]
  resultsFitsInt$t_val_g0[i] <- summary(modelFits[[i]])$coefficients[5]
  resultsFitsInt$P_val_g0[i] <- summary(modelFits[[i]])$coefficients[7]
  resultsFitsInt$xi[i] <- coefficients(modelFits[[i]])[2]
  resultsFitsInt$xi_err[i] <- summary(modelFits[[i]])$coefficients[4]
  resultsFitsInt$t_val_xi[i] <- summary(modelFits[[i]])$coefficients[6]
  resultsFitsInt$P_val_xi[i] <- summary(modelFits[[i]])$coefficients[8]
  resultsFitsInt$upCI_xi[i] <- confint(modelFits[[i]])[2]
  resultsFitsInt$lwCI_xi[i] <- confint(modelFits[[i]])[4]
  resultsFitsInt$CI_xi[i] <- resultsFitsInt[i,'lwCI_Ep']-resultsFitsInt[i,'Ep']
  resultsFitsInt$AIC_CAP[i] <- AIC(modelFits[[i]])
  resultsFitsInt$RMSE_CAP[i] <- sqrt(sum((gasexL[[i]][,'gc']-predict(modelFits[[i]]))^2)
                                     /noNAlength(gasexL[[i]][,'gc']))
  resultsFitsInt$intMES[i] <- coefficients(modelFitsMES[[i]])[1]
  resultsFitsInt$intMES_err[i] <- summary(modelFitsMES[[i]])$coefficients[3]
  resultsFitsInt$t_val_intMES[i] <- summary(modelFitsMES[[i]])$coefficients[5]
  resultsFitsInt$P_val_intMES[i] <- summary(modelFitsMES[[i]])$coefficients[7]
  resultsFitsInt$sqrtEmax[i] <- coefficients(modelFitsMES[[i]])[2]
  resultsFitsInt$sqrtEmax_err[i] <- summary(modelFitsMES[[i]])$coefficients[4]
  resultsFitsInt$t_val_MES[i] <- summary(modelFitsMES[[i]])$coefficients[6]
  resultsFitsInt$P_val_MES[i] <- summary(modelFitsMES[[i]])$coefficients[8]
  resultsFitsInt$AIC_MES[i] <- AIC(modelFitsMES[[i]])
  resultsFitsInt$R2_MES[i] <- summary(modelFitsMES[[i]])$r.squared
  resultsFitsInt$RMSE_MES[i] <- sqrt(sum((gasexL[[i]][which(!is.na(gasexL[[i]][,'MESindex'])),'gc']-
                                            predict(modelFitsMES[[i]]))^2)/noNAlength(gasexL[[i]][,'gc']))
}
resultsFitsInt$Emax <- resultsFitsInt$sqrtEmax^2
resultsFitsInt$Emax_err <- 2*resultsFitsInt$sqrtEmax*resultsFitsInt$sqrtEmax_err
resultsFitsInt$g1 <- resultsFitsInt$xi*sqrt(0.1016)/1.6
resultsFitsInt$g1_err <- resultsFitsInt$xi_err*sqrt(0.1016)/1.6
resultsFitsInt$g1_CI <- (resultsFitsInt$xi-resultsFitsInt$lwCI_xi)*sqrt(0.1016)/1.6
resultsFitsInt$Soil <- ifelse(resultsFitsInt$Soil==1, 'A', 'C')
resultsFitsInt$Treat <- ifelse(resultsFitsInt$Treat==3, 'CONTROL', 'DRY')

for (i in 1:length(gasexL)){
  gasexL[[i]][,'gc_CAP'] <- predict(modelFits[[i]],
                                    list(Photo=gasexL[[i]][,'Photo'],
                                         Dmmol=gasexL[[i]][,'Dmmol'],CO2=gasexL[[i]][,'CO2S']))
  gasexL[[i]][,'gc_MES'] <- predict(modelFitsMES[[i]],
                                    list(MESindex=gasexL[[i]][,'MESindex']))
}
gasexPred <- as.data.frame(data.table::rbindlist(gasexL))
write.csv(gasexPred, file='hydroBoutput/gasexPred.csv', row.names=F)

USOdry <- fitUSO(subset(gasExHB, Treat2=='DRY'))
USOcont <- fitUSO(subset(gasExHB, Treat2=='CONTROL'))

forPredCont <- subset(forPred, Treat2='CONTROL')
forPredCont$gc_CAP <- coef(USOcont)[1]+(1+(coef(USOcont)[2]/sqrt(forPredCont$Dmmol)))*
  (forPredCont$Photo/(forPredCont$CO2S-gammaStar))
forPredDry <- subset(forPred, Treat2='DRY')
forPredDry$gc_CAP <- coef(USOdry)[1]+(1+(coef(USOdry)[2]/sqrt(forPredDry$Dmmol)))*
  (forPredDry$Photo/(forPredDry$CO2S-gammaStar))
forPred <- rbind(forPredDry, forPredCont)
forPred$gs_pred <- forPred$gc_CAP*1.6
write.csv(forPred, file='hydroBoutput/condPred.csv', row.names=F)

confint(lm(gc_CAP~gc, data=gasexPred))
anova(lm(gc_MES~gc*Soil*Treat, data=gasexPred))
