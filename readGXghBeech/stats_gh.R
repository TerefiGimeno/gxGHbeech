###Stats for everything

###stats for VWC...look in swc.R to open data
modelGWC <- lm(log(GWC)~nday*Treat*Soil, data=swc)
summary(aov(log(GWC)~nday*Treat*Soil, data=swc))
modelVWC <- lm(log(VWC)~nday*Treat*Soil, data=swc)
summary(aov(log(VWC)~nday*Treat*Soil, data=swc))

swc$Soil<-relevel(swc$Soil,ref='A')
summary(modelVWC)

model <- lm(log(VWC)~Treat, data=swc)
summary(model)

eff.pres <- effects::allEffects(modelVWC) 
plot(eff.pres,multiline=T)

swc$VWC

###sandy
swc_sand<-subset(swc,Soil=='A')
swc_sand$swp<-(-6)*exp(-64*swc_sand$VWC/100)
ggplot(subset(swc_sand, Treat=='DRY'),aes(x=as.factor(nday),y=swp))+geom_boxplot()

# use the other equation
swc_sand$swp2<-(-16)*exp(-110*swc_sand$VWC/100)
ggplot(subset(swc_sand, Treat=='DRY'),aes(x=as.factor(nday),y=swp2))+geom_boxplot()

swc_clay<-subset(swc,Soil=='C')
swc_clay$swp<-(-4)*exp(-24*swc_clay$VWC/100)
ggplot(subset(swc_clay, Treat=='DRY'),aes(x=as.factor(nday),y=swp))+geom_boxplot()

# use the other equation
swc_clay$swp2<-(-224)*exp(-33*swc_clay$VWC/100)
ggplot(subset(swc_clay, Treat=='DRY'),aes(x=as.factor(nday),y=swp2))+geom_boxplot()


###stats per water potential i stomatal conductance
stomata$nday[stomata$Date=='180517'] <- "1"
stomata$nday[stomata$Date=='180524'] <- "9"
stomata$nday[stomata$Date=='180531'] <- "15"
stomata$nday[stomata$Date=='180613'] <- "29"
stomata$nday[stomata$Date=='180620'] <- "36"

stomata.nobag<-subset(stomata,!Treat=="BAG")

stomata.nobag$Soil<-relevel(stomata.nobag$Soil,ref='B')

model<-lm(stomata~Treat*Soil*nday,data=stomata.nobag)
summary(model)

stomata.bag<-subset(stomata,!Soil=='B')
stomata.bag<-subset(stomata.bag,!Soil=='C')
stomata.bag<-subset(stomata.bag,Date=='180517'|Date=='180524')

stomata.bag<-subset(stomata.bag,!Treat=='CONTROL')

model<-lm(stomata~Treat,data=stomata.bag)
summary(model)

wp$nday[wp$Date=='20180517'] <- "1"
wp$nday[wp$Date=='20180524'] <- "9"
wp$nday[wp$Date=='20180531'] <- "15"
wp$nday[wp$Date=='20180613'] <- "29"
wp$nday[wp$Date=='20180620'] <- "36"

wp.nobag<-subset(wp,!Treat=="BAG")
wp.nobag$diff_wp<-wp.nobag$pd_wp-wp.nobag$md_wp

wp.nobag$Soil<-relevel(wp.nobag$Soil,ref='C')

model<-lm(pd_wp~Treat*Soil*nday,data=wp.nobag)
summary(model)
model<-lm(md_wp~Treat*Soil*nday,data=wp.nobag)
summary(model)

wp.nobag$diff_wp<-wp.nobag$pd_wp-wp.nobag$md_wp

model<-lm(diff_wp~Treat*Soil*nday,data=wp.nobag)
summary(model)

wp.bag<-subset(wp,!Soil=='B')
wp.bag<-subset(wp.bag,!Soil=='C')
wp.bag<-subset(wp.bag,Date=='20180517'|Date=='20180524')

wp.bag<-subset(wp.bag,!Treat=='CONTROL')
model<-lmer(pd_wp~Treat+(1|Date),data=wp.bag)
summary(model)
model<-lmer(md_wp~Treat+(1|Date),data=wp.bag)
summary(model)

wp.bag$diff_wp<-wp.bag$pd_wp-wp.bag$md_wp

model<-lmer(diff_wp~Treat+(1|Date),data=wp.bag)
summary(model)

##stats for isotopic compositions
isotopes<-subset(gh,!Type=="IRRIGATIONW")

isotopes$nday[isotopes$Date=='180517'] <- "1"
isotopes$nday[isotopes$Date=='180524'] <- "9"
isotopes$nday[isotopes$Date=='180531'] <- "15"
isotopes$nday[isotopes$Date=='180613'] <- "29"
isotopes$nday[isotopes$Date=='180620'] <- "36"
isotopes$nday<-as.numeric(isotopes$nday)

control<-subset(isotopes,Treat="CONTROL")
control<-subset(control,!Type=='Rock')

control$Type<-relevel(control$Type,ref='Soil')

model<-lm(delta_D_various~Type*nday*Soil,data=control)
summary(model)
model<-lm(delta_O18_various~Type*nday*Soil,data=control)
summary(model)
summary(model)

anova(model)
emmeans(model,~Type)

isotopes$Type<-relevel(isotopes$Type,ref='Soil')

rocks<-subset(control,Soil=="B")

isotopes<-subset(isotopes,!Type=='Rock')
drought<-subset(isotopes,Treat=='DRY')

drought$Type<-relevel(drought$Type,ref='Stem')
drought$Type<-relevel(drought$Type,ref='Root')

model<-lm(delta_D_various~Type*nday*Soil,data=drought)
summary(model)
model<-lm(delta_O18_various~Type*nday*Soil,data=drought)
summary(model)

rocks.control<-subset(rocks,Treat=='CONTROL')
rocks.dry<-subset(rocks,Treat=='DRY')

model<-lm(delta_D_various~Type*nday,data=rocks.dry)
summary(model)
model<-lm(delta_O18_various~Type*nday,data=rocks.dry)
summary(model)

isotopes.bag<-subset(isotopes,Date=='180517'|Date=='180524')
isotopes.bag<-subset(isotopes.bag,!Treat=='CONTROL')
isotopes.bag<-subset(isotopes.bag,Soil=='B')

model<-lm(delta_D_various~Type*Treat,data=isotopes.bag)
summary(model)
model<-lm(delta_O18_various~Type*Treat,data=isotopes.bag)
summary(model)

###Statistics for everything
###isotopic differences between soil and stem
stemsoil.stats<-rbind(Soil,Stem)
stemsoil.stats<-subset(stemsoil.stats,!Treat=="BAG")
stemsoil.stats.control<-subset(stemsoil.stats,Treat=="CONTROL")
stemsoil.stats.dry<-subset(stemsoil.stats,Treat=="DRY")

model<-lmer(delta_D_various~Type*Soil*Treat+(1|Date),data=stemsoil.stats)
summary(model)
anova(model)
model<-lmer(delta_O18_various~Type*Soil*Treat+(1|Date),data=stemsoil.stats)
summary(model)

model<-lmer(delta_D_various~Type+(1|Date),data=stemsoil.stats.control)
summary(model)
model<-lmer(delta_O18_various~Type+(1|Date),data=stemsoil.stats.control)
summary(model)
model<-lmer(delta_D_various~Type+(1|Date),data=stemsoil.stats.dry)
summary(model)
model<-lmer(delta_O18_various~Type+(1|Date),data=stemsoil.stats.dry)
summary(model)

model<-lmer(delta_D_various~Type*Soil+(1|Date),data=stemsoil.stats.control)
summary(model)
model<-lmer(delta_O18_various~Type*Soil+(1|Date),data=stemsoil.stats.control)
summary(model)
model<-lmer(delta_D_various~Type*Soil+(1|Date),data=stemsoil.stats.dry)
summary(model)
model<-lmer(delta_O18_various~Type*Soil+(1|Date),data=stemsoil.stats.dry)
summary(model)
###isotopic differences between soil and stem
rootsoil.stats<-rbind(Soil,Root)
rootsoil.stats<-subset(rootsoil.stats,!Treat=="BAG")
rootsoil.stats.control<-subset(rootsoil.stats,Treat=="CONTROL")
rootsoil.stats.dry<-subset(rootsoil.stats,Treat=="DRY")

model<-lmer(delta_D_various~Type*Soil*Treat+(1|Date),data=rootsoil.stats)
summary(model)
model<-lmer(delta_O18_various~Type*Soil*Treat+(1|Date),data=rootsoil.stats)
summary(model)

model<-lmer(delta_D_various~Type+(1|Date),data=rootsoil.stats.control)
summary(model)
model<-lmer(delta_O18_various~Type+(1|Date),data=rootsoil.stats.control)
summary(model)
model<-lmer(delta_D_various~Type+(1|Date),data=rootsoil.stats.dry)
summary(model)
model<-lmer(delta_O18_various~Type+(1|Date),data=rootsoil.stats.dry)
summary(model)

model<-lmer(delta_D_various~Type*Soil+(1|Date),data=rootsoil.stats.control)
summary(model)
model<-lmer(delta_O18_various~Type*Soil+(1|Date),data=rootsoil.stats.control)
summary(model)
model<-lmer(delta_D_various~Type*Soil+(1|Date),data=rootsoil.stats.dry)
summary(model)
model<-lmer(delta_O18_various~Type*Soil+(1|Date),data=rootsoil.stats.dry)
summary(model)
###isotopic differences between stem and roots
stemroot.stats<-rbind(Stem,Root)
stemroot.stats<-subset(stemroot.stats,!Treat=="BAG")
stemroot.stats.control<-subset(stemroot.stats,Treat=="CONTROL")
stemroot.stats.dry<-subset(stemroot.stats,Treat=="DRY")

model<-lmer(delta_D_various~Type*Soil*Treat+(1|Date),data=stemroot.stats)
summary(model)
model<-lmer(delta_O18_various~Type*Soil*Treat+(1|Date),data=stemroot.stats)
summary(model)

model<-lmer(delta_D_various~Type+(1|Date),data=stemroot.stats.control)
summary(model)
model<-lmer(delta_O18_various~Type+(1|Date),data=stemroot.stats.control)
summary(model)
model<-lmer(delta_D_various~Type+(1|Date),data=stemroot.stats.dry)
summary(model)
model<-lmer(delta_O18_various~Type+(1|Date),data=stemroot.stats.dry)
summary(model)

model<-lmer(delta_D_various~Type*Soil+(1|Date),data=stemroot.stats.control)
summary(model)
model<-lmer(delta_O18_various~Type*Soil+(1|Date),data=stemroot.stats.control)
summary(model)
model<-lmer(delta_D_various~Type*Soil+(1|Date),data=stemroot.stats.dry)
summary(model)
model<-lmer(delta_O18_various~Type*Soil+(1|Date),data=stemroot.stats.dry)
summary(model)

###Stats for the offset

stemsoil$nday[stemsoil$Date=='180517'] <- "1"
stemsoil$nday[stemsoil$Date=='180524'] <- "9"
stemsoil$nday[stemsoil$Date=='180531'] <- "15"
stemsoil$nday[stemsoil$Date=='180613'] <- "29"
stemsoil$nday[stemsoil$Date=='180620'] <- "36"
stemsoil$nday<-as.numeric(stemsoil$nday)

stemsoil.nobag<-subset(stemsoil,!Treat=="BAG")

stemsoil.control<-subset(stemsoil.nobag,Treat=="CONTROL")

model<-lmer(diffH~Treat*nday+(1|Soil),data=stemsoil.nobag)
summary(model)
model<-lmer(diffO~Treat*nday+(1|Soil),data=stemsoil.nobag)
summary(model)

model<-lm(diffH~Treat*nday*Soil,data=stemsoil.nobag)
summary(model)
model<-lm(diffO~Treat*nday*Soil,data=stemsoil.nobag)
summary(model)

rootsoil$nday[rootsoil$Date=='180517'] <- "1"
rootsoil$nday[rootsoil$Date=='180524'] <- "9"
rootsoil$nday[rootsoil$Date=='180531'] <- "15"
rootsoil$nday[rootsoil$Date=='180613'] <- "29"
rootsoil$nday[rootsoil$Date=='180620'] <- "36"
rootsoil$nday<-as.numeric(rootsoil$nday)
rootsoil.nobag<-subset(rootsoil,!Treat=="BAG")

rootsoil.control<-subset(rootsoil.nobag,Treat=="CONTROL")

model<-lmer(diffH~Treat*nday+(1|Soil),data=rootsoil.nobag)
summary(model)
model<-lmer(diffO~Treat*nday+(1|Soil),data=rootsoil.nobag)
summary(model)

model<-lm(diffH~Treat*nday*Soil,data=rootsoil.nobag)
summary(model)
model<-lm(diffO~Treat*nday*Soil,data=rootsoil.nobag)
summary(model)

##Offset
stemsoil.data<-merge(stemsoil3,Soil2,by='Plant')

model4<-lmer(diffO~GWC+md_wp+diff_wp+(1|Soil.x),data=stemsoil.data)
model5<-lmer(diffH~GWC+md_wp+diff_wp+(1|Soil.x),data=stemsoil.data)
summary(model4)
AIC(model4)
summary(model5)
AIC(model5)

install.packages('r2glmm')
library(r2glmm)
library("QuantPsyc")
r2beta(model4)
r2beta(model5)
lm.beta.lmer <- function(mod) {
  b <- fixef(mod)[-1]
  sd.x <- apply(getME(mod,"X")[,-1],2,sd)
  sd.y <- sd(getME(mod,"y"))
  b*sd.x/sd.y
}

lm.beta.lmer(model4)
lm.beta.lmer(model5)

##visualitzem els efectes

eff.pres <- allEffects(model4) 
plot(eff.pres,multiline=T)

library(GGally)
library(regclass)

ggpairs(model4)
VIF(model.soil) ##needs to be smaller than 10
library("QuantPsyc")
lm.beta(model5)

### test for bags

head(stemsoil3)
stemsoil.bag<-subset(stemsoil3,!Treat=='CONTROL')
stemsoil.bag<-subset(stemsoil.bag,Date=='180517'|Date=='180524')

model<-aov(diffH~Treat,stemsoil.bag)
summary(model)
TukeyHSD(model)

rootsoil.bag<-subset(rootsoil3,!Treat=='CONTROL')
rootsoil.bag<-subset(rootsoil.bag,Date=='180517'|Date=='180524')

model<-aov(diffO~Treat,rootsoil.bag)
summary(model)
TukeyHSD(model)

stemroot.bag<-subset(stemroot3,!Treat=='CONTROL')
stemroot.bag<-subset(stemroot.bag,Date=='180517'|Date=='180524')

model<-aov(diffH~Treat,stemroot.bag)
summary(model)
TukeyHSD(model)

###test for soil isotopes
gwc

stemsoil3.nobag<-subset(stemsoil3,!Treat=="BAG")
stemsoil3.nobag<-merge(stemsoil3.nobag,Soil2,by=c('Plant','Soil','Treat'))
stemsoil3.nobag<-subset(stemsoil3.nobag,!GWC>0.4)

model.soil<-lm(delta_D_various.x~md_wp+pd_wp+stomata+GWC,data=stemsoil3.nobag,na.action='na.omit')
summary(model.soil)
AIC(model.soil)

model.soil<-lm(delta_O18_various.x~md_wp+pd_wp+stomata+GWC,data=stemsoil3.nobag)
summary(model.soil)

eff.pres <- allEffects(model.soil) #  numero de l?nies
plot(eff.pres,multiline=TRUE)
r2beta(model.soil)

###diff wp over time

ggplot(stemsoil3.nobag,aes(x=Date.x,y=pd_wp,fill=Treat.x))+geom_boxplot()
