
# Estimate one-point Vcmax from data on A, Ci & Tleaf
# Function call: Vcmax1pt(Photo,Ci,Tleaf,Tcorr)
# Function needs input of photosynthesis, Ci, and Tleaf
# If Tcorr = T (default) it returns a value corrected to 25 deg 
# Otherwise value is at measurement temperature

# Functions
.Rgas <- function()8.314
Tk <- function(x)x+273.15
# Arrhenius
arrh <- function(Tleaf, Ea){
  exp((Ea * (Tk(Tleaf) - 298.15)) / (298.15 * .Rgas() * Tk(Tleaf))) 
}
#Gamma Star
TGammaStar <- function(Tleaf,  Egamma=37830.0, value25=42.75){  
  value25*arrh(Tleaf,Egamma)
}
# Km 
TKm <- function(Tleaf) {
  Kc <- 404.9*arrh(Tleaf,79430)
  Ko <- 278400*arrh(Tleaf,36380)
  Oi <- 205000
  Km <- Kc * (1+Oi/Ko)
  return(Km)
}
# Vcmax
peaked_arrh <- function(Tleaf, Ea, delS) {
  TleafK <- Tk(Tleaf)
  Ed <- 2E5
  func <- exp((Ea*(TleafK - 298.15))/(298.15 * .Rgas() * TleafK)) * 
    (1 + exp((298.15*delS - Ed)/(298.15 * .Rgas()))) / 
    (1 + exp((TleafK*delS - Ed)/(TleafK * .Rgas())))
  return(func)
}

# Main function
Vcmax1pt <- function(Photo,Ci,Tleaf,Tcorr=T) {
  
  RVrat <- 0.015 # assume Rd/Vcmax ratio equals 0.015
  Gstar <- TGammaStar(Tleaf)
  Km <- TKm(Tleaf)
  Vcmax <- Photo/((Ci-Gstar)/(Ci+Km) - RVrat) 
  Vcmax25 <- Vcmax / peaked_arrh(Tleaf, Ea = 67338, delS = 631.7)
  if (Tcorr == T) {
    return(Vcmax25)
  } else {
    return(Vcmax)
  }
}
