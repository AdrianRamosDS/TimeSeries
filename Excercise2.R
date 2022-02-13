rm(list=ls())

# Examen Final - Adrian Ramos Perez

# Lectura de la Serie:
df <- read.csv(file = 'iteso-2examen-2021.csv')
serie <- df$V21
serie_2 <- serie^2

mean(serie)
var(serie)
# Prueba de estacionariedad
adf.test(serie)  #h0: no estacionaria
kpss.test(serie) #h0: estacionaria

# PACF y ACF
pacf(serie)
acf(serie)
pacf(serie_2)
acf(serie_2)


archTest <- function(rtn,m=10){
  # Perform Lagrange Multiplier Test for ARCH effect of a time series
  # rtn: time series
  # m: selected AR order
  # Source:, 
  y=(rtn-mean(rtn))^2
  T=length(rtn)
  atsq=y[(m+1):T]
  x=matrix(0,(T-m),m)
  for (i in 1:m){
    x[,i]=y[(m+1-i):(T-i)]
  }
  md=lm(atsq~x)
  summary(md)
}

test.archtest <- archTest(serie)
