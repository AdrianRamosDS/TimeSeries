rm(list=ls())

# 1er Parcial - Adrian Ramos Perez

#1. GRAFICAR LA SERIE:
df <- read.csv(file = 'examen_iteso_21a.csv')
serie <- df$V3
plot(serie, type="l", lwd=1)
mean(serie)
# La gráfica parece mostrar una varianza y medias constantes, lo que indica una
# posible serie estacionaria débil.

#2.---DETERMINAR EL ORDEN DE INTEGRACIÓN DE LA SERIE---:
# Pruebas de estacionariedad/raíz unitaria:
adf.test(serie)
# Con p-value = 0.01201 para adf se rechaza hipótesis nula (no estacionaria)
kpss.test(serie)
# Con p-value = 0.1 para kpss se acepta hipótesis nula (estacionaria)
# Determinamos que la serie es ESTACIONARIA con ambas pruebas (probabilidad muy alta).
# Por lo que no es necesaria diferenciación y no tiene orden de integración (0).
# Así  mismo se descarta ruido blanco al haber algunos cuantos rezagos.

#3.---OBTENER FUNCIÓN DE AUTOCORRELACIÓN PARCIAL Y FUNCIÓN DE AUTOCORRELACIÓN---
# (Comentar implicaciones de los resultados (rezagos significativos) ).
autocor <- function(x,lag){
  xbar <- mean(x)
  numerador <- sum((x[(lag+1):length(x)]-xbar)*(x[1:(length(x)-lag)]-xbar))
  denominador <- sum((x-xbar)^2)
  return(numerador/denominador)
}
tautocor <- function(x, lag){
  rho <- NA
  t <- NA
  for(i in 1:lag){
    rho[i]<- autocor(x,i)
    rho2 <- rho^2
    t[i] <- rho[i]/sqrt((1+2*sum(rho2[1:(i-1)]))/length(x))
  }
  return(cbind(rho,(1-pnorm(abs(t)))/2))
}
autocor(serie,10)
tautocor(serie,10)
acf(serie)
# En ACF se aprecian 6 o 7 rezagos de error (ignorando el primer rezago consigo misma) (Q)
pacf(serie)
# En PACF se aprecian 2 rezagos autorregresivos de variable dependiente (P)

#4.---ESTABLECER LA LISTA DE POSIBLES CANDIDATOS A MODELO ARIMA---:
# de lo anterior se observan posibles modelos ARIMA:
# -(2,0,6) (a ojo de buen cubero,de gráfica de ACF)
# -(2,0,7) (a ojo de buen cubero, de gráfica de PACF)
# -(2,0,0) (arroado por el auto.arima)

# 5. ANALIZAR AUTO.ARIMA DISCUTIR DIFERENCIAS CON CANDIDATOS DEL MODELO 4
auto.arima(serie)  
# Auto.arima arroja 2 rezagos autorregresivos en P/AR y 0 rezagos de error en Q/MAn. El mejor modelo considerando 
# el AIC más bajo de los tres (-11.15) es el modelo 2,0,0

# 6. ESTIMAR MODELOS PARA CADA UNO DE LOS CANDIDATOS. ELIMIAR POR SIGNIFICANCIA (DETERMINAR NIVEL DE SIGNIFICACNIA):
m206 <- arima(serie, order=c(2,0,6), method="ML")
m207 <- arima(serie, order=c(2,0,7), method="ML")
m200 <- arima(serie, order=c(2,0,0), method="ML")

# Se está utilizando un nivel de significancia de .05 para todo el ejercicio(examen)

pm206 <- (1-pnorm(.4269/.0546))*2
pm207 <- (1-pnorm(.3210/.0553))*2
pm207 <- (1-pnorm(.3210/.0553))*2
# 7.---ESTABLECER LOS MODELOS CON AIC MÁS ALTO, Y EVALUAR LOS RESIDUALES---

pacf(m200$residuals)
# Rezago 15 a cuidar 
acf(m200$residuals)
# Ignorando primer rezago por autocorrelación con sigo misma, todos los rezagos son admisibles
# a excepción de 15 de nueva cuenta
tsdiag(m200)

Box.test(m200$residuals,lag=12,type="Ljung")
# P-value > .05 indica normalidads

jarque.bera.test(m200$residuals)
qqnorm(m200$residuals, pch=1, frame=F)
# Aparenta normalidad, las colas por el tamaño de la gráfica no se ven mal
qqline(m200$residuals, col="steelblue", lwd=1)



# 8.---BACKTESTING, UTILIZAR 3 DIFERENTES VENTANAS DE TIEMPO ---
# (DEFINIR AMPLITUD DE TIEMPO A UTILIZAR PARA EL PRONÓSTICO).

p_m206a <- forecast(m206,15)
plot(p_m206a)
lines(serie, col="red")
p_m206b <- forecast(m206,30)
plot(p_m206b)
lines(serie, col="red")
p_m206c <- forecast(m206,45)
plot(p_m206c)
lines(serie, col="red")

p_m207a <- forecast(m207,15)
plot(p_m207a)
lines(serie, col="red")
p_m207b <- forecast(m207,30)
plot(p_m207b)
lines(serie, col="red")
p_m207c <- forecast(m207,45)
plot(p_m207c)
lines(serie, col="red")

p_m200a <- forecast(m200,15)
plot(p_m200a)
lines(serie, col="red")
p_m200b <- forecast(m200,30)
plot(p_m200b)
lines(serie, col="red")
p_m200c <- forecast(m200,45)
plot(p_m200c)
lines(serie, col="red")

# 9.--- CALCULAR LA SUMA DE LOS ERRORES AL CUADRADO DEL ERROR DEL PRONÓSTICO.
# ¿QUÉ MODELO SE COMPORTA MEJOR? ¿COINCIDE CON EL MODELO CON EL AIC MÁS BAJO?
sum(p_m206c$mean - serie[(length(serie)-44):(length(serie))])^2
sum(p_m207c$mean - serie[(length(serie)-44):(length(serie))])^2
sum(p_m200c$mean - serie[(length(serie)-44):(length(serie))])^2
# El modelo 2,0,0 tiene el error cuadrado más bajo (0.02761734), coincidiendo con el AIC más bajo.

# 10. DEFINIR EL MODELO GANADOR. JUSTIFICAR RESPUESTA.
# Modelo ganador es 2,0,0 puesto que tiene AIC más bajo, y tiene el error al cuadrado más bajo de los
# demás modelos propuestos.