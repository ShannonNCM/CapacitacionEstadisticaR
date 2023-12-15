setwd("/Users/rodrigoortiz/Dropbox/Profesor cursos/UAI/Econometría Financiera/Clases Rodrigo/II T 2023/Clase 6");

# cargar librerias
library(quantmod)
library(tseries)
library(lmtest)
library(forecast)
library(lubridate)
#library(rugarch)

# Descargar datos
mdate="1990-03-26"
CPE_prices=getSymbols('CPE', from=mdate, auto.assign = F)[,4]
head(CPE_prices)
plot(CPE_prices)

# otro tipo de grafico
chartSeries(CPE_prices)
chartSeries(CPE_prices, subset ="last 3 months")

# Serie en logaritmo
CPE_prices_log=log(CPE_prices)

#####################################
# Paso 1 Identificacion del modelo
#####################################

# a) Serie estacionaria
adf.test(CPE_prices_log, alternative = 'stationary')
# H0 la serie no es estacionaria (raiz unitaria)
# H1 la serie es estacionaria

#p-value = 0.07949

# Como p value = 0.07949 > 1% entonces no se rechaza H0,
#es decir, la serie es no es estacionaria

# Calculamos la primera diferencia
seriedif=diff(CPE_prices_log)
adf.test(seriedif[2:(length(seriedif)-1)], alternative = 'stationary')
# H0 la series no es estacionaria (raiz unitaria)
# H1 la series es estacionaria

#p-value = 0.01

# Como p value = 0.01 < 1% entonces se rechaza H0,
#es decir, la serie es estacionaria

# b) Identificar el orden
# Autocorrelacion y autocorrelacion parcial
acf(CPE_prices_log) # Para MA
pacf(CPE_prices_log) # Para AR
# Seria un modelo AR(1) al log del precio

acf(seriedif[2:(length(seriedif)-1),]) # Para MA
pacf(seriedif[2:(length(seriedif)-1),]) # Para AR
# Seria un modelo ARIMA(1,1,1)

#####################################
# Paso 2 Estimacion del modelo
#####################################

# Modelo AR(1)
modelo_1=arima(CPE_prices_log, order=c(1,0,0))
modelo_1

#####################################
# Paso 3 Evaluacion del modelo
#####################################
tsdiag(modelo_1)
Box.test(residuals(modelo_1),type="Ljung-Box")
# H0: Las autocorrelaciones son independientes
# H1: Las autocorrelaciones no son independientes

# p-value = 4.824e-05

# como pvalue = 4.824e-05 <1% entonces se rechaza H0, 
# por tanto no hay ruido blanco, MODELO INADECUADO

#####################################
# Paso 4 Realización de pronósticos con el modelo
#####################################

modelo_1_pred <-forecast::forecast(modelo_1,h=1500, level=c(99.5))
modelo_1_pred
plot(modelo_1_pred)

# Modelo ARIMA(1,1,1)
modelo_2=arima(CPE_prices_log, order=c(1,1,1))
modelo_2
tsdiag(modelo_2)

Box.test(residuals(modelo_2),type="Ljung-Box")
# H0: Las autocorrelaciones son independientes
# H1: Las autocorrelaciones no son independientes

# p-value = 0.9977

# como pvalue = 0.9977 > 10 % entonces no se rechaza H0, 
# por tanto hay ruido blanco, MODELO ADECUADO

modelo_2_pred <-forecast::forecast(modelo_1,h=10000, level=c(99.5))
modelo_2_pred
plot(modelo_2_pred)

# Modelo ARIMA(2,1,1)
modelo_3=arima(CPE_prices_log, order=c(2,1,1))
modelo_3
tsdiag(modelo_3)
Box.test(residuals(modelo_3),type="Ljung-Box")

# Modelo ARIMA(3,1,1)
modelo_4=arima(CPE_prices_log, order=c(3,1,1))
modelo_4
tsdiag(modelo_4)
Box.test(residuals(modelo_4),type="Ljung-Box")

# Modelo ARIMA(4,1,2)
modelo_5=arima(CPE_prices_log, order=c(4,1,2))
modelo_5
tsdiag(modelo_5)
Box.test(residuals(modelo_5),type="Ljung-Box")

#####################################
#####################################
# Mas de una diferencia
#####################################
#####################################

# Descargar datos
# Importar los datos desde archivo excel entregado

# Declarar la serie de tiempo de precios
dim(Precio_Exportciones_Petroleo_Mexicano)
precios=Precio_Exportciones_Petroleo_Mexicano[,1]
arimar.ts=ts(precios, start = c(2013,1), frequency=12)
arimar.ts
plot(arimar.ts)

# Serie en logaritmo
precios_log=log(arimar.ts)

#####################################
# Paso 1 Identificacion del modelo
#####################################

# a) Serie estacionaria
adf.test(precios_log, alternative = 'stationary')
#p-value = 0.7714
# No es estacionaria

# Calculamos la primera diferencia
seriedif_m=diff(precios_log)
adf.test(seriedif_m, alternative = 'stationary')
#p-value = 0.167
# No es estacionaria

# Calculamos la segunda diferencia
seriedif_m2=diff(precios_log, differences=2)
adf.test(seriedif_m2, alternative = 'stationary')
#p-value = 0.01
# Es estacionaria

# b) Identificar el orden
# Autocorrelacion y autocorrelacion parcial
acf(seriedif_m2) # Para MA
pacf(seriedif_m2) # Para AR
# Seria un modelo ARIMA(1,2,1)

#####################################
# Paso 2 Estimacion del modelo
#####################################

# Modelo ARIMA(1,2,1)
modelo_6=arima(precios_log, order=c(1,2,1))
modelo_6

#####################################
# Paso 3 Evaluacion del modelo
#####################################
tsdiag(modelo_6)
Box.test(residuals(modelo_6),type="Ljung-Box")
# p-value = 0.7973
# No se rechaza H0, es decir si son ruido blanco los errores

#####################################
# Paso 4 Realización de pronósticos con el modelo
#####################################

modelo_6_pred <-forecast::forecast(modelo_6,h=15, level=c(99.5))
modelo_6_pred
plot(modelo_6_pred)

exp(3.164413)
