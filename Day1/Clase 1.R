setwd("/Users/rodrigoortiz/Dropbox/Adelante/Sesiones Guatemala/Clases en Guatemala/Sesiones");

library(readxl)
PersonasConNegocios_ENIGH2022 <- read_excel("PersonasConNegocios_ENIGH2022.xlsx")
attach(PersonasConNegocios_ENIGH2022)

# Creando variable tamaño de empresa

# El tamaño de la empresa: para esto puedes construir 
# una variable categórica con las siguientes categorías: 
# 0= No sabe
# 1= Micro empresa (1 a 9 trabajadores); 
# 2=Pequeña (10 a 99); 
# 3=Mediana (100 a 199)
# 4= Grande (+200).

# ifelse(test, yes, no)
PersonasConNegocios_ENIGH2022$tamano <- as.factor(ifelse(PersonasConNegocios_ENIGH2022$Numero_Empleados == '1 a 9', 1,
                            ifelse(PersonasConNegocios_ENIGH2022$Numero_Empleados == '10 a 99', 2, 
                                   ifelse(PersonasConNegocios_ENIGH2022$Numero_Empleados == '100 a 199', 3, 
                                          ifelse(PersonasConNegocios_ENIGH2022$Numero_Empleados == '+200',4, 0 )))))
attach(PersonasConNegocios_ENIGH2022)
summary(tamano)
table(Numero_Empleados)

# Pruebas de normalidad
# TIEMPO LABORANDO AÑOS
# B1P02B06A

PersonasConNegocios_ENIGH2022$datos_normales<-rnorm(10954)
attach(PersonasConNegocios_ENIGH2022)

plot(density(datos_normales), lwd=3, col='blue', main='Gráfico de densidad', las=1,
     xlab='', ylab='Densidad')

lines(density(B1P02B06A), lwd=3, col='red')

legend('topleft', legend=c('Normal', 'TIEMPO LABORANDO AÑOS'),
       lwd=3, col=c('blue', 'red'), bty='n')

# Gráfico qq plot
qqnorm(datos_normales, pch=20,
       main='QQplot Datos normales')
qqline(datos_normales)

qqnorm(B1P02B06A, pch=20,
       main='QQplot TIEMPO LABORANDO AÑOS')
qqline(B1P02B06A)

library(nortest) ###REALIZA 5 PRUEBAS DE NORMALIDAD###
library(moments) ###REALIZA 1 PRUEBA DE NORMALIDAD###

# Hipótesis
# H0: La muestra proviene de una distribución normal.
# H1: La muestra no proviene de una distribución normal.

# Para pruebas de normalidad siempre se plantean así las hipótesis.

# Pruebas de Normalidad del Paquete “nortest”

###Prueba de Anderson-Darling###
ad.test(B1P02B06A)
ad.test(datos_normales)

###Prueba de Cramer-von Mises###
###Es útil para pequeñas muestras y usa los momentos como criterio.###
cvm.test(B1P02B06A)
cvm.test(datos_normales)

###Pruena de Lilliefors (Kolmogorov-Smirnov)###
lillie.test(B1P02B06A)
lillie.test(datos_normales)

###Prueba de Pearson chi-square###
###basada en una distribución Ji cuadrado y que corresponde a una prueba de bondad de ajuste.###
pearson.test(B1P02B06A)
pearson.test(datos_normales)

###Prueba de Shapiro-Francia###
sf.test(B1P02B06A[1:5000])
sf.test(datos_normales[1:5000])

# Pruebas de Normalidad del Paquete “moments”
###Prueba de Agostino###
agostino.test(B1P02B06A)
agostino.test(datos_normales)
  
# Funciones incluidas en los paquetes básicos de R.
###Prueba de Shapiro-Wilk###
###Es más poderosa cuando se compara con otras pruebas de normalidad cuando la muestra es pequeña.###
shapiro.test(B1P02B06A[1:5000])
shapiro.test(datos_normales[1:5000])
