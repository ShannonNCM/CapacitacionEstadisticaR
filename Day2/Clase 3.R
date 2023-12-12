setwd("/Users/rodrigoortiz/Dropbox/Adelante/Sesiones Guatemala/Clases en Guatemala/Sesiones");

library(readxl)
PersonasConNegocios_ENIGH2022 <- read_excel("PersonasConNegocios_ENIGH2022.xlsx")
attach(PersonasConNegocios_ENIGH2022)

# B1P02B08	SUELDO O SALARIO MENSUAL
# B1P02B06A	TIEMPO LABORANDO AÑOS
# Variable independiente: B1P02B06A	TIEMPO LABORANDO AÑOS
# Variable depedendiente: B1P02B08	SUELDO O SALARIO MENSUAL

#Scatterplot
plot(B1P02B06A,B1P02B08, xlab='TIEMPO LABORANDO AÑOS', ylab='SUELDO O SALARIO MENSUAL', main="Grafica de puntos: SALARIO vs TIEMPO")

# Modelo de regresion simple
modelo_1_salario <- lm(B1P02B08 ~ B1P02B06A)
anova(modelo_1_salario)
summary(modelo_1_salario)

# Ecuación
# Salario = 1699.882 + 32.510* Tiempo
# Tiempo = 0, entonces el salario es 1699.882
# Tiempo es 10 años, 1699.882 + 32.510* 10

# B1P00B07A	NIVEL MAS ALTO APROBADO