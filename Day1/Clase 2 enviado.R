setwd("/Users/rodrigoortiz/Dropbox/Adelante/Sesiones Guatemala/Clases en Guatemala/Sesiones");

library(readxl)
PersonasConNegocios_ENIGH2022 <- read_excel("PersonasConNegocios_ENIGH2022.xlsx")
attach(PersonasConNegocios_ENIGH2022)

# Comparación de las medias de los datos de una muestra
# Existen principalmente dos técnicas que se utilizan para comparar 
# la media de una muestra con una media estándar conocida. 
# Estas dos técnicas son:

# One Sample T-test
# One-Sample Wilcoxon Test

# One Sample T-test
# La prueba T para una muestra se usa para probar la 
# diferencia estadística entre la media de una muestra 
# y un valor conocido o supuesto/hipotetizado de la media en la población.

# promedio 
mean(B1P02B06A)
# 11.23873

# Ho: mu = 5
t.test(B1P02B06A, mu=5, alternative='two.sided',conf.level = 0.95)

# Ho: mu < 5
t.test(B1P02B06A, mu=5, alternative='greater',conf.level = 0.95)

# Ho: mu > 5
t.test(B1P02B06A, mu=5, alternative='less',conf.level = 0.95)


# Ho: mu = 5
t.test(B1P02B06A, mu=5, alternative='two.sided',conf.level = 0.99)

# Ho: mu < 5
t.test(B1P02B06A, mu=5, alternative='greater',conf.level = 0.99)

# Ho: mu > 5
t.test(B1P02B06A, mu=5, alternative='less',conf.level = 0.99)

# One-Sample Wilcoxon Test
# La prueba de rango con signo de Wilcoxon de una 
# muestra es una alternativa no paramétrica a una 
# prueba t de una muestra cuando no se puede suponer 
# que los datos se distribuyen normalmente. Se utiliza 
# para determinar si la mediana de la muestra es igual a 
# un valor estándar conocido, es decir, un valor teórico.

# Ho: mu = 5
wilcox.test(B1P02B06A, mu=5, alternative='two.sided',conf.level = 0.99)

# Ho: mu < 5
wilcox.test(B1P02B06A, mu=5, alternative='greater',conf.level = 0.99)

# Ho: mu > 5
wilcox.test(B1P02B06A, mu=5, alternative='less',conf.level = 0.99)

# Comparación de las medias de muestras pareadas
# Existen principalmente dos técnicas que se utilizan 
# para comparar las medias de muestras pareadas. 

# Estas dos técnicas son:

# Paired sample T-test
# Paired Samples Wilcoxon Test

# Paired sample T-test
# Este es un procedimiento estadístico que se utiliza 
# para determinar si la diferencia media entre dos 
# conjuntos de observaciones es cero. En una 
# prueba t de muestras pareadas, cada sujeto se mide 
# dos veces, lo que da como resultado pares de observaciones.

# promedio Años trabajando
mean(B1P02B06A)
# 11.23873

# promedio  EDAD
mean(B1P00A03)
# 43.87101

t.test(B1P02B06A, B1P00A03, alternative='two.sided',conf.level = 0.95)
t.test(B1P02B06A, B1P00A03, alternative='greater',conf.level = 0.95)
t.test(B1P02B06A, B1P00A03, alternative='less',conf.level = 0.95)

# Paired Samples Wilcoxon Test
# La prueba de Wilcoxon para muestras pareadas 
# es una alternativa no paramétrica a la prueba t 
# pareada utilizada para comparar datos pareados. 
# Se utiliza cuando los datos no se distribuyen normalmente.

# promedio Años trabajando
mean(B1P02B06A)
# 11.23873

# promedio  EDAD
mean(B1P00A03)
# 43.87101

wilcox.test(B1P02B06A, B1P00A03, alternative='two.sided',conf.level = 0.99)
wilcox.test(B1P02B06A, B1P00A03, alternative='greater',conf.level = 0.99)
wilcox.test(B1P02B06A, B1P00A03, alternative='less',conf.level = 0.99)
