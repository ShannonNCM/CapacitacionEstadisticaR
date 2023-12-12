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

# Comparación de las medias de más de dos grupos

# Existen principalmente dos técnicas que se utilizan 
# para comparar la media de una muestra con una media 
# estándar conocida. Estas dos técnicas son:

# Analysis of Variance (ANOVA)
# One way ANOVA
# Two way ANOVA
# MANOVA Test

# One way ANOVA
# El análisis de varianza unidireccional (ANOVA),
# también conocido como ANOVA de un factor, es una 
# extensión de la prueba t de dos muestras independientes 
# para comparar medias en una situación en la que 
# hay más de dos grupos. En ANOVA unidireccional, los 
# datos se organizan en varios grupos basados en una 
# única variable de agrupación.

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

# anova de B1P02B08: SUELDO O SALARIO MENSUAL
# con tamano
anova_1 <- aov(B1P02B08 ~ factor(tamano))
anova_1
print(summary(anova_1))

library(psych)
describeBy(B1P02B08,tamano)

# Tukey's Test
tukey.test_1<-TukeyHSD(anova_1)
tukey.test_1

plot(tukey.test_1)

# ifelse(test, yes, no)
PersonasConNegocios_ENIGH2022$trabajos <- as.factor(ifelse(PersonasConNegocios_ENIGH2022$B1P02B01 == 'UN SOLO TRABAJO', 1,
                                                         ifelse(PersonasConNegocios_ENIGH2022$B1P02B01 == 'DOS TRABAJOS', 2, 3)))
attach(PersonasConNegocios_ENIGH2022)
table(B1P02B01)
table(trabajos)

# anova de B1P02B08: SUELDO O SALARIO MENSUAL
# con tamano
anova_2 <- aov(B1P02B08 ~ factor(trabajos))
anova_2
print(summary(anova_2))

library(psych)
describeBy(B1P02B08,trabajos)

# Tukey's Test
tukey.test_2<-TukeyHSD(anova_2)
tukey.test_2

plot(tukey.test_2)
# Two way ANOVA
# La prueba ANOVA de dos vías se usa para evaluar 
# simultáneamente el efecto de dos variables de 
# agrupación (A y B) en una variable de respuesta. 
# Toma en consideración dos grupos categóricos.

# anova de B1P02B06A: Ingresos por venta de bienes o servicios 2016
# con innovacion y tamano juntos
anova_3 <- aov(B1P02B06A ~ factor(trabajos)*factor(tamano))
print(summary(anova_3))

# Tukey's Test
tukey.test_3<-TukeyHSD(anova_3)
tukey.test_3
plot(tukey.test_3)
