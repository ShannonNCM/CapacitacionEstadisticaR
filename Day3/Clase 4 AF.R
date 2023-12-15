setwd("/Users/rodrigoortiz/Dropbox/Adelante/Sesiones Guatemala/Clases en Guatemala/Sesiones");

library(readxl)
PersonasConNegocios_ENIGH2022 <- read_excel("PersonasConNegocios_ENIGH2022.xlsx")
attach(PersonasConNegocios_ENIGH2022)

# B1P00A03	EDAD
# B1P00A04A	DIA DE NACIMIENTO
# B1P00A04B	MES DE NACIMIENTO
# B1P02B06A	TIEMPO LABORANDO AÑOS
# B1P02B06B	TIEMPO LABORANDO MESES
# B1P02B24	GANANCIA NO AGROPECUARIA
# B1P02B25	GANANCIA AGROPECUARIA
# B1P02B28	HORAS TRABAJADAS

# B1P00A09A	DSICAPACIDAD
# B1P00A09B	DISCAPACIDAD
# B1P00A09C	DISCAPACIDAD
# B1P00A09D	DISCAPACIDAD
# B1P00A09E	DISCAPACIDAD
# B1P00A09F	DISCAPACIDAD

PersonasConNegocios_ENIGH2022$DISCAP_1 <- as.numeric(ifelse(PersonasConNegocios_ENIGH2022$B1P00A09A == 'Con dificultad', 1,0))
attach(PersonasConNegocios_ENIGH2022)

PersonasConNegocios_ENIGH2022$DISCAP_2 <- as.numeric(ifelse(PersonasConNegocios_ENIGH2022$B1P00A09B == 'Con dificultad', 1,0))
attach(PersonasConNegocios_ENIGH2022)

PersonasConNegocios_ENIGH2022$DISCAP_3 <- as.numeric(ifelse(PersonasConNegocios_ENIGH2022$B1P00A09C == 'Con dificultad', 1,0))
attach(PersonasConNegocios_ENIGH2022)

PersonasConNegocios_ENIGH2022$DISCAP_4 <- as.numeric(ifelse(PersonasConNegocios_ENIGH2022$B1P00A09D == 'Con dificultad', 1,0))
attach(PersonasConNegocios_ENIGH2022)

PersonasConNegocios_ENIGH2022$DISCAP_5 <- as.numeric(ifelse(PersonasConNegocios_ENIGH2022$B1P00A09E == 'Con dificultad', 1,0))
attach(PersonasConNegocios_ENIGH2022)

PersonasConNegocios_ENIGH2022$DISCAP_6 <- as.numeric(ifelse(PersonasConNegocios_ENIGH2022$B1P00A09F == 'Con dificultad', 1,0))
attach(PersonasConNegocios_ENIGH2022)

PersonasConNegocios_ENIGH2022$DISCAPACIDAD<- DISCAP_1+DISCAP_2+DISCAP_3+DISCAP_4+DISCAP_5+DISCAP_6
attach(PersonasConNegocios_ENIGH2022)

###########################
# B1P00B07A	NIVEL MAS ALTO APROBADO
###########################

# ifelse(test, yes, no)
PersonasConNegocios_ENIGH2022$nivel_educacional <- as.numeric(ifelse(PersonasConNegocios_ENIGH2022$B1P00B07A == 'Ninguno', 1,
                                                                     ifelse(PersonasConNegocios_ENIGH2022$B1P00B07A == 'Primaria', 2, 
                                                                            ifelse(PersonasConNegocios_ENIGH2022$B1P00B07A == 'Basico', 3, 
                                                                                   ifelse(PersonasConNegocios_ENIGH2022$B1P00B07A == 'Diversificado',4, 5 )))))
attach(PersonasConNegocios_ENIGH2022)
table(nivel_educacional)
table(B1P00B07A)

###########################
# Creando variable tamaño de empresa
###########################

# El tamaño de la empresa: para esto puedes construir 
# una variable categórica con las siguientes categorías: 
# 0= No sabe
# 1= Micro empresa (1 a 9 trabajadores); 
# 2=Pequeña (10 a 99); 
# 3=Mediana (100 a 199)
# 4= Grande (+200).

# ifelse(test, yes, no)
PersonasConNegocios_ENIGH2022$tamano <- as.numeric(ifelse(PersonasConNegocios_ENIGH2022$Numero_Empleados == '1 a 9', 1,
                                                          ifelse(PersonasConNegocios_ENIGH2022$Numero_Empleados == '10 a 99', 2, 
                                                                 ifelse(PersonasConNegocios_ENIGH2022$Numero_Empleados == '100 a 199', 3, 
                                                                        ifelse(PersonasConNegocios_ENIGH2022$Numero_Empleados == '+200',4, 0 )))))
attach(PersonasConNegocios_ENIGH2022)

library(psych)
library(polycor)
library(ggcorrplot)

############################
# Analisis factorial
############################
# El Análisis Factorial (AF) es un método multivariante que 
# pretende expresar p variables observables como una combinación 
# lineal de m variables hipotéticas o latentes, denominadas factores.

datos_factorial<-cbind(B1P00A03,B1P00A04A,B1P00A04B,B1P02B06A,B1P02B06B,B1P02B24,B1P02B25,B1P02B28,DISCAP_1,DISCAP_2,DISCAP_3,DISCAP_4,DISCAP_5,DISCAP_6,nivel_educacional,tamano)
head(datos_factorial)

mat_cor <- cor(datos_factorial) #matriz de correlación policorican
ggcorrplot(mat_cor,type="lower",hc.order = T)

# La prueba de Kaiser-Meyer-Olkin (KMO) es una medida de qué tan 
# adecuados son sus datos para el análisis factorial . 
# La prueba mide la adecuación del muestreo para cada variable en el modelo 
# y para el modelo completo. La estadística es una medida de la proporción 
# de varianza entre variables que podrían ser varianza común.

# Como referencia, Kaiser puso los siguientes valores en los resultados:

# 0.00 a 0.49 inaceptable.
# 0.50 a 0.59 miserable.
# 0,60 a 0,69 mediocre.
# 0.70 a 0.79 medio.
# 0,80 a 0,89 meritorio.
# 0.90 a 1.00 maravilloso.

KMO(mat_cor)

# Grafico para la seleccion del numero de factores
scree(mat_cor)

############################
# Modelo factorial con 4 factores, componentes principales
modelo_varimax<-fa(mat_cor,nfactors = 4,rotate = "varimax",
                   fa="minres")
modelo_varimax
fa.diagram(modelo_varimax)
print(modelo_varimax$loadings,cut=0)

# Calculo de factores
pesos<-modelo_varimax$weights
head(pesos)
factores_nuevos <- datos_factorial%*%pesos
factores_nuevos

# Modelo factorial con 3 factores, componentes principales
modelo_varimax<-fa(mat_cor,nfactors = 3,rotate = "varimax",
                   fa="minres")
modelo_varimax
fa.diagram(modelo_varimax)
print(modelo_varimax$loadings,cut=0)

# Calculo de factores
pesos<-modelo_varimax$weights
head(pesos)
factores_nuevos <- datos_factorial%*%pesos
factores_nuevos

############################
# Modelo factorial con 4 factores, maxima verosilimitud
modelo_varimax_2<-factanal(datos_factorial, factors = 4 ,rotation = "varimax")
modelo_varimax_2
print(modelo_varimax_2$loadings,cut=0)
fa.diagram(modelo_varimax_2$loadings)

# Calculo de factores
pesos_2<-modelo_varimax_2$scores
factores_nuevos_2 <- datos_factorial%*%pesos
factores_nuevos_2
