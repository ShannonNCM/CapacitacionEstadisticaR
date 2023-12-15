setwd("/Users/rodrigoortiz/Dropbox/Adelante/Sesiones Guatemala/Clases en Guatemala/Sesiones");

library(readxl)
PersonasConNegocios_ENIGH2022 <- read_excel("PersonasConNegocios_ENIGH2022.xlsx")
attach(PersonasConNegocios_ENIGH2022)

# B1P00A03	EDAD
# B1P00A04A	DIA DE NACIMIENTO
# B1P00A04B	MES DE NACIMIENTO
# B1P02B06A	TIEMPO LABORANDO AÑOS
# B1P02B06B	TIEMPO LABORANDO MESES
# B1P02B08	SUELDO O SALARIO MENSUAL
# B1P02B28	HORAS TRABAJADAS

datos<-cbind(B1P00A03,B1P00A04A,B1P00A04B,B1P02B06A,B1P02B06B,B1P02B08,B1P02B28)
head(datos)
dim(datos)

datos_acp<-na.omit(datos)
head(datos_acp)
dim(datos_acp)

# Matriz de correlaciones
cor(datos_acp)

## Analisis de componentes principales
#Datos transformados:
datos_acp.c <- scale(datos_acp,center=TRUE,scale=TRUE)
head(datos_acp)
head(datos_acp.c)

# Seleccion de las variables
X <- datos_acp.c
head(X)

# Matriz de varianzas y covarianzas:
S <- var(X)
S
traza_S <- sum(diag(S)) 
traza_S

# Matriz de correlaciones:
R <- cor(X) 
R

# Analisis de Componentes Principales 
# Datos Centrados y Reducidos

# Matriz X′X, en Rp (proyectamos individuos):
X.rp <- eigen(var(X))
X1.val <- round(X.rp$values,4) 
X1.val
sum(X1.val)
X1.vec <- round(X.rp$vectors,4) 
X1.vec

# Importancia de las componentes:
Pro.Var <- round((X1.val/traza_S)*100,4) 
Pro.Var
Pro.Var.Cum <- round(cumsum(Pro.Var),4)
Pro.Var.Cum

# Grafico para la seleccion del numero de componentes:
plot(X1.val,type="b",
     main="Grafico para la seleccion del numero de componentes", 
     xlab="Numero de Componentes",ylab="Varianzas", pch=20,col="red",lwd=2)

# Proyeccion de los individuos:
pci1 <- X%*%X1.vec[,1]
pci2 <- X%*%X1.vec[,2]
pci14 <- X%*%X1.vec[,1:4]

# Primera y segunda componente:
plot(pci14[,1],pci14[,2], type="p",col="red",
     pch=20,cex=2, xlim=c(-7,4),ylim=c(-17,3), 
     main="Proyeccion de los Individuos", 
     xlab="Primera componente (19.78%)",
     ylab="Segunda componente (17.31%)")
text(pci14[,1],pci14[,2],labels=rownames(X),pos=1,cex=0.5)

abline(h=0,v=0)

# Correlaciones entre las variables y las componentes principales:

CorVarPC <- diag(sqrt(X1.val))%*%X1.vec # en el caso de var. estandarizadas 
rownames(CorVarPC) <- colnames(X)
colnames(CorVarPC) <- c("PC1","PC2","PC3","PC4","PC5", "PC6","PC7")
CorVarPC

# Cırculo de correlaciones:

t=seq(0,2*pi,l=100)
plot(cos(t),sin(t),type="l",xlim=c(-1,1),ylim=c(-1,1), xlab="Primera componente (33%)",ylab="Segunda componente (17%)", main="Circulo de Correlaciones")

abline(h=0,v=0,col="blue")

abline(h=seq(-1,1,0.25),v=seq(-1,1,0.25),col="blue",lty=3)

arrows(0,0,CorVarPC[1:7,1],CorVarPC[1:7,2],length=0.1)

text(CorVarPC[,1],CorVarPC[,2],labels=colnames(X),pos=1,cex=0.8)

