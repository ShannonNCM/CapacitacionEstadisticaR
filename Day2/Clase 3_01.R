#setwd("/Users/rodrigoortiz/Dropbox/Adelante/Sesiones Guatemala/Clases en Guatemala/Sesiones");

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

# ifelse(test, yes, no)
PersonasConNegocios_ENIGH2022$nivel_educacional <- as.numeric(ifelse(PersonasConNegocios_ENIGH2022$B1P00B07A == 'Ninguno', 1,
                                                         ifelse(PersonasConNegocios_ENIGH2022$B1P00B07A == 'Primaria', 2, 
                                                                ifelse(PersonasConNegocios_ENIGH2022$B1P00B07A == 'Basico', 3, 
                                                                       ifelse(PersonasConNegocios_ENIGH2022$B1P00B07A == 'Diversificado',4, 5 )))))
attach(PersonasConNegocios_ENIGH2022)
table(nivel_educacional)
table(B1P00B07A)

# Modelo de regresion simple con nivel en 5 categorias
modelo_1_salario_nivel <- lm(B1P02B08 ~ nivel_educacional)
anova(modelo_1_salario_nivel)
summary(modelo_1_salario_nivel)

PersonasConNegocios_ENIGH2022$nivel_educacional_5 <- as.numeric(ifelse(PersonasConNegocios_ENIGH2022$nivel_educacional == 5, 1,0))
attach(PersonasConNegocios_ENIGH2022)
table(nivel_educacional_5)
table(B1P00B07A)

# Modelo de regresion simple solo con nivel 5
modelo_1_salario_nivel_5 <- lm(B1P02B08 ~ nivel_educacional_5)
anova(modelo_1_salario_nivel_5)
summary(modelo_1_salario_nivel_5)

# Modelo de regresión múltiple con los 5 niveles como variables independientes
PersonasConNegocios_ENIGH2022$nivel_educacional_4 <- as.numeric(ifelse(PersonasConNegocios_ENIGH2022$nivel_educacional == 4, 1,0))
attach(PersonasConNegocios_ENIGH2022)
table(nivel_educacional_4)
table(B1P00B07A)

PersonasConNegocios_ENIGH2022$nivel_educacional_3 <- as.numeric(ifelse(PersonasConNegocios_ENIGH2022$nivel_educacional == 3, 1,0))
attach(PersonasConNegocios_ENIGH2022)
table(nivel_educacional_3)
table(B1P00B07A)

PersonasConNegocios_ENIGH2022$nivel_educacional_2 <- as.numeric(ifelse(PersonasConNegocios_ENIGH2022$nivel_educacional == 2, 1,0))
attach(PersonasConNegocios_ENIGH2022)
table(nivel_educacional_2)
table(B1P00B07A)

PersonasConNegocios_ENIGH2022$nivel_educacional_1 <- as.numeric(ifelse(PersonasConNegocios_ENIGH2022$nivel_educacional == 1, 1,0))
attach(PersonasConNegocios_ENIGH2022)
table(nivel_educacional_1)
table(B1P00B07A)

# Modelo de regresion multiple con los 5 niveles
modelo_1_salario_nivel_todos <- lm(B1P02B08 ~ nivel_educacional_1+nivel_educacional_2+nivel_educacional_3+nivel_educacional_4+nivel_educacional_5)
summary(modelo_1_salario_nivel_todos)

# Modelo de regresion multiple con los 5 niveles
modelo_1_salario_nivel_todos_ok <- lm(B1P02B08 ~ nivel_educacional_2+nivel_educacional_3+nivel_educacional_4+nivel_educacional_5)
summary(modelo_1_salario_nivel_todos_ok)
# Salario = 1200.03 + 340.88*N2+654.50*N3+1989.58*N4+3741.83*N5
# Si N2=N3=N4=N5=0... osea eres N1 tu salario será 1200.03
# Si N2=1, osea eres un N2, tu salario será 1200.03 + 340.88*1 
# Si N5=1, osea eres un N5, tu salario será 1200.03 + 3741.83*1

# Salario con nivel educacional
summary(B1P02B08)

PersonasConNegocios_ENIGH2022$salario_alto <- as.numeric(ifelse(PersonasConNegocios_ENIGH2022$B1P02B08 > 1440 , 1,0))
attach(PersonasConNegocios_ENIGH2022)
table(salario_alto)

# Regresión MCO
reg_mco = lm(salario_alto~ nivel_educacional_2+nivel_educacional_3+nivel_educacional_4+nivel_educacional_5)
summary(reg_mco)
precict_mco=predict(reg_mco, type='response')
summary(precict_mco)
estimaciones1=fitted.values(reg_mco) 
plot(estimaciones1, main="¿Datos estimados fuera de [0, 1]?", xlab="Individuo", ylab="Estimación de salario", col="blue", lwd=2)
abline(a=1,b=0,col="red",lwd=2) # línea y=1
abline(a=0,b=0,col="red",lwd=2) # línea y=0
library(mfx)
# Regresgión logit
reg_logit = glm(salario_alto~ nivel_educacional_2+nivel_educacional_3+nivel_educacional_4+nivel_educacional_5, data=PersonasConNegocios_ENIGH2022, family=binomial(link='logit'))
summary(reg_logit)
precict_logit=predict(reg_logit, type='response')
summary(precict_logit)
estimaciones2 = reg_logit$fitted.values
plot(estimaciones2,main="¿Datos estimados fuera de [0, 1]?", xlab="Individuo", ylab="Estimación de Partipación", col="blue",lwd=2)
abline(a=1,b=0,col="red",lwd=2) # línea y=1
abline(a=0,b=0,col="red",lwd=2) # línea y=0
marginal_logit<-logitmfx(formula = salario_alto~ nivel_educacional_2+nivel_educacional_3+nivel_educacional_4+nivel_educacional_5,  data = PersonasConNegocios_ENIGH2022)
print(marginal_logit)

# Regresión probit
reg_probit = glm(salario_alto~ nivel_educacional_2+nivel_educacional_3+nivel_educacional_4+nivel_educacional_5, data=PersonasConNegocios_ENIGH2022, family=binomial(link='probit'))
summary(reg_probit)
precict_probit=predict(reg_probit, type='response')
summary(precict_probit)
estimaciones3 = reg_probit$fitted.values
plot(estimaciones3,main="¿Datos estimados fuera de [0, 1]?", xlab="Individuo", ylab="Estimación de Partipación", col="blue",lwd=2)
abline(a=1,b=0,col="red",lwd=2) # línea y=1
abline(a=0,b=0,col="red",lwd=2) # línea y=0
marginal_probit<- probitmfx(formula = salario_alto~ nivel_educacional_2+nivel_educacional_3+nivel_educacional_4+nivel_educacional_5,  data = PersonasConNegocios_ENIGH2022)
print(marginal_probit)

library(stargazer)
library(memisc)
library(caret)
stargazer(reg_mco, reg_logit, reg_probit, type="text", df=FALSE)

#stargazer(reg_mco, reg_logit, reg_probit, df=FALSE)

# Matriz de confusión mco
reg_mco_predicho <- predict(reg_mco,PersonasConNegocios_ENIGH2022,type='response')
reg_mco_predicho[is.na(reg_mco_predicho)] <- 0;

PersonasConNegocios_ENIGH2022$mco_predict<-0;
PersonasConNegocios_ENIGH2022$mco_predict <- reg_mco_predicho;

PersonasConNegocios_ENIGH2022$predicho_correctamente_mco<-0;
PersonasConNegocios_ENIGH2022$predicho_correctamente_mco <- ifelse(PersonasConNegocios_ENIGH2022$mco_predict>=0.5, 1, 0)

CM_cmo<-confusionMatrix(as.factor(PersonasConNegocios_ENIGH2022$salario_alto), as.factor(PersonasConNegocios_ENIGH2022$predicho_correctamente_mco))
print(CM_cmo)

# Matriz de confusión logit
glm_logit <- predict.glm(reg_logit,PersonasConNegocios_ENIGH2022,type='response')
glm_logit[is.na(glm_logit)] <- 0;

PersonasConNegocios_ENIGH2022$logit_predict<-0;
PersonasConNegocios_ENIGH2022$logit_predict <- glm_logit;

PersonasConNegocios_ENIGH2022$predicho_correctamente_logit<-0;
PersonasConNegocios_ENIGH2022$predicho_correctamente_logit <- ifelse(PersonasConNegocios_ENIGH2022$logit_predict>=0.5, 1, 0)

CM_logit<-confusionMatrix(as.factor(PersonasConNegocios_ENIGH2022$salario_alto), as.factor(PersonasConNegocios_ENIGH2022$predicho_correctamente_logit))
print(CM_logit)

# Matriz de confusión probit
glm_prob <- predict.glm(reg_probit,PersonasConNegocios_ENIGH2022,type='response')
glm_prob[is.na(glm_prob)] <- 0;

PersonasConNegocios_ENIGH2022$probit_predict<-0;
PersonasConNegocios_ENIGH2022$probit_predict <- glm_prob;

PersonasConNegocios_ENIGH2022$predicho_correctamente_probit<-0;
PersonasConNegocios_ENIGH2022$predicho_correctamente_probit <- ifelse(PersonasConNegocios_ENIGH2022$probit_predict>=0.5, 1, 0)

CM_probit<-confusionMatrix(as.factor(PersonasConNegocios_ENIGH2022$salario_alto), as.factor(PersonasConNegocios_ENIGH2022$predicho_correctamente_probit))
print(CM_probit)

# Sexo (mujeres) versus Edad, Salario, Tamaño de empresa 