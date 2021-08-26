#Librerías a usar:
library(haven)
library(lavaan)
library(psych)
library(semPlot)
library(dplyr)
library(semTools)

#Datos a usar:
Datos_AFC <- as.data.frame(read_sav("C:/Users/alex9_000/Desktop/Universidad/Master/TFM/Datos/2.AFC_PISA/AFC_PISA.sav"))
Datos_AFC <- Datos_AFC[,1:11]


#Test de Mardia (normalidad multivariante):
skew(Datos_AFC, na.rm = TRUE, type = 3)
kurtosi(Datos_AFC, na.rm = TRUE, type = 3)
mardia(Datos_AFC, na.rm = TRUE)
ks.test(Datos_AFC,pnorm)
#No existe normalidad multivariante: utilizaremos un método robusto de ULS

#Especificación del modelo:
modelo <- ' ATTLNACT =~ ST036Q05TA + ST036Q06TA + ST036Q08TA   
            MASTGOAL =~ ST208Q01HA + ST208Q02HA + ST208Q04HA
            RESILIENCE =~ ST188Q01HA + ST188Q02HA + ST188Q03HA + ST188Q06HA + ST188Q07HA'

#AFC 1: factores ortogonales
fit <- cfa(modelo, Datos_AFC, estimator="ULS", ordered=TRUE, orthogonal = TRUE)
summary(fit, fit.measures=TRUE, standardized=TRUE)
#Índices de bondad del ajuste: CFI = 0.766; TLI = 0.708; RMSEA = 0.195; SRMR = 0.159
semPaths(fit, intercepts = FALSE,edge.label.cex=1.5, optimizeLatRes = TRUE, groups = "lat",pastel = TRUE, exoVar = FALSE, sizeInt=5,edge.color ="black",esize = 6, label.prop=2,sizeLat = 6,"std", layout="circle2")

#AFC 2: factores correlacionados
fit2 <- cfa(modelo, Datos_AFC, estimator="ULS", ordered=TRUE, orthogonal = FALSE)
summary(fit2, fit.measures=TRUE, standardized=TRUE)
#Índices de bondad del ajuste: CFI = 0.987; TLI = 0.982; RMSEA = 0.048; SRMR = 0.039


#Índices de modificación:
modindices(fit2, sort. = T)

#Modelo 2:
modelo2 <- ' ATTLNACT =~ ST036Q05TA + ST036Q06TA + ST036Q08TA   
            MASTGOAL =~ ST208Q01HA + ST208Q02HA + ST208Q04HA
            RESILIENCE =~ ST188Q01HA + ST188Q02HA + ST188Q03HA + ST188Q06HA + ST188Q07HA
            ATTLNACT ~~   MASTGOAL'

#AFC 2:
fit3 <- cfa(modelo2, Datos_AFC, estimator="ULS", ordered=TRUE, orthogonal=TRUE)
summary(fit3, fit.measures=TRUE, standardized=TRUE)
#Índices de bondad del ajuste mejoran con respecto al primero: CFI = 0.860; TLI = 0.821; RMSEA = 0.152; SRMR = 0.123

#AFC 3: un solo factor
modelo1factor <- ' Factor =~ ST036Q05TA + ST036Q06TA + ST036Q08TA + ST208Q01HA + ST208Q02HA + ST208Q04HA + ST188Q01HA + ST188Q02HA + ST188Q03HA + ST188Q06HA + ST188Q07HA'
fit4 <- cfa(modelo1factor, Datos_AFC, estimator="ULS", ordered=TRUE, orthogonal=FALSE)
summary(fit4, fit.measures=TRUE, standardized=TRUE)
#Malos índices de bondad del ajuste: CFI = 0.718; TLI = 0.648; RMSEA = 0.213; SRMR = 0.174

#AFE2factores:
fa.poly(Datos_AFC,2, cor="poly") #Indica dos factores: ATTLNACT y MASTGOAL forman el primero
modelo2factores <- ' ATTLNACT =~ ST036Q05TA + ST036Q06TA + ST036Q08TA + ST208Q01HA + ST208Q02HA + ST208Q04HA
            RESILIENCE =~ ST188Q01HA + ST188Q02HA + ST188Q03HA + ST188Q06HA + ST188Q07HA'
fit5 <- cfa(modelo2factores, Datos_AFC, estimator="ULS", ordered=TRUE, orthogonal=TRUE)
summary(fit5, fit.measures=TRUE, standardized=TRUE)
#Malos índices de bondad del ajuste: CFI = 0.747; TLI = 0.684; RMSEA = 0.202; SRMR = 0.165
fit6 <- cfa(modelo2factores, Datos_AFC, estimator="ULS", ordered=TRUE, orthogonal=FALSE)
summary(fit6, fit.measures=TRUE, standardized=TRUE)
#Malos índices de bondad del ajuste: CFI = 0.869; TLI = 0.833; RMSEA = 0.147; SRMR = 0.119
