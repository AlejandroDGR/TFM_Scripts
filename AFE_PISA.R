#Librerías a utilizar:
library(foreign)
library(haven)
library(psych)
library(GPArotation)
library(polycor) #Para las matriz policórica
library(ggcorrplot)
library(coefficientalpha)
library(corrr)


###################################################################################
#Fiabilidad (alfa y omega)

#Lectura Datos SPSS:
Datos_fiabilidad <- as.data.frame(read_sav("C:/Users/alex9_000/Desktop/Universidad/Master/TFM/Datos/ESP_3F.sav"))
Datos_fiabilidad <- Datos_fiabilidad[,1:11]
Datos_fiabilidad <- na.omit(Datos_fiabilidad)
#Divisi�n del dataset en tres: uno por cada variable
Datos_fiabilidad_ATTLNACT <- Datos_fiabilidad[,1:3]
Datos_fiabilidad_MASTGOAL <- Datos_fiabilidad[,4:6]
Datos_fiabilidad_RESILIENCE <- Datos_fiabilidad[,7:11]

#Alfa:
alfa.est <- alpha(Datos_fiabilidad, varphi = 0.1)
plot(alfa.est, type="d") #Usamos un varphi de 0.1
alfa.total <- alpha(Datos_fiabilidad, varphi = 0.1, se= FALSE, test=TRUE, complete=TRUE)
alfa.ATTLNACT <- alpha(Datos_fiabilidad_ATTLNACT, varphi = 0.1, se= FALSE, test=TRUE, complete=TRUE)
alfa.MASTGOAL <- alpha(Datos_fiabilidad_MASTGOAL, varphi = 0.1, se= FALSE, test=TRUE, complete=TRUE)
alfa.RESILIENCE <- alpha(Datos_fiabilidad_RESILIENCE, varphi = 0.1, se= FALSE, test=TRUE, complete=TRUE)

#Omega:
omega.est <- omega(Datos_fiabilidad, varphi = 0.1)
plot(omega.est, type="d") #Usamos un varphi de 0.1
omega.total <- omega(Datos_fiabilidad, varphi = 0.1, se= FALSE, test=TRUE, complete=TRUE)
omega.ATTLNACT <- omega(Datos_fiabilidad_ATTLNACT, varphi = 0.1, se= FALSE, test=FALSE, complete=TRUE)
omega.MASTGOAL <- omega(Datos_fiabilidad_MASTGOAL, varphi = 0.1, se= FALSE, test=FALSE, complete=TRUE)
omega.RESILIENCE <- omega(Datos_fiabilidad_RESILIENCE, varphi = 0.1, se= FALSE, test=FALSE, complete=TRUE)

#Correlaci�n item-test:
co <- correlate(Datos_fiabilidad)
inter_item <- colMeans( co[, 2:11], na.rm = TRUE )
inter_item
mean(inter_item) #Correlaci�n media entre los items: 0.2362628
Datos_fiabilidad$score <- rowMeans(Datos_fiabilidad)
item_total <- Datos_fiabilidad %>% correlate() %>% focus(score)
mean(item_total$score) #Correlaci�n item-total: 0.5486906

####################################################################################
#AFE

#Lecturas Datos SPSS
Datos_AFE <- read.spss("C:/Users/alex9_000/Desktop/Universidad/Master/TFM/Datos/1.AFE_PISA/AFE_PISA.sav", to.data.frame = TRUE)
Datos_AFE <- Datos_AFE[,1:11]

#Test de Mardia (normalidad multivariante):
Datos_AFE <- as.data.frame(read_sav("C:/Users/alex9_000/Desktop/Universidad/Master/TFM/Datos/1.AFE_PISA/AFE_PISA.sav"))
skew(Datos_AFE, na.rm = TRUE, type = 3)
kurtosi(Datos_AFE, na.rm = TRUE, type = 3)
mardia(Datos_AFE, na.rm = TRUE)
ks.test(Datos_AFE,pnorm)
#No existe normalidad multivariante: utilizaremos un m�todo robusto de ULS

#Calcular matriz policórica
policorica <- hetcor(Datos_AFE)$correlations
ggcorrplot(policorica,type="lower",hc.order = T)

#Verificar que la matriz sea factorizable
#1. Prueba de Bartlett
p_esf <- cortest.bartlett(policorica)
p_esf$p.value
#El resultado del p-valor: 3.975117e-79 es muy pequeño, por lo que podemos rechazar H0, que indica que las variables no están correlacionadas
#2.KMO:
KMO(policorica)
#El resultado general es 0.81, por lo que es un valor aceptable para continuar con el AFE
#3. Determinante matriz:
det(policorica)
#El determinante de la matriz es bastante bajo: 0.003616462

#Determinar el número de factores a extraer:
#1. Gráfico de sedimentación con regla de Kaiser:
scree(policorica)
#Sugiere la extracción de 3 factores
#2. Análisis paralelo:
fa.parallel(policorica, n.obs=500, fa="fa", fm="uls")
#Sugiere la extracción de 3 factores

#Extracción de factores:
AFE_oblimin <- fa(policorica, nfactors=3, n.obs=nrow(Datos_AFE), rotate="oblimin", fm="uls", alpha=0.05)
AFE_oblimin
fa.diagram(AFE_oblimin)
AFE_varimax <- fa(policorica, nfactors=3, n.obs=nrow(Datos_AFE), rotate="varimax", fm="uls", alpha=0.05)
AFE_varimax
fa.diagram(AFE_varimax)
# El modelo con tres factores es correcto.
# Se cumplen los tres factores esperados:
## El primer factor explica la variable RESILIENCE
## El segundo factor explica la variable ATTLNACT
## El tercer factor explica la variable MASTGOAL
#Índices de bondad del ajuste más o menos aceptables: RMSR = 0.03, TLI = 0.932 y RMSEA = 0.083

