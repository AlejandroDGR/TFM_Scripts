#Librerías a usar:
library(pacman)
p_load(haven, BIFIEsurvey, dplyr)


#Lectura de datos:
Datos_SPSS <- as.data.frame(read_sav("C:/Users/alex9_000/Desktop/Universidad/Master/TFM/Datos/3.Multinivel_PISA/Multinivel_PISA.sav"))
Datos_INE <- read.csv("C:/Users/alex9_000/Desktop/Universidad/Master/TFM/Datos/3.Multinivel_PISA/Callejero/Tabla_Final_INE.csv", sep=";")
#Para solucionar problema de que el indice de Gini, Pobr_Rel y P80/P20 estan expresados con , en el .csv
Datos_INE$Gini <- sub(",", ".", Datos_INE$Gini, fixed = TRUE)
Datos_INE$Gini <- as.numeric(Datos_INE$Gini)
Datos_INE$Pobr_Rel <- sub(",", ".", Datos_INE$Pobr_Rel, fixed = TRUE)
Datos_INE$Pobr_Rel <- as.numeric(Datos_INE$Pobr_Rel)
Datos_INE$P80P20 <- sub(",", ".", Datos_INE$P80P20, fixed = TRUE)
Datos_INE$P80P20 <- as.numeric(Datos_INE$P80P20)
#Union de ficheros en un solo dataset
PISA_BIFIE <- merge(x=Datos_SPSS,y=Datos_INE[, c("CNTSCHID","COMUNIDAD","RNM_Persona", "RNM_Hogar", "Gini","Pobr_Rel","P80P20")], all.x=TRUE, by="CNTSCHID")
head(PISA_BIFIE)
#Creacion variable SCH_ESCS
TEMP <- aggregate(PISA_BIFIE$ESCS, list(PISA_BIFIE$CNTSCHID), mean)
names(TEMP) <- c("CNTSCHID","SCH_ESCS")
PISA_BIFIE <- merge(PISA_BIFIE, TEMP, by=c("CNTSCHID"))

#Perdidos:
sum(is.na(PISA_BIFIE)) #Cantidad de datos perdidos totales
nrow(PISA_BIFIE)*ncol(PISA_BIFIE)-sum(is.na(PISA_BIFIE)) #Cantidad de datos no perdidos totales
mean(is.na(PISA_BIFIE)) #5,496797% de porcentaje de datos perdidos en todo el dataset
apply(is.na(PISA_BIFIE_BIFIE), 2, sum) #NÃºmero de perdidos por variable
apply(is.na(PISA_BIFIE), 2, mean) #Porcentaje de perdidos por variable
table(apply(is.na(PISA_BIFIE), 1, sum)) #Perdidos por observaciones
N_Observaciones_Perdidos <- nrow(PISA_BIFIE)-15090 #NÃºmero de observaciones con perdidos
N_Observaciones_Perdidos/length(PISA_BIFIE$CNTSCHID) #ReducciÃ³n en un 59,12521% de las observaciones al hacer listwise
nrow_original <- nrow(PISA_BIFIE)
PISA_BIFIE <- filter(PISA_BIFIE, COMUNIDAD != "NAVARRA")
PISA_BIFIE <- filter(PISA_BIFIE, COMUNIDAD != "PAÍS VASCO")
nrow_2 <- nrow(PISA_BIFIE)
1-(nrow_2/nrow_original) #13,03174%
PISA_BIFIE <- na.exclude(PISA_BIFIE)
nrow_3 <- nrow(PISA_BIFIE)
1-(nrow_3/nrow_original)

#Crear dataframe solo con los PV de lectura:
PISA_Lectura <- PISA_BIFIE[,-c(20:49)]
names(PISA_Lectura)

#Creación objeto BIFIE
Datos_Mult_Lectura <- BIFIE.data.jack(data = PISA_Lectura, jktype = "RW_PISA" , wgtrep="W_FSTURWT", pvpre = paste0("PV",1:10), cdata=FALSE)
summary(Datos_Mult_Lectura)

##########################################################################################

### PASO 1: Modelo nulo solo pesos 2 ###
Modelo.Nulo.Lectura.BIFIE <- BIFIE.twolevelreg(BIFIEobj=Datos_Mult_Lectura, dep="READ", formula.fixed=~1, formula.random=~1,idcluster="CNTSCHID", wgtlevel2="W_SCHGRNRABWT", se=TRUE)
summary(Modelo.Nulo.Lectura.BIFIE)
#La ICC es de 0.1754
Datos_Mult_Lectura$varnames

### Modelo nulo pesos nivel 1 y 2 ###
Modelo.Nulo.Lectura.BIFIE <- BIFIE.twolevelreg(BIFIEobj=Datos_Mult_Lectura,dep="READ", formula.fixed=~1, formula.random=~1,idcluster="CNTSCHID", wgtlevel1 = "W_FSTUWT",wgtlevel2="W_SCHGRNRABWT", se=TRUE)
summary(Modelo.Nulo.Lectura.BIFIE2)
#La ICC es de 0.1754

##########################################################################################

### PASO 2: Variables de control nivel 1 ###
Modelo.1.Lectura.BIFIE <- BIFIE.twolevelreg(BIFIEobj=Datos_Mult_Lectura,dep="READ", formula.fixed=~1+SEX+IMMIG+REPEAT+ESCS, formula.random=~1,idcluster="CNTSCHID", wgtlevel2="W_SCHGRNRABWT", se=TRUE)
summary(Modelo.1.Lectura.BIFIE)
# Todas las variables son estadísticamente significativas


##########################################################################################

### PASO 3: Introducimos variables de nivel 1: variables school-focused (ATTLNACT, RESILIENCE y MASTGOAL) ###
Modelo.2.Lectura.BIFIE <- BIFIE.twolevelreg(BIFIEobj=Datos_Mult_Lectura,dep="READ", formula.fixed=~1+SEX+IMMIG+REPEAT+ESCS+ATTLNACT+RESILIENCE+MASTGOAL, formula.random=~1,idcluster="CNTSCHID", wgtlevel2="W_SCHGRNRABWT", se=TRUE)
summary(Modelo.2.Lectura.BIFIE)
# Todas las variables son estadísticamente significativas
# ICC condicional: 0.1214

##########################################################################################

### PASO 4: testar variacion de la pendiente para las variables de nivel 1 ###
#ATTLNACT
Modelo.3.1.Lectura.BIFIE <- BIFIE.twolevelreg(BIFIEobj=Datos_Mult_Lectura,dep="READ", formula.fixed=~1+SEX+IMMIG+REPEAT+ESCS+ATTLNACT+RESILIENCE+MASTGOAL, formula.random=~1+ATTLNACT,idcluster="CNTSCHID", wgtlevel2="W_SCHGRNRABWT", se=TRUE)
summary(Modelo.3.1.Lectura.BIFIE)
#La variación de la pendiente es significativa

#RESILIENCE
Modelo.3.2.Lectura.BIFIE <- BIFIE.twolevelreg(BIFIEobj=Datos_Mult_Lectura,dep="READ", formula.fixed=~1+SEX+IMMIG+REPEAT+ESCS+ATTLNACT+RESILIENCE+MASTGOAL, formula.random=~1+RESILIENCE,idcluster="CNTSCHID", wgtlevel2="W_SCHGRNRABWT", se=TRUE)
summary(Modelo.3.2.Lectura.BIFIE)
#La variación de la pendiente no es significativa

#MASTGOAL
Modelo.3.3.Lectura.BIFIE <- BIFIE.twolevelreg(BIFIEobj=Datos_Mult_Lectura,dep="READ", formula.fixed=~1+SEX+IMMIG+REPEAT+ESCS+ATTLNACT+RESILIENCE+MASTGOAL, formula.random=~1+MASTGOAL,idcluster="CNTSCHID", wgtlevel2="W_SCHGRNRABWT", se=TRUE)
summary(Modelo.3.3.Lectura.BIFIE)
#La variación de la pendiente no es significativa

#Las tres variables a la vez
Modelo.3.5.Lectura.BIFIE <- BIFIE.twolevelreg(BIFIEobj=Datos_Mult_Lectura,dep="READ", formula.fixed=~1+SEX+IMMIG+REPEAT+ESCS+ATTLNACT+RESILIENCE+MASTGOAL, formula.random=~1+ATTLNACT+RESILIENCE+MASTGOAL,idcluster="CNTSCHID", wgtlevel2="W_SCHGRNRABWT", se=TRUE)
summary(Modelo.3.5.Lectura.BIFIE)
#La variación de la pendiente no es significativa

#ATTLNACT y RESILIENCE
Modelo.3.6.Lectura.BIFIE <- BIFIE.twolevelreg(BIFIEobj=Datos_Mult_Lectura,dep="READ", formula.fixed=~1+SEX+IMMIG+REPEAT+ESCS+ATTLNACT+RESILIENCE+MASTGOAL, formula.random=~1+ATTLNACT+RESILIENCE,idcluster="CNTSCHID", wgtlevel2="W_SCHGRNRABWT", se=TRUE)
summary(Modelo.3.6.Lectura.BIFIE)
#La variación de la pendiente no es significativa

#ATTLNACT y MASTGOAL
Modelo.3.7.Lectura.BIFIE <- BIFIE.twolevelreg(BIFIEobj=Datos_Mult_Lectura,dep="READ", formula.fixed=~1+SEX+IMMIG+REPEAT+ESCS+ATTLNACT+RESILIENCE+MASTGOAL, formula.random=~1+ATTLNACT+MASTGOAL,idcluster="CNTSCHID", wgtlevel2="W_SCHGRNRABWT", se=TRUE)
summary(Modelo.3.7.Lectura.BIFIE)
#La variación de la pendiente no es significativa

#RESILIENCE Y MASTGOAL
Modelo.3.8.Lectura.BIFIE <- BIFIE.twolevelreg(BIFIEobj=Datos_Mult_Lectura,dep="READ", formula.fixed=~1+SEX+IMMIG+REPEAT+ESCS+ATTLNACT+RESILIENCE+MASTGOAL, formula.random=~1+RESILIENCE+MASTGOAL,idcluster="CNTSCHID", wgtlevel2="W_SCHGRNRABWT", se=TRUE)
summary(Modelo.3.8.Lectura.BIFIE)
#La variación de la pendiente no es significativa


##########################################################################################

### PASO 5: introducimos variable de control de nivel 2: SCHLTYPE ###

# Dejamos random ATTLNACT:
Modelo.4.1.Lectura.BIFIE <- BIFIE.twolevelreg(BIFIEobj=Datos_Mult_Lectura,dep="READ", formula.fixed=~1+SEX+IMMIG+REPEAT+ESCS+ATTLNACT+RESILIENCE+MASTGOAL+SCHLTYPE, formula.random=~1+ATTLNACT,idcluster="CNTSCHID", wgtlevel2="W_SCHGRNRABWT", se=TRUE)
summary(Modelo.4.1.Lectura.BIFIE)
#SCHLTYPE no es estadísticamente significativo, por lo que la quitamos de los posteriores modelos.

##########################################################################################

### PASO 6A: introducimos variables de nivel 2: SCH_ESCS, RNM_Persona y Pobr_Rel ###
# Dejamos random ATTLNACT:
Modelo.5.1.Lectura.BIFIE <- BIFIE.twolevelreg(BIFIEobj=Datos_Mult_Lectura,dep="READ", formula.fixed=~1+SEX+IMMIG+REPEAT+ESCS+ATTLNACT+RESILIENCE+MASTGOAL+SCH_ESCS+RNM_Persona+Pobr_Rel, formula.random=~1+ATTLNACT,idcluster="CNTSCHID", wgtlevel2="W_SCHGRNRABWT", se=TRUE)
summary(Modelo.5.1.Lectura.BIFIE)
#El efecto random no es significativo
#SCH_ESCS sí es estadísticamente significativo, pero RNM_Persona y Pobr_Rel no lo son
#Lo mismo ocurre con RESILIENCE

##########################################################################################

### PASO 6B: introducimos variable de nivel 2: SCH_ESCS###
Modelo.5.2.Lectura.BIFIE <- BIFIE.twolevelreg(BIFIEobj=Datos_Mult_Lectura,dep="READ", formula.fixed=~1+SEX+IMMIG+REPEAT+ESCS+ATTLNACT+RESILIENCE+MASTGOAL+SCH_ESCS, formula.random=~1+ATTLNACT,idcluster="CNTSCHID", wgtlevel2="W_SCHGRNRABWT", se=TRUE)
summary(Modelo.5.2.Lectura.BIFIE)
#El efecto ramdon es estadísticamente significativo al 90%
#SCH_ESCS es estadísticamente significativo
#No lo es RESILIENCE

##########################################################################################

### PASO 6C: introducimos variable de nivel 2: RNM_Persona###
Modelo.5.3.Lectura.BIFIE <- BIFIE.twolevelreg(BIFIEobj=Datos_Mult_Lectura,dep="READ", formula.fixed=~1+SEX+IMMIG+REPEAT+ESCS+ATTLNACT+RESILIENCE+MASTGOAL+RNM_Persona, formula.random=~1+ATTLNACT,idcluster="CNTSCHID", wgtlevel2="W_SCHGRNRABWT", se=TRUE)
summary(Modelo.5.3.Lectura.BIFIE)
#El efecto ramdon es estadísticamente significativo al 95%
#RNM_Persona es estadísticamente significativo
#RESILIENCE lo es también

##########################################################################################

### PASO 6D: introducimos variable de nivel 2: Pobr_Rel###
Modelo.5.4.Lectura.BIFIE <- BIFIE.twolevelreg(BIFIEobj=Datos_Mult_Lectura,dep="READ", formula.fixed=~1+SEX+IMMIG+REPEAT+ESCS+ATTLNACT+RESILIENCE+MASTGOAL+Pobr_Rel, formula.random=~1+ATTLNACT,idcluster="CNTSCHID", wgtlevel2="W_SCHGRNRABWT", se=TRUE)
summary(Modelo.5.4.Lectura.BIFIE)
#El efecto ramdon es estadísticamente significativo al 95%
#Pobr_Rel es estadísticamente significativo al 95%
#RESILIENCE es estadísticamente significativo

##########################################################################################

### PASO 6E: introducimos variable de nivel 2: SCH_ESCS y RNM_Persona###
Modelo.5.5.Lectura.BIFIE <- BIFIE.twolevelreg(BIFIEobj=Datos_Mult_Lectura,dep="READ", formula.fixed=~1+SEX+IMMIG+REPEAT+ESCS+ATTLNACT+RESILIENCE+MASTGOAL+SCH_ESCS+RNM_Persona, formula.random=~1+ATTLNACT,idcluster="CNTSCHID", wgtlevel2="W_SCHGRNRABWT", se=TRUE)
summary(Modelo.5.5.Lectura.BIFIE)
#El efecto random no es estadísticamente significativo
#Ni RESILIENCE ni RNM_Persona son estadísticamente significativas

##########################################################################################

### PASO 6F: introducimos variable de nivel 2: SCH_ESCS y Pobr_Rel###
Modelo.5.6.Lectura.BIFIE <- BIFIE.twolevelreg(BIFIEobj=Datos_Mult_Lectura,dep="READ", formula.fixed=~1+SEX+IMMIG+REPEAT+ESCS+ATTLNACT+RESILIENCE+MASTGOAL+SCH_ESCS+Pobr_Rel, formula.random=~1+ATTLNACT,idcluster="CNTSCHID", wgtlevel2="W_SCHGRNRABWT", se=TRUE)
summary(Modelo.5.6.Lectura.BIFIE)
#El efecto random no es estadísticamente significativo
#Ni RESILIENCE ni Pobr_Rel son estadísticamente significativas

##########################################################################################

### PASO 6G: introducimos variable de nivel 2: RNM_Persona y Pobr_Rel###
Modelo.5.7.Lectura.BIFIE <- BIFIE.twolevelreg(BIFIEobj=Datos_Mult_Lectura,dep="READ", formula.fixed=~1+SEX+IMMIG+REPEAT+ESCS+ATTLNACT+RESILIENCE+MASTGOAL+RNM_Persona+Pobr_Rel, formula.random=~1+ATTLNACT,idcluster="CNTSCHID", wgtlevel2="W_SCHGRNRABWT", se=TRUE)
summary(Modelo.5.7.Lectura.BIFIE)
#El efecto random es estadísticamente significativo al 95%
#RESILIENCE y RNM_Persona son estadísticamente significativas
#Ni Pobr_Rel ni MASTGOAL son estadísticamente significativa

##########################################################################################

### PASO 7A: interacciones con SCH_ESCS###
Modelo.6.1.1.Lectura.BIFIE <- BIFIE.twolevelreg(BIFIEobj=Datos_Mult_Lectura,dep="READ", formula.fixed=~1+SEX+IMMIG+REPEAT+ESCS+ATTLNACT*SCH_ESCS+RESILIENCE+MASTGOAL, formula.random=~1+ATTLNACT,idcluster="CNTSCHID", wgtlevel2="W_SCHGRNRABWT", se=TRUE)
summary(Modelo.6.1.1.Lectura.BIFIE)
#La interacción es significativa y negativa
vcov(Modelo.6.1.1.Lectura.BIFIE)
#Gráfico interacción:
xx <- c(-0.798,1.165)   #  <-- change to alter plot dims
yy <- c(489.1461,515.7997)   #  <-- change to alter plot dims
leg <- c(-0.798,492.4778)   #  <-- change to alter legend location
x <- c(-0.798,1.165)   #  <-- x-coords for lines
y1 <- c(493.5884,500.153)
y2 <- c(504.694,507.6884)
y3 <- c(515.7997,515.2237)
plot(xx,yy,type='n',font=2,font.lab=2,xlab='ATTLNACT',ylab='Rendimiento en Lectura', main="Lectura ATTLNACT*SCH_ESCS")
lines(x,y1,lwd=3,lty=5,col=2)
lines(x,y2,lwd=3,lty=5,col=4)
lines(x,y3,lwd=3,lty=5,col=3)
points(x,y1,col=1,pch=16)
points(x,y2,col=1,pch=16)
points(x,y3,col=1,pch=16)
legend(leg[1],leg[2],legend=c('Valor bajo SCH_ESCS','Valor medio SCH_ESCS','Valor alto SCH_ESCS'),lwd=c(3,3,3),lty=c(5,5,5),col=c(2,4,3))

Modelo.6.1.2.Lectura.BIFIE <- BIFIE.twolevelreg(BIFIEobj=Datos_Mult_Lectura,dep="READ", formula.fixed=~1+SEX+IMMIG+REPEAT+ESCS+ATTLNACT+RESILIENCE*SCH_ESCS+MASTGOAL, formula.random=~1+ATTLNACT,idcluster="CNTSCHID", wgtlevel2="W_SCHGRNRABWT", se=TRUE)
summary(Modelo.6.1.2.Lectura.BIFIE)
#La interacción no es significativa

Modelo.6.1.3.Lectura.BIFIE <- BIFIE.twolevelreg(BIFIEobj=Datos_Mult_Lectura,dep="READ", formula.fixed=~1+SEX+IMMIG+REPEAT+ESCS+ATTLNACT+RESILIENCE+MASTGOAL*SCH_ESCS, formula.random=~1+ATTLNACT,idcluster="CNTSCHID", wgtlevel2="W_SCHGRNRABWT", se=TRUE)
summary(Modelo.6.1.3.Lectura.BIFIE)
#La interacción es significativa y positiva
vcov(Modelo.6.1.3.Lectura.BIFIE)
#Graficamos la interacción:
xx <- c(-1.1668,0.9599)   #  <-- change to alter plot dims
yy <- c(485.9213,523.5539)   #  <-- change to alter plot dims
leg <- c(-1.1668,490.6254)   #  <-- change to alter legend location
x <- c(-1.1668,0.9599)   #  <-- x-coords for lines
y1 <- c(492.1934,500.1355)
y2 <- c(498.4431,511.8447)
y3 <- c(504.6928,523.5539)
plot(xx,yy,type='n',font=2,font.lab=2,xlab='MASTGOAL',ylab='Rendimiento en Lectura', main="Lectura MASTGOAL*SCH_ESCS")
lines(x,y1,lwd=3,lty=5,col=2)
lines(x,y2,lwd=3,lty=5,col=4)
lines(x,y3,lwd=3,lty=5,col=3)
points(x,y1,col=1,pch=16)
points(x,y2,col=1,pch=16)
points(x,y3,col=1,pch=16)
legend(leg[1],leg[2],legend=c('Valor bajo SCH_ESCS','Valor medio SCH_ESCS','Valor alto SCH_ESCS'),lwd=c(3,3,3),lty=c(5,5,5),col=c(2,4,3))

##########################################################################################

### PASO 7B: interacciones con RNM_Persona###
Modelo.6.2.1.Lectura.BIFIE <- BIFIE.twolevelreg(BIFIEobj=Datos_Mult_Lectura,dep="READ", formula.fixed=~1+SEX+IMMIG+REPEAT+ESCS+ATTLNACT*RNM_Persona+RESILIENCE+MASTGOAL, formula.random=~1+ATTLNACT,idcluster="CNTSCHID", wgtlevel2="W_SCHGRNRABWT", se=TRUE)
summary(Modelo.6.2.1.Lectura.BIFIE)
#La interacción no es significativa

Modelo.6.2.2.Lectura.BIFIE <- BIFIE.twolevelreg(BIFIEobj=Datos_Mult_Lectura,dep="READ", formula.fixed=~1+SEX+IMMIG+REPEAT+ESCS+ATTLNACT+RESILIENCE*RNM_Persona+MASTGOAL, formula.random=~1+ATTLNACT,idcluster="CNTSCHID", wgtlevel2="W_SCHGRNRABWT", se=TRUE)
summary(Modelo.6.2.2.Lectura.BIFIE)
#La interacción no es significativa

Modelo.6.2.3.Lectura.BIFIE <- BIFIE.twolevelreg(BIFIEobj=Datos_Mult_Lectura,dep="READ", formula.fixed=~1+SEX+IMMIG+REPEAT+ESCS+ATTLNACT+RESILIENCE+MASTGOAL*RNM_Persona, formula.random=~1+ATTLNACT,idcluster="CNTSCHID", wgtlevel2="W_SCHGRNRABWT", se=TRUE)
summary(Modelo.6.2.3.Lectura.BIFIE)
#La interacción es significativa y positiva
vcov(Modelo.6.2.3.Lectura.BIFIE)
#Graficamos la interacción:
xx <- c(-1.1668,0.9599)   #  <-- change to alter plot dims
yy <- c(490.7403,517.4556)   #  <-- change to alter plot dims
leg <- c(-1.1668,494.0797)   #  <-- change to alter legend location
x <- c(-1.1668,0.9599)   #  <-- x-coords for lines
y1 <- c(495.1929,502.6115)
y2 <- c(498.3603,510.0335)
y3 <- c(501.5276,517.4556)
plot(xx,yy,type='n',font=2,font.lab=2,xlab='MASTGOAL',ylab='Rendimiento en Lectura', main="Lectura MASTGOAL*RNM_PERSONA")
lines(x,y1,lwd=3,lty=5,col=2)
lines(x,y2,lwd=3,lty=5,col=4)
lines(x,y3,lwd=3,lty=5,col=3)
points(x,y1,col=1,pch=16)
points(x,y2,col=1,pch=16)
points(x,y3,col=1,pch=16)
legend(leg[1],leg[2],legend=c('Valor bajo RNM_PERSONA','Valor medio RNM_PERSONA','Valor alto RNM_PERSONA'),lwd=c(3,3,3),lty=c(5,5,5),col=c(2,4,3))

##########################################################################################

### PASO 7C: interacciones con Pobr_Rel###
Modelo.6.3.1.Lectura.BIFIE <- BIFIE.twolevelreg(BIFIEobj=Datos_Mult_Lectura,dep="READ", formula.fixed=~1+SEX+IMMIG+REPEAT+ESCS+ATTLNACT*Pobr_Rel+RESILIENCE+MASTGOAL, formula.random=~1+ATTLNACT,idcluster="CNTSCHID", wgtlevel2="W_SCHGRNRABWT", se=TRUE)
summary(Modelo.6.3.1.Lectura.BIFIE)
#La interacción no es estadísticamente significativa

Modelo.6.3.2.Lectura.BIFIE <- BIFIE.twolevelreg(BIFIEobj=Datos_Mult_Lectura,dep="READ", formula.fixed=~1+SEX+IMMIG+REPEAT+ESCS+ATTLNACT+RESILIENCE*Pobr_Rel+MASTGOAL, formula.random=~1+ATTLNACT,idcluster="CNTSCHID", wgtlevel2="W_SCHGRNRABWT", se=TRUE)
summary(Modelo.6.3.2.Lectura.BIFIE)
#La interacción no es estadísticamente significativa

Modelo.6.3.3.Lectura.BIFIE <- BIFIE.twolevelreg(BIFIEobj=Datos_Mult_Lectura,dep="READ", formula.fixed=~1+SEX+IMMIG+REPEAT+ESCS+ATTLNACT+RESILIENCE+MASTGOAL*Pobr_Rel, formula.random=~1+ATTLNACT,idcluster="CNTSCHID", wgtlevel2="W_SCHGRNRABWT", se=TRUE)
summary(Modelo.6.3.3.Lectura.BIFIE)
#La interacción no es estadísticamente significativa
