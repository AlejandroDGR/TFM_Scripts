#Librerías a usar:
library(pacman)
p_load(haven, BIFIEsurvey)


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
PISA_BIFIE <- merge(x=Datos_SPSS,y=Datos_INE[, c("CNTSCHID","RNM_Persona", "RNM_Hogar", "Gini","Pobr_Rel","P80P20")], all.x=TRUE, by="CNTSCHID")
head(PISA_BIFIE)
#Creacion variable SCH_ESCS
TEMP <- aggregate(PISA_BIFIE$ESCS, list(PISA_BIFIE$CNTSCHID), mean)
names(TEMP) <- c("CNTSCHID","SCH_ESCS")
PISA_BIFIE <- merge(PISA_BIFIE, TEMP, by=c("CNTSCHID"))

#Crear dataframe solo con los PV de lectura:
PISA_Ciencias <- PISA_BIFIE[,-c(10:29,40:49)]

#Creación objeto BIFIE
Datos_Mult_Ciencias <- BIFIE.data.jack(data = PISA_Ciencias, jktype = "RW_PISA" , wgtrep="W_FSTURWT", pvpre = paste0("PV",1:10), cdata=FALSE)
summary(Datos_Mult)

##########################################################################################

### PASO 1: Modelo nulo solo pesos 2 ###
Modelo.Nulo.Ciencias.BIFIE <- BIFIE.twolevelreg(BIFIEobj=Datos_Mult_Ciencias,dep="SCIE", formula.fixed=~1, formula.random=~1,idcluster="CNTSCHID", wgtlevel2="W_SCHGRNRABWT", se=TRUE)
summary(Modelo.Nulo.Ciencias.BIFIE)
#La ICC es de 0.1414

### Modelo nulo pesos nivel 1 y 2 ###
Modelo.Nulo.Ciencias.BIFIE2 <- BIFIE.twolevelreg(BIFIEobj=Datos_Mult_Ciencias,dep="SCIE", formula.fixed=~1, formula.random=~1,idcluster="CNTSCHID", wgtlevel1 = "W_FSTUWT", wgtlevel2="W_SCHGRNRABWT", se=TRUE)
summary(Modelo.Nulo.Ciencias.BIFIE2)
#La ICC es de 0.1414

##########################################################################################

### PASO 2: Variables de control nivel 1 ###
Modelo.1.Ciencias.BIFIE <- BIFIE.twolevelreg(BIFIEobj=Datos_Mult_Ciencias,dep="SCIE", formula.fixed=~1+IMMIG+REPEAT+ESCS, formula.random=~1,idcluster="CNTSCHID", wgtlevel2="W_SCHGRNRABWT", se=TRUE)
summary(Modelo.1.Ciencias.BIFIE)
# Todas las variables son estadísticamente significativas

##########################################################################################

### PASO 3: Introducimos variables de nivel 1: variables school-focused (ATTLNACT, RESILIENCE y MASTGOAL) ###
Modelo.2.Ciencias.BIFIE <- BIFIE.twolevelreg(BIFIEobj=Datos_Mult_Ciencias,dep="SCIE", formula.fixed=~1+IMMIG+REPEAT+ESCS+ATTLNACT+RESILIENCE+MASTGOAL, formula.random=~1,idcluster="CNTSCHID", wgtlevel2="W_SCHGRNRABWT", se=TRUE)
summary(Modelo.2.Ciencias.BIFIE)
#ATTLNACT no es estadísticamente significativa

##########################################################################################

### PASO 4: testar variacion de la pendiente para las variables de nivel 1 ###
#RESILIENCE
Modelo.3.2.Ciencias.BIFIE <- BIFIE.twolevelreg(BIFIEobj=Datos_Mult_Ciencias,dep="SCIE", formula.fixed=~1+IMMIG+REPEAT+ESCS+RESILIENCE+MASTGOAL, formula.random=~1+RESILIENCE,idcluster="CNTSCHID", wgtlevel2="W_SCHGRNRABWT", se=TRUE)
summary(Modelo.3.2.Ciencias.BIFIE)
#La variación de la pendiente no es significativa

#MASTGOAL
Modelo.3.3.Ciencias.BIFIE <- BIFIE.twolevelreg(BIFIEobj=Datos_Mult_Ciencias,dep="SCIE", formula.fixed=~1+IMMIG+REPEAT+ESCS+RESILIENCE+MASTGOAL, formula.random=~1+MASTGOAL,idcluster="CNTSCHID", wgtlevel2="W_SCHGRNRABWT", se=TRUE)
summary(Modelo.3.3.Ciencias.BIFIE)
#La variación de la pendiente no es significativa


#Las dos variables a la vez
Modelo.3.5.Ciencias.BIFIE <- BIFIE.twolevelreg(BIFIEobj=Datos_Mult_Ciencias,dep="SCIE", formula.fixed=~1+IMMIG+REPEAT+ESCS+RESILIENCE+MASTGOAL, formula.random=~1+RESILIENCE+MASTGOAL,idcluster="CNTSCHID", wgtlevel2="W_SCHGRNRABWT", se=TRUE)
summary(Modelo.3.5.Ciencias.BIFIE)
#La variación de la pendiente es al 90% significativa

#########################################################################################

### PASO 5: introducimos variable de control de nivel 2: SCHLTYPE ###

# Dejamos random RESILIENCE y MASTGOAL:
Modelo.4.3.Ciencias.BIFIE <- BIFIE.twolevelreg(BIFIEobj=Datos_Mult_Ciencias,dep="SCIE", formula.fixed=~1+IMMIG+REPEAT+ESCS+RESILIENCE+MASTGOAL+SCHLTYPE, formula.random=~1+RESILIENCE+MASTGOAL,idcluster="CNTSCHID", wgtlevel2="W_SCHGRNRABWT", se=TRUE)
summary(Modelo.4.3.Ciencias.BIFIE)
#SCHLTYPE no es estadísticamente significativo, por lo que la quitamos de los posteriores modelos.

##########################################################################################

### PASO 6A: introducimos variables de nivel 2: SCH_ESCS, RNM_Persona y Pobr_Rel ###
Modelo.5.1.Ciencias.BIFIE <- BIFIE.twolevelreg(BIFIEobj=Datos_Mult_Ciencias,dep="SCIE", formula.fixed=~1+IMMIG+REPEAT+ESCS+RESILIENCE+MASTGOAL+SCH_ESCS+RNM_Persona+Pobr_Rel, formula.random=~1+RESILIENCE+MASTGOAL,idcluster="CNTSCHID", wgtlevel2="W_SCHGRNRABWT", se=TRUE)
summary(Modelo.5.1.Ciencias.BIFIE)
#Ni RNM_Persona ni Pobr_Rel son estadísticamente significativas
#RESILIENCE lo es la 95%

##########################################################################################

### PASO 6B: introducimos variable de nivel 2: SCH_ESCS###
Modelo.5.2.Ciencias.BIFIE <- BIFIE.twolevelreg(BIFIEobj=Datos_Mult_Ciencias,dep="SCIE", formula.fixed=~1+IMMIG+REPEAT+ESCS+RESILIENCE+MASTGOAL+SCH_ESCS, formula.random=~1+RESILIENCE+MASTGOAL,idcluster="CNTSCHID", wgtlevel2="W_SCHGRNRABWT", se=TRUE)
summary(Modelo.5.2.Ciencias.BIFIE)
#SCH_ESCS es estadísticamente significativo

##########################################################################################

### PASO 6C: introducimos variable de nivel 2: RNM_Persona###
Modelo.5.3.Ciencias.BIFIE <- BIFIE.twolevelreg(BIFIEobj=Datos_Mult_Ciencias,dep="SCIE", formula.fixed=~1+IMMIG+REPEAT+ESCS+RESILIENCE+MASTGOAL+RNM_Persona, formula.random=~1+RESILIENCE+MASTGOAL,idcluster="CNTSCHID", wgtlevel2="W_SCHGRNRABWT", se=TRUE)
summary(Modelo.5.3.Ciencias.BIFIE)
#RNM_Persona es estadísticamente significativo

##########################################################################################

### PASO 6D: introducimos variable de nivel 2: Pobr_Rel###
Modelo.5.4.Ciencias.BIFIE <- BIFIE.twolevelreg(BIFIEobj=Datos_Mult_Ciencias,dep="SCIE", formula.fixed=~1+IMMIG+REPEAT+ESCS+RESILIENCE+MASTGOAL+Pobr_Rel, formula.random=~1+RESILIENCE+MASTGOAL,idcluster="CNTSCHID", wgtlevel2="W_SCHGRNRABWT", se=TRUE)
summary(Modelo.5.4.Ciencias.BIFIE)
#Pobr_Rel es estadísticamente significativo

##########################################################################################

### PASO 6E: introducimos variable de nivel 2: SCH_ESCS y RNM_Persona###
Modelo.5.5.Ciencias.BIFIE <- BIFIE.twolevelreg(BIFIEobj=Datos_Mult_Ciencias,dep="SCIE", formula.fixed=~1+IMMIG+REPEAT+ESCS+RESILIENCE+MASTGOAL+SCH_ESCS+RNM_Persona, formula.random=~1+RESILIENCE+MASTGOAL,idcluster="CNTSCHID", wgtlevel2="W_SCHGRNRABWT", se=TRUE)
#RNM_Persona no es estadísticamente significativa
#RESILIENCE es estadísticamente significativa al 95%

##########################################################################################

### PASO 6F: introducimos variable de nivel 2: SCH_ESCS y Pobr_Rel###
Modelo.5.6.Ciencias.BIFIE <- BIFIE.twolevelreg(BIFIEobj=Datos_Mult_Ciencias,dep="SCIE", formula.fixed=~1+IMMIG+REPEAT+ESCS+RESILIENCE+MASTGOAL+SCH_ESCS+Pobr_Rel, formula.random=~1+RESILIENCE+MASTGOAL,idcluster="CNTSCHID", wgtlevel2="W_SCHGRNRABWT", se=TRUE)
summary(Modelo.5.6.Ciencias.BIFIE)
#Pobr_Rel no es estadísticamente significativa
#RESILIENCE es estadísticamente significativa al 95%

##########################################################################################

### PASO 6G: introducimos variable de nivel 2: RNM_Persona y Pobr_Rel###
Modelo.5.7.Ciencias.BIFIE <- BIFIE.twolevelreg(BIFIEobj=Datos_Mult_Ciencias,dep="SCIE", formula.fixed=~1+IMMIG+REPEAT+ESCS+RESILIENCE+MASTGOAL+RNM_Persona+Pobr_Rel, formula.random=~1+RESILIENCE+MASTGOAL,idcluster="CNTSCHID", wgtlevel2="W_SCHGRNRABWT", se=TRUE)
summary(Modelo.5.7.Ciencias.BIFIE)
#Ni RNM_Persona ni Pobr_Rel son estadísticamente significativas
#RESILIENCE es estadísticamente significativa al 95%

##########################################################################################

### PASO 7A: interacciones con SCH_ESCS###
Modelo.6.1.1.Ciencias.BIFIE <- BIFIE.twolevelreg(BIFIEobj=Datos_Mult_Ciencias,dep="SCIE", formula.fixed=~1+IMMIG+REPEAT+ESCS+RESILIENCE*SCH_ESCS+MASTGOAL, formula.random=~1+RESILIENCE+MASTGOAL,idcluster="CNTSCHID", wgtlevel2="W_SCHGRNRABWT", se=TRUE)
#La interacción no es significativa

Modelo.6.1.2.Ciencias.BIFIE <- BIFIE.twolevelreg(BIFIEobj=Datos_Mult_Ciencias,dep="SCIE", formula.fixed=~1+IMMIG+REPEAT+ESCS+RESILIENCE+MASTGOAL*SCH_ESCS, formula.random=~1+RESILIENCE+MASTGOAL,idcluster="CNTSCHID", wgtlevel2="W_SCHGRNRABWT", se=TRUE)
#La interacción es estadísticamente significativa y positiva
vcov(Modelo.6.1.2.Ciencias.BIFIE)
#Graficamos la interacción:
xx <- c(-1.1668,0.9599)   #  <-- change to alter plot dims
yy <- c(502.5038,536.6375)   #  <-- change to alter plot dims
leg <- c(-1.1668,506.7705)   #  <-- change to alter legend location
x <- c(-1.1668,0.9599)   #  <-- x-coords for lines
y1 <- c(515.4234,508.1927)
y2 <- c(514.2132,522.4151)
y3 <- c(513.003,536.6375)
plot(xx,yy,type='n',font=2,font.lab=2,xlab='MASTGOAL',ylab='Rendimiento en Ciencias', main="Ciencias MASTGOAL*SCH_ESCS")
lines(x,y1,lwd=3,lty=5,col=2)
lines(x,y2,lwd=3,lty=5,col=4)
lines(x,y3,lwd=3,lty=5,col=3)
points(x,y1,col=1,pch=16)
points(x,y2,col=1,pch=16)
points(x,y3,col=1,pch=16)
legend(leg[1],leg[2],legend=c('Valor bajo SCH_ESCS','Valor medio SCH_ESCS','Valor alto SCH_ESCS'),lwd=c(3,3,3),lty=c(5,5,5),col=c(2,4,3))

##########################################################################################

### PASO 7B: interacciones con RNM_Persona###
Modelo.6.2.1.Ciencias.BIFIE <- BIFIE.twolevelreg(BIFIEobj=Datos_Mult_Ciencias,dep="SCIE", formula.fixed=~1+IMMIG+REPEAT+ESCS+RESILIENCE*RNM_Persona+MASTGOAL, formula.random=~1+RESILIENCE+MASTGOAL,idcluster="CNTSCHID", wgtlevel2="W_SCHGRNRABWT", se=TRUE)
#La interacción no es significativa

Modelo.6.2.2.Ciencias.BIFIE <- BIFIE.twolevelreg(BIFIEobj=Datos_Mult_Ciencias,dep="SCIE", formula.fixed=~1+IMMIG+REPEAT+ESCS+RESILIENCE+MASTGOAL*RNM_Persona, formula.random=~1+RESILIENCE+MASTGOAL,idcluster="CNTSCHID", wgtlevel2="W_SCHGRNRABWT", se=TRUE)
summary(Modelo.6.2.2.Ciencias.BIFIE)
#La interacción no es significativa

##########################################################################################

### PASO 7C: interacciones con Pobr_Rel###
Modelo.6.3.1.Ciencias.BIFIE <- BIFIE.twolevelreg(BIFIEobj=Datos_Mult_Ciencias,dep="SCIE", formula.fixed=~1+IMMIG+REPEAT+ESCS+RESILIENCE*Pobr_Rel+MASTGOAL, formula.random=~1+RESILIENCE+MASTGOAL,idcluster="CNTSCHID", wgtlevel2="W_SCHGRNRABWT", se=TRUE)
#La interacción no es significativa

Modelo.6.3.2.Ciencias.BIFIE <- BIFIE.twolevelreg(BIFIEobj=Datos_Mult_Ciencias,dep="SCIE", formula.fixed=~1+IMMIG+REPEAT+ESCS+RESILIENCE+MASTGOAL*Pobr_Rel, formula.random=~1+RESILIENCE+MASTGOAL,idcluster="CNTSCHID", wgtlevel2="W_SCHGRNRABWT", se=TRUE)
summary(Modelo.6.3.2.Ciencias.BIFIE)
#La interacción no es significativa