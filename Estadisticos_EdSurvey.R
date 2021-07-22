#Librerías
library(pacman)
p_load(haven, EdSurvey)

#Lectura de datos PISA:
downloadPISA(
  "C:/Users/alex9_000/Desktop/Universidad/Master/TFM/Datos/3.Multinivel_PISA",
  years = c(2018),
  database = c("INT"),
  cache = FALSE,
  verbose = TRUE)
PISA <- readPISA("C:/Users/alex9_000/Desktop/Universidad/Master/TFM/Datos/3.Multinivel_PISA/PISA/2018", database=c("INT"), countries = c("ESP"), cognitive =  c("none"), forceReread = FALSE, verbose = TRUE)
searchSDF(string="schltype", data=PISA) #Buscador de nombres de variables
levelsSDF(varnames = "schltype", data=PISA) #Para conocer los niveles de una variable categórica
Datos_PISA1 <- getData(data=PISA,
                      varnames=c("cntschid", "cntstuid",
                                 "st004d01t","repeatgrade","immig","escs",
                                 "attlnact","resilience","mastgoal",
                                 "schltype",
                                 "read","scie","scie","glcm",
                                 "w_schgrnrabwt","w_fstuwt"),
                      omittedLevels=TRUE,
                      addAttributes=TRUE)

#Modificamos variables que lo requieren:
Datos_PISA1$Sex <- ifelse(Datos_PISA1$st004d01t == 2, 0, 1)
Datos_PISA1$Repeat <- ifelse(Datos_PISA1$repeatgrade == "REPEATED A  GRADE", 1, 0)
Datos_PISA1$Immig[Datos_PISA1$immig == "NATIVE"] <- 0
Datos_PISA1$Immig[Datos_PISA1$immig == "SECOND-GENERATION"] <- 1
Datos_PISA1$Immig[Datos_PISA1$immig == "FIRST-GENERATION"] <- 2
Datos_PISA1$Schltype[Datos_PISA1$schltype == "PUBLIC"] <- 0
Datos_PISA1$Schltype[Datos_PISA1$schltype == "PRIVATE GOVERNMENT-DEPENDENT"] <- 1
Datos_PISA1$Schltype[Datos_PISA1$schltype == "PRIVATE INDEPENDENT"] <- 2

#Creamos variable sch_escs
TEMP <- aggregate(Datos_PISA1$escs, list(Datos_PISA1$cntschid), mean)
names(TEMP) <- c("cntschid","sch_escs")
Datos_PISA1 <- merge(Datos_PISA1, TEMP, by=c("cntschid"))

#Union con datos INE
Datos_INE <- read.csv("C:/Users/alex9_000/Desktop/Universidad/Master/TFM/Datos/3.Multinivel_PISA/Callejero/Tabla_Final_INE.csv", sep=";")
#Para solucionar problema de que el indice de Gini, Pobr_Rel y P80/P20 estan expresados con , en el .csv
Datos_INE$Gini <- sub(",", ".", Datos_INE$Gini, fixed = TRUE)
Datos_INE$Gini <- as.numeric(Datos_INE$Gini)
Datos_INE$Pobr_Rel <- sub(",", ".", Datos_INE$Pobr_Rel, fixed = TRUE)
Datos_INE$Pobr_Rel <- as.numeric(Datos_INE$Pobr_Rel)
Datos_INE$P80P20 <- sub(",", ".", Datos_INE$P80P20, fixed = TRUE)
Datos_INE$P80P20 <- as.numeric(Datos_INE$P80P20)
Datos_INE$cntschid <- Datos_INE$CNTSCHID
#Union de ficheros en un solo dataset
Datos_PISA <- merge(x=Datos_PISA1,y=Datos_INE[, c("cntschid","RNM_Persona", "RNM_Hogar", "Gini","Pobr_Rel","P80P20")], all.x=TRUE, by="cntschid")
Datos_PISA <- na.exclude(Datos_PISA)
Datos_PISA <- rebindAttributes(Datos_PISA, PISA)
dim(Datos_PISA)

##########################################################################################

#Correlaciones entre predictores de nivel 1:
cor.sdf(x="attlnact", y="resilience", data=Datos_PISA, method="Pearson")
cor.sdf(x="attlnact", y="mastgoal", data=Datos_PISA, method="Pearson")
cor.sdf(x="resilience", y="mastgoal", data=Datos_PISA, method="Pearson")
#Correlaciones entre precitores de nivel 2:
cor.sdf(x="sch_escs", y="RNM_Persona", data=Datos_PISA, method="Pearson")
cor.sdf(x="sch_escs", y="Pobr_Rel", data=Datos_PISA, method="Pearson")
cor.sdf(x="RNM_Persona", y="Pobr_Rel", data=Datos_PISA, method="Pearson")


##########################################################################################

#Correlaciones entre valores plausibles y variables de control:
#Lectura
Cor1ReadS <- cor.sdf(x="pv1read", y="Sex", data=Datos_PISA, method="Polyserial")
Cor2ReadS <- cor.sdf(x="pv2read", y="Sex", data=Datos_PISA, method="Polyserial")
Cor3ReadS <- cor.sdf(x="pv3read", y="Sex", data=Datos_PISA, method="Polyserial")
Cor4ReadS <- cor.sdf(x="pv4read", y="Sex", data=Datos_PISA, method="Polyserial")
Cor5ReadS <- cor.sdf(x="pv5read", y="Sex", data=Datos_PISA, method="Polyserial")
Cor6ReadS <- cor.sdf(x="pv6read", y="Sex", data=Datos_PISA, method="Polyserial")
Cor7ReadS <- cor.sdf(x="pv7read", y="Sex", data=Datos_PISA, method="Polyserial")
Cor8ReadS <- cor.sdf(x="pv8read", y="Sex", data=Datos_PISA, method="Polyserial")
Cor9ReadS <- cor.sdf(x="pv9read", y="Sex", data=Datos_PISA, method="Polyserial")
Cor10ReadS <- cor.sdf(x="pv10read", y="Sex", data=Datos_PISA, method="Polyserial")
mean(Cor1ReadS$correlation,Cor2ReadS$correlation,Cor3ReadS$correlation,
     Cor4ReadS$correlation,Cor5ReadS$correlation,Cor6ReadS$correlation,
     Cor7ReadS$correlation,Cor8ReadS$correlation,Cor9ReadS$correlation,
     Cor10ReadS$correlation)
Cor1ReadI <- cor.sdf(x="pv1read", y="Immig", data=Datos_PISA, method="Polyserial")
Cor2ReadI <- cor.sdf(x="pv2read", y="Immig", data=Datos_PISA, method="Polyserial")
Cor3ReadI <- cor.sdf(x="pv3read", y="Immig", data=Datos_PISA, method="Polyserial")
Cor4ReadI <- cor.sdf(x="pv4read", y="Immig", data=Datos_PISA, method="Polyserial")
Cor5ReadI <- cor.sdf(x="pv5read", y="Immig", data=Datos_PISA, method="Polyserial")
Cor6ReadI <- cor.sdf(x="pv6read", y="Immig", data=Datos_PISA, method="Polyserial")
Cor7ReadI <- cor.sdf(x="pv7read", y="Immig", data=Datos_PISA, method="Polyserial")
Cor8ReadI <- cor.sdf(x="pv8read", y="Immig", data=Datos_PISA, method="Polyserial")
Cor9ReadI <- cor.sdf(x="pv9read", y="Immig", data=Datos_PISA, method="Polyserial")
Cor10ReadI <- cor.sdf(x="pv10read", y="Immig", data=Datos_PISA, method="Polyserial")
mean(Cor1ReadI$correlation,Cor2ReadI$correlation,Cor3ReadI$correlation,
     Cor4ReadI$correlation,Cor5ReadI$correlation,Cor6ReadI$correlation,
     Cor7ReadI$correlation,Cor8ReadI$correlation,Cor9ReadI$correlation,
     Cor10ReadI$correlation)
Cor1ReadR <- cor.sdf(x="pv1read", y="Repeat", data=Datos_PISA, method="Polyserial")
Cor2ReadR <- cor.sdf(x="pv2read", y="Repeat", data=Datos_PISA, method="Polyserial")
Cor3ReadR <- cor.sdf(x="pv3read", y="Repeat", data=Datos_PISA, method="Polyserial")
Cor4ReadR <- cor.sdf(x="pv4read", y="Repeat", data=Datos_PISA, method="Polyserial")
Cor5ReadR <- cor.sdf(x="pv5read", y="Repeat", data=Datos_PISA, method="Polyserial")
Cor6ReadR <- cor.sdf(x="pv6read", y="Repeat", data=Datos_PISA, method="Polyserial")
Cor7ReadR <- cor.sdf(x="pv7read", y="Repeat", data=Datos_PISA, method="Polyserial")
Cor8ReadR <- cor.sdf(x="pv8read", y="Repeat", data=Datos_PISA, method="Polyserial")
Cor9ReadR <- cor.sdf(x="pv9read", y="Repeat", data=Datos_PISA, method="Polyserial")
Cor10ReadR <- cor.sdf(x="pv10read", y="Repeat", data=Datos_PISA, method="Polyserial")
mean(Cor1ReadR$correlation,Cor2ReadR$correlation,Cor3ReadR$correlation,
     Cor4ReadR$correlation,Cor5ReadR$correlation,Cor6ReadR$correlation,
     Cor7ReadR$correlation,Cor8ReadR$correlation,Cor9ReadR$correlation,
     Cor10ReadR$correlation)
Cor1ReadE <- cor.sdf(x="pv1read", y="escs", data=Datos_PISA, method="Pearson")
Cor2ReadE <- cor.sdf(x="pv2read", y="escs", data=Datos_PISA, method="Pearson")
Cor3ReadE <- cor.sdf(x="pv3read", y="escs", data=Datos_PISA, method="Pearson")
Cor4ReadE <- cor.sdf(x="pv4read", y="escs", data=Datos_PISA, method="Pearson")
Cor5ReadE <- cor.sdf(x="pv5read", y="escs", data=Datos_PISA, method="Pearson")
Cor6ReadE <- cor.sdf(x="pv6read", y="escs", data=Datos_PISA, method="Pearson")
Cor7ReadE <- cor.sdf(x="pv7read", y="escs", data=Datos_PISA, method="Pearson")
Cor8ReadE <- cor.sdf(x="pv8read", y="escs", data=Datos_PISA, method="Pearson")
Cor9ReadE <- cor.sdf(x="pv9read", y="escs", data=Datos_PISA, method="Pearson")
Cor10ReadE <- cor.sdf(x="pv10read", y="escs", data=Datos_PISA, method="Pearson")
mean(Cor1ReadE$correlation,Cor2ReadE$correlation,Cor3ReadE$correlation,
     Cor4ReadE$correlation,Cor5ReadE$correlation,Cor6ReadE$correlation,
     Cor7ReadE$correlation,Cor8ReadE$correlation,Cor9ReadE$correlation,
     Cor10ReadE$correlation)
Cor1ReadSch <- cor.sdf(x="pv1read", y="Schltype", data=Datos_PISA, method="Polyserial")
Cor2ReadSch <- cor.sdf(x="pv2read", y="Schltype", data=Datos_PISA, method="Polyserial")
Cor3ReadSch <- cor.sdf(x="pv3read", y="Schltype", data=Datos_PISA, method="Polyserial")
Cor4ReadSch <- cor.sdf(x="pv4read", y="Schltype", data=Datos_PISA, method="Polyserial")
Cor5ReadSch <- cor.sdf(x="pv5read", y="Schltype", data=Datos_PISA, method="Polyserial")
Cor6ReadSch <- cor.sdf(x="pv6read", y="Schltype", data=Datos_PISA, method="Polyserial")
Cor7ReadSch <- cor.sdf(x="pv7read", y="Schltype", data=Datos_PISA, method="Polyserial")
Cor8ReadSch <- cor.sdf(x="pv8read", y="Schltype", data=Datos_PISA, method="Polyserial")
Cor9ReadSch <- cor.sdf(x="pv9read", y="Schltype", data=Datos_PISA, method="Polyserial")
Cor10ReadSch <- cor.sdf(x="pv10read", y="Schltype", data=Datos_PISA, method="Polyserial")
mean(Cor1ReadSch$correlation,Cor2ReadSch$correlation,Cor3ReadSch$correlation,
     Cor4ReadSch$correlation,Cor5ReadSch$correlation,Cor6ReadSch$correlation,
     Cor7ReadSch$correlation,Cor8ReadSch$correlation,Cor9ReadSch$correlation,
     Cor10ReadSch$correlation)

#Matematicas
Cor1mathS <- cor.sdf(x="pv1math", y="Sex", data=Datos_PISA, method="Polyserial")
Cor2mathS <- cor.sdf(x="pv2math", y="Sex", data=Datos_PISA, method="Polyserial")
Cor3mathS <- cor.sdf(x="pv3math", y="Sex", data=Datos_PISA, method="Polyserial")
Cor4mathS <- cor.sdf(x="pv4math", y="Sex", data=Datos_PISA, method="Polyserial")
Cor5mathS <- cor.sdf(x="pv5math", y="Sex", data=Datos_PISA, method="Polyserial")
Cor6mathS <- cor.sdf(x="pv6math", y="Sex", data=Datos_PISA, method="Polyserial")
Cor7mathS <- cor.sdf(x="pv7math", y="Sex", data=Datos_PISA, method="Polyserial")
Cor8mathS <- cor.sdf(x="pv8math", y="Sex", data=Datos_PISA, method="Polyserial")
Cor9mathS <- cor.sdf(x="pv9math", y="Sex", data=Datos_PISA, method="Polyserial")
Cor10mathS <- cor.sdf(x="pv10math", y="Sex", data=Datos_PISA, method="Polyserial")
mean(Cor1mathS$correlation,Cor2mathS$correlation,Cor3mathS$correlation,
     Cor4mathS$correlation,Cor5mathS$correlation,Cor6mathS$correlation,
     Cor7mathS$correlation,Cor8mathS$correlation,Cor9mathS$correlation,
     Cor10mathS$correlation)
Cor1mathI <- cor.sdf(x="pv1math", y="Immig", data=Datos_PISA, method="Polyserial")
Cor2mathI <- cor.sdf(x="pv2math", y="Immig", data=Datos_PISA, method="Polyserial")
Cor3mathI <- cor.sdf(x="pv3math", y="Immig", data=Datos_PISA, method="Polyserial")
Cor4mathI <- cor.sdf(x="pv4math", y="Immig", data=Datos_PISA, method="Polyserial")
Cor5mathI <- cor.sdf(x="pv5math", y="Immig", data=Datos_PISA, method="Polyserial")
Cor6mathI <- cor.sdf(x="pv6math", y="Immig", data=Datos_PISA, method="Polyserial")
Cor7mathI <- cor.sdf(x="pv7math", y="Immig", data=Datos_PISA, method="Polyserial")
Cor8mathI <- cor.sdf(x="pv8math", y="Immig", data=Datos_PISA, method="Polyserial")
Cor9mathI <- cor.sdf(x="pv9math", y="Immig", data=Datos_PISA, method="Polyserial")
Cor10mathI <- cor.sdf(x="pv10math", y="Immig", data=Datos_PISA, method="Polyserial")
mean(Cor1mathI$correlation,Cor2mathI$correlation,Cor3mathI$correlation,
     Cor4mathI$correlation,Cor5mathI$correlation,Cor6mathI$correlation,
     Cor7mathI$correlation,Cor8mathI$correlation,Cor9mathI$correlation,
     Cor10mathI$correlation)
Cor1mathR <- cor.sdf(x="pv1math", y="Repeat", data=Datos_PISA, method="Polyserial")
Cor2mathR <- cor.sdf(x="pv2math", y="Repeat", data=Datos_PISA, method="Polyserial")
Cor3mathR <- cor.sdf(x="pv3math", y="Repeat", data=Datos_PISA, method="Polyserial")
Cor4mathR <- cor.sdf(x="pv4math", y="Repeat", data=Datos_PISA, method="Polyserial")
Cor5mathR <- cor.sdf(x="pv5math", y="Repeat", data=Datos_PISA, method="Polyserial")
Cor6mathR <- cor.sdf(x="pv6math", y="Repeat", data=Datos_PISA, method="Polyserial")
Cor7mathR <- cor.sdf(x="pv7math", y="Repeat", data=Datos_PISA, method="Polyserial")
Cor8mathR <- cor.sdf(x="pv8math", y="Repeat", data=Datos_PISA, method="Polyserial")
Cor9mathR <- cor.sdf(x="pv9math", y="Repeat", data=Datos_PISA, method="Polyserial")
Cor10mathR <- cor.sdf(x="pv10math", y="Repeat", data=Datos_PISA, method="Polyserial")
mean(Cor1mathR$correlation,Cor2mathR$correlation,Cor3mathR$correlation,
     Cor4mathR$correlation,Cor5mathR$correlation,Cor6mathR$correlation,
     Cor7mathR$correlation,Cor8mathR$correlation,Cor9mathR$correlation,
     Cor10mathR$correlation)
Cor1mathE <- cor.sdf(x="pv1math", y="escs", data=Datos_PISA, method="Pearson")
Cor2mathE <- cor.sdf(x="pv2math", y="escs", data=Datos_PISA, method="Pearson")
Cor3mathE <- cor.sdf(x="pv3math", y="escs", data=Datos_PISA, method="Pearson")
Cor4mathE <- cor.sdf(x="pv4math", y="escs", data=Datos_PISA, method="Pearson")
Cor5mathE <- cor.sdf(x="pv5math", y="escs", data=Datos_PISA, method="Pearson")
Cor6mathE <- cor.sdf(x="pv6math", y="escs", data=Datos_PISA, method="Pearson")
Cor7mathE <- cor.sdf(x="pv7math", y="escs", data=Datos_PISA, method="Pearson")
Cor8mathE <- cor.sdf(x="pv8math", y="escs", data=Datos_PISA, method="Pearson")
Cor9mathE <- cor.sdf(x="pv9math", y="escs", data=Datos_PISA, method="Pearson")
Cor10mathE <- cor.sdf(x="pv10math", y="escs", data=Datos_PISA, method="Pearson")
mean(Cor1mathE$correlation,Cor2mathE$correlation,Cor3mathE$correlation,
     Cor4mathE$correlation,Cor5mathE$correlation,Cor6mathE$correlation,
     Cor7mathE$correlation,Cor8mathE$correlation,Cor9mathE$correlation,
     Cor10mathE$correlation)
Cor1mathSch <- cor.sdf(x="pv1math", y="Schltype", data=Datos_PISA, method="Polyserial")
Cor2mathSch <- cor.sdf(x="pv2math", y="Schltype", data=Datos_PISA, method="Polyserial")
Cor3mathSch <- cor.sdf(x="pv3math", y="Schltype", data=Datos_PISA, method="Polyserial")
Cor4mathSch <- cor.sdf(x="pv4math", y="Schltype", data=Datos_PISA, method="Polyserial")
Cor5mathSch <- cor.sdf(x="pv5math", y="Schltype", data=Datos_PISA, method="Polyserial")
Cor6mathSch <- cor.sdf(x="pv6math", y="Schltype", data=Datos_PISA, method="Polyserial")
Cor7mathSch <- cor.sdf(x="pv7math", y="Schltype", data=Datos_PISA, method="Polyserial")
Cor8mathSch <- cor.sdf(x="pv8math", y="Schltype", data=Datos_PISA, method="Polyserial")
Cor9mathSch <- cor.sdf(x="pv9math", y="Schltype", data=Datos_PISA, method="Polyserial")
Cor10mathSch <- cor.sdf(x="pv10math", y="Schltype", data=Datos_PISA, method="Polyserial")
mean(Cor1mathSch$correlation,Cor2mathSch$correlation,Cor3mathSch$correlation,
     Cor4mathSch$correlation,Cor5mathSch$correlation,Cor6mathSch$correlation,
     Cor7mathSch$correlation,Cor8mathSch$correlation,Cor9mathSch$correlation,
     Cor10mathSch$correlation)

#Ciencias
Cor1scieS <- cor.sdf(x="pv1scie", y="Sex", data=Datos_PISA, method="Polyserial")
Cor2scieS <- cor.sdf(x="pv2scie", y="Sex", data=Datos_PISA, method="Polyserial")
Cor3scieS <- cor.sdf(x="pv3scie", y="Sex", data=Datos_PISA, method="Polyserial")
Cor4scieS <- cor.sdf(x="pv4scie", y="Sex", data=Datos_PISA, method="Polyserial")
Cor5scieS <- cor.sdf(x="pv5scie", y="Sex", data=Datos_PISA, method="Polyserial")
Cor6scieS <- cor.sdf(x="pv6scie", y="Sex", data=Datos_PISA, method="Polyserial")
Cor7scieS <- cor.sdf(x="pv7scie", y="Sex", data=Datos_PISA, method="Polyserial")
Cor8scieS <- cor.sdf(x="pv8scie", y="Sex", data=Datos_PISA, method="Polyserial")
Cor9scieS <- cor.sdf(x="pv9scie", y="Sex", data=Datos_PISA, method="Polyserial")
Cor10scieS <- cor.sdf(x="pv10scie", y="Sex", data=Datos_PISA, method="Polyserial")
mean(Cor1scieS$correlation,Cor2scieS$correlation,Cor3scieS$correlation,
     Cor4scieS$correlation,Cor5scieS$correlation,Cor6scieS$correlation,
     Cor7scieS$correlation,Cor8scieS$correlation,Cor9scieS$correlation,
     Cor10scieS$correlation)
Cor1scieI <- cor.sdf(x="pv1scie", y="Immig", data=Datos_PISA, method="Polyserial")
Cor2scieI <- cor.sdf(x="pv2scie", y="Immig", data=Datos_PISA, method="Polyserial")
Cor3scieI <- cor.sdf(x="pv3scie", y="Immig", data=Datos_PISA, method="Polyserial")
Cor4scieI <- cor.sdf(x="pv4scie", y="Immig", data=Datos_PISA, method="Polyserial")
Cor5scieI <- cor.sdf(x="pv5scie", y="Immig", data=Datos_PISA, method="Polyserial")
Cor6scieI <- cor.sdf(x="pv6scie", y="Immig", data=Datos_PISA, method="Polyserial")
Cor7scieI <- cor.sdf(x="pv7scie", y="Immig", data=Datos_PISA, method="Polyserial")
Cor8scieI <- cor.sdf(x="pv8scie", y="Immig", data=Datos_PISA, method="Polyserial")
Cor9scieI <- cor.sdf(x="pv9scie", y="Immig", data=Datos_PISA, method="Polyserial")
Cor10scieI <- cor.sdf(x="pv10scie", y="Immig", data=Datos_PISA, method="Polyserial")
mean(Cor1scieI$correlation,Cor2scieI$correlation,Cor3scieI$correlation,
     Cor4scieI$correlation,Cor5scieI$correlation,Cor6scieI$correlation,
     Cor7scieI$correlation,Cor8scieI$correlation,Cor9scieI$correlation,
     Cor10scieI$correlation)
Cor1scieR <- cor.sdf(x="pv1scie", y="Repeat", data=Datos_PISA, method="Polyserial")
Cor2scieR <- cor.sdf(x="pv2scie", y="Repeat", data=Datos_PISA, method="Polyserial")
Cor3scieR <- cor.sdf(x="pv3scie", y="Repeat", data=Datos_PISA, method="Polyserial")
Cor4scieR <- cor.sdf(x="pv4scie", y="Repeat", data=Datos_PISA, method="Polyserial")
Cor5scieR <- cor.sdf(x="pv5scie", y="Repeat", data=Datos_PISA, method="Polyserial")
Cor6scieR <- cor.sdf(x="pv6scie", y="Repeat", data=Datos_PISA, method="Polyserial")
Cor7scieR <- cor.sdf(x="pv7scie", y="Repeat", data=Datos_PISA, method="Polyserial")
Cor8scieR <- cor.sdf(x="pv8scie", y="Repeat", data=Datos_PISA, method="Polyserial")
Cor9scieR <- cor.sdf(x="pv9scie", y="Repeat", data=Datos_PISA, method="Polyserial")
Cor10scieR <- cor.sdf(x="pv10scie", y="Repeat", data=Datos_PISA, method="Polyserial")
mean(Cor1scieR$correlation,Cor2scieR$correlation,Cor3scieR$correlation,
     Cor4scieR$correlation,Cor5scieR$correlation,Cor6scieR$correlation,
     Cor7scieR$correlation,Cor8scieR$correlation,Cor9scieR$correlation,
     Cor10scieR$correlation)
Cor1scieR <- cor.sdf(x="pv1scie", y="escs", data=Datos_PISA, method="Pearson")
Cor2scieR <- cor.sdf(x="pv2scie", y="escs", data=Datos_PISA, method="Pearson")
Cor3scieR <- cor.sdf(x="pv3scie", y="escs", data=Datos_PISA, method="Pearson")
Cor4scieR <- cor.sdf(x="pv4scie", y="escs", data=Datos_PISA, method="Pearson")
Cor5scieR <- cor.sdf(x="pv5scie", y="escs", data=Datos_PISA, method="Pearson")
Cor6scieR <- cor.sdf(x="pv6scie", y="escs", data=Datos_PISA, method="Pearson")
Cor7scieR <- cor.sdf(x="pv7scie", y="escs", data=Datos_PISA, method="Pearson")
Cor8scieR <- cor.sdf(x="pv8scie", y="escs", data=Datos_PISA, method="Pearson")
Cor9scieR <- cor.sdf(x="pv9scie", y="escs", data=Datos_PISA, method="Pearson")
Cor10scieR <- cor.sdf(x="pv10scie", y="escs", data=Datos_PISA, method="Pearson")
mean(Cor1scieR$correlation,Cor2scieR$correlation,Cor3scieR$correlation,
     Cor4scieR$correlation,Cor5scieR$correlation,Cor6scieR$correlation,
     Cor7scieR$correlation,Cor8scieR$correlation,Cor9scieR$correlation,
     Cor10scieR$correlation)
Cor1scieSch <- cor.sdf(x="pv1scie", y="Schltype", data=Datos_PISA, method="Polyserial")
Cor2scieSch <- cor.sdf(x="pv2scie", y="Schltype", data=Datos_PISA, method="Polyserial")
Cor3scieSch <- cor.sdf(x="pv3scie", y="Schltype", data=Datos_PISA, method="Polyserial")
Cor4scieSch <- cor.sdf(x="pv4scie", y="Schltype", data=Datos_PISA, method="Polyserial")
Cor5scieSch <- cor.sdf(x="pv5scie", y="Schltype", data=Datos_PISA, method="Polyserial")
Cor6scieSch <- cor.sdf(x="pv6scie", y="Schltype", data=Datos_PISA, method="Polyserial")
Cor7scieSch <- cor.sdf(x="pv7scie", y="Schltype", data=Datos_PISA, method="Polyserial")
Cor8scieSch <- cor.sdf(x="pv8scie", y="Schltype", data=Datos_PISA, method="Polyserial")
Cor9scieSch <- cor.sdf(x="pv9scie", y="Schltype", data=Datos_PISA, method="Polyserial")
Cor10scieSch <- cor.sdf(x="pv10scie", y="Schltype", data=Datos_PISA, method="Polyserial")
mean(Cor1scieSch$correlation,Cor2scieSch$correlation,Cor3scieSch$correlation,
     Cor4scieSch$correlation,Cor5scieSch$correlation,Cor6scieSch$correlation,
     Cor7scieSch$correlation,Cor8scieSch$correlation,Cor9scieSch$correlation,
     Cor10scieSch$correlation)

##########################################################################################

#Estisticos descriptivos:
#Variables dependientes
PVREAD <- summary2(Datos_PISA, "read")
PVMATH <- summary2(Datos_PISA, "math")
PVSCIE <- summary2(Datos_PISA, "scie")
#Variables de control (tengo que darle la vuelta a las categoricas)
SEX <- summary2(Datos_PISA, "st004d01t", weightVar = NULL)
IMMIG <- summary2(Datos_PISA, "immig", weightVar = NULL)
REPEAT <- summary2(Datos_PISA, "repeatgrade", weightVar = NULL)
ESCS <- summary2(Datos_PISA, "escs", weightVar = NULL)
SCHLTYPE <- summary2(Datos_PISA, "schltype", weightVar = NULL)
#Variables nivel 1
ATTLNACT <- summary2(Datos_PISA, "attlnact", weightVar = NULL)
MASTGOAL <- summary2(Datos_PISA, "mastgoal", weightVar = NULL)
RESILIENCE <- summary2(Datos_PISA, "resilience", weightVar = NULL)
#Variables nivel 2
SCH_ESCS <- summary2(Datos_PISA, "sch_escs", weightVar = NULL)
RNM_PERSONA <- summary2(Datos_PISA, "RNM_Persona", weightVar = NULL)
POBR_REL <- summary2(Datos_PISA, "Pobr_Rel", weightVar = NULL)


################################################################################

### PASO 1: MODELOS NULOS ###

Modelo.Nulo.Lectura <- mixed.sdf(read~(1|cntschid), data=Datos_PISA, weightVars=c("w_fstuwt","w_schgrnrabwt"), weightTransformation=FALSE, verbose=0)
summary(Modelo.Nulo.Lectura)

Modelo.Nulo.Matemáticas <- mixed.sdf(math~(1|cntschid), data=Datos_PISA, weightVars=c("w_fstuwt","w_schgrnrabwt"), weightTransformation=FALSE, verbose=0)
summary(Modelo.Nulo.Matemáticas)
prop.test(x=Modelo.Nulo.Ciencias$coef,25701)

Modelo.Nulo.Ciencias <- mixed.sdf(scie~(1|cntschid), data=Datos_PISA, weightVars=c("w_fstuwt","w_schgrnrabwt"), weightTransformation=FALSE, verbose=0)
summary(Modelo.Nulo.Ciencias)
Modelo.Nulo.Ciencias$Vimp

################################################################################

### PASO 2: VARIABLES DE CONTROL ###
Modelo.1.Lectura <- mixed.sdf(pv1read~(1|cntschid), data=Datos_PISA, weightVars=c("w_fstuwt","w_schgrnrabwt"), weightTransformation=FALSE, verbose=0)
summary(Modelo.1.Lectura)
Modelo.2.Lectura <- mixed.sdf(pv1read~Sex+Repeat+Immig+(1|cntschid), data=Datos_PISA, weightVars=c("w_fstuwt","w_schgrnrabwt"), weightTransformation=FALSE, verbose=0)
summary(Modelo.2.Lectura)
anova(Modelo.2.Lectura, Modelo.2.Lectura)

