setwd("C:/Users/alex9_000/Desktop/Universidad/Master/TFM/Datos/3.Multinivel_PISA/Callejero")

Secciones_Renta <- read.csv("C:/Users/alex9_000/Desktop/Universidad/Master/TFM/Datos/3.Multinivel_PISA/Callejero/Renta_Media_España.csv", sep = ";")
Gini_Espanna <- read.csv("C:/Users/alex9_000/Desktop/Universidad/Master/TFM/Datos/3.Multinivel_PISA/Callejero/Gini_España.csv", sep=";")
Pobreza_Relativa <- read.csv("C:/Users/alex9_000/Desktop/Universidad/Master/TFM/Datos/3.Multinivel_PISA/Callejero/Medidas_Pobreza_Relativa.csv", sep=";")

Secciones_RentaMedia_Persona <- Secciones_Renta[Secciones_Renta$Periodo ==2018,]
Secciones_RentaMedia_Persona <- Secciones_RentaMedia_Persona[Secciones_RentaMedia_Persona$Indicadores.de.renta.media == "Renta neta media por persona ",]
head(Secciones_RentaMedia_Persona)

Secciones_RentaMedia_Hogar <- Secciones_Renta[Secciones_Renta$Periodo ==2018,]
Secciones_RentaMedia_Hogar <- Secciones_RentaMedia_Hogar[Secciones_RentaMedia_Hogar$Indicadores.de.renta.media == "Renta neta media por hogar",]
head(Secciones_RentaMedia_Hogar)

Secciones_Gini <- Gini_Espanna[Gini_Espanna$Periodo ==2018,]
Secciones_Gini <- Secciones_Gini[Secciones_Gini$Indicadores.de.renta.media == "Índice de Gini",]
head(Secciones_Gini)


write.csv(Secciones_RentaMedia_Persona, "Secciones_RentaMedia_Persona.csv")
write.csv(Secciones_RentaMedia_Hogar, "Secciones_RentaMedia_Hogar.csv")
write.csv(Secciones_Gini, "Secciones_Gini.csv")

Secciones_RentaMedia_Persona <- read.csv("C:/Users/alex9_000/Desktop/Universidad/Master/TFM/Datos/Callejero/Secciones_RentaMedia_Persona.csv", sep=";")
Secciones_RentaMedia_Hogar <- read.csv("C:/Users/alex9_000/Desktop/Universidad/Master/TFM/Datos/Callejero/Secciones_RentaMedia_Hogar.csv", sep=";")
Secciones_Gini <- read.csv("C:/Users/alex9_000/Desktop/Universidad/Master/TFM/Datos/Callejero/Secciones_Gini.csv", sep=";")

combine <- merge(x=Secciones_RentaMedia_Persona,y=Secciones_RentaMedia_Hogar, by="N_Seccion")
combine2 <- merge(x=combine,y=Secciones_Gini, by="N_Seccion")
write.csv(combine2, "Secciones_Renta_ESP.csv")

Instituto_Secciones <- read.csv("C:/Users/alex9_000/Desktop/Universidad/Master/TFM/Datos/Callejero/Institutos_Secciones.csv", sep=";")

head(combine2)

combine3 <- merge(x=Instituto_Secciones,y=combine2, by="N_Seccion")

nrow(combine3)
write.csv(combine3, "Institutos_Renta_ESP.csv")

join1 <- read.csv("C:/Users/alex9_000/Desktop/Universidad/Master/TFM/Datos/Callejero/Coordenadas_Institutos_PISA.csv", sep=";")
join2 <- read.csv("C:/Users/alex9_000/Desktop/Universidad/Master/TFM/Datos/Callejero/Institutos_Renta_ESP.csv", sep=";")
combine4 <- merge(x=join1,y=join2, all = TRUE)
write.csv(combine4, "Tabla_Final_Renta.csv")

Tabla_Final_Renta <- read.csv("C:/Users/alex9_000/Desktop/Universidad/Master/TFM/Datos/3.Multinivel_PISA/Callejero/Tabla_Final_Renta.csv", sep=";")
length(unique(Tabla_Final_Renta$CNTSCHID))
head(Tabla_Final_Renta)

PISA_DEF <- merge(x=Datos_PISA,y=Tabla_Final_Renta[, c("CNTSCHID","RNM_Persona", "RNM_Hogar", "Gini")], all.x=TRUE, by="CNTSCHID")
View(PISA_DEF)
names(PISA_DEF)
PISA <- na.exclude(PISA_DEF)
View (Datos_PISA$CNTSCHID %in% Tabla_Final_Renta$CNTSCHID) #Para ver que todos los centros del primer dataset están en el segundo
View (Tabla_Final_Renta$CNTSCHID %in% Datos_PISA$CNTSCHID) #FALSE si no están en el segundo
Datos_PISA[14196:14234, ] #72400437
Datos_PISA[16026:16066, ] #72400491
Datos_PISA[22283:22288, ] #72400683
n_occur1 <- data.frame(table(Tabla_Final_Renta$CNTSCHID)) 
n_occur1[n_occur1$Freq > 1,] #Para ver qué centros se repiten en el segundo dataset
Tabla_Final_Renta[Tabla_Final_Renta$CNTSCHID %in% n_occur1$Var1[n_occur1$Freq > 1],]


#Para añadir más datos:
Secciones_Desigualdad8020 <- Gini_Espanna[Gini_Espanna$Periodo ==2018,]
Secciones_Desigualdad8020 <- Secciones_Gini[Secciones_Gini$Indicadores.de.renta.media == "Distribución de la renta P80/P20",]
head(Secciones_Desigualdad8020)

Pobreza_Relativa <- Pobreza_Relativa[Pobreza_Relativa$Periodo == 2018,]
Pobreza_Relativa <- Pobreza_Relativa[Pobreza_Relativa$Sexo == "Total",]
Pobreza_Relativa <- Pobreza_Relativa[Pobreza_Relativa$`DistribuciÃ³n.de.la.renta.por.unidad.de.consumo` == "PoblaciÃ³n con ingresos por unidad de consumo por debajo 60% de la mediana",]
head(Pobreza_Relativa)

write.csv(Secciones_Desigualdad8020, "Distribucion_P80P20.csv")
write.csv(Pobreza_Relativa, "Porcentaje_POB_Umbral_Pobreza.csv")

Pobreza_Relativa <- read.csv("C:/Users/alex9_000/Desktop/Universidad/Master/TFM/Datos/3.Multinivel_PISA/Callejero/Porcentaje_POB_Umbral_Pobreza.csv", sep=";")
P80P20 <- read.csv("C:/Users/alex9_000/Desktop/Universidad/Master/TFM/Datos/3.Multinivel_PISA/Callejero/Distribucion_P80P20.csv", sep=";")
head(P80P20)

Tabla_Final_Renta <- merge(x=Tabla_Final_Renta,y=Pobreza_Relativa, all.x=TRUE, by="N_Seccion")
Tabla_Final_Renta <- merge(x=Tabla_Final_Renta,y=P80P20, all.x=TRUE, by="N_Seccion")

write.csv(Tabla_Final_Renta, "Tabla_Final_INE.csv")