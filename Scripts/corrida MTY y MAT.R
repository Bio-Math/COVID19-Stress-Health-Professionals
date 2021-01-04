# Código de análisis de datos de stress
# Hecho por GSRC, MACL, GJAR
# UABC, UPP, UANL oct 2020

#cargando datos
setwd("D:/Documentos/UABC/2019-nCov/Stress")

datar <- read.csv(file = "datosNum.csv", header = TRUE, sep = ",")
#omitir faltantes
datar <- na.omit(datar)
datar <- datar[,-1]

library(psych)

psych::alpha(datar)$total$std.alpha


# Ward Hierarchical Clustering with Bootstrapped p values #El buenas buenas!!!!!
library(pvclust)
fit <- pvclust(datar, method.hclust="ward",
               method.dist="euclidean")
plot(fit) # dendogram with p values
# add rectangles around groups highly supported by the data
pvrect(fit, alpha=.95)



# Análisis y árbol
library(C50)
library(ggplot2)

dataC <- read.csv(file = "class.csv", header = TRUE, sep = ",")
attach(dataC)
summary(dataC)
dataC <- as.data.frame(dataC)
countsStress = table(dataC$Class)
countsStress
barplot(countsStress, main="Stress  Distribution in Healthcare Personnel",
        xlab="Stress Level", ylab="Frequency") 

countsProf= table(dataC$Prof)
countsProf

countsArea = table(dataC$area)
countsArea

modelo.arbol <-C5.0(Class~Prof+area+Dan.Con+Socio+Xeno+Trauma+Comp, data=dataC)
plot(modelo.arbol)

summary(modelo.arbol)
 