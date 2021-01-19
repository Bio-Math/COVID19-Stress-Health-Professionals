# C?digo de an?lisis de datos de stress
# Hecho por GSRC, MACL, GJAR
# UABC, UPP, UANL oct 2020

#cargando datos
setwd("D:/Documentos/UABC/2019-nCov/Stress")

datar <- read.csv(file = "Data/datosNum.csv", header = TRUE, sep = ",")
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



# An?lisis y ?rbol
library(C50)
library(ggplot2)

dataC <- read.csv(file = "Data/class.csv", header = TRUE, sep = ",")
attach(dataC)
summary(dataC)
dataC <- as.data.frame(dataC)
countsStress = table(dataC$Class)
countsStress
barplot(countsStress, main="Stress  Distribution in Healthcare Personnel",
        xlab="Stress Level", ylab="Frequency") 


library(plotly)

plot_ly(
  x = c("Absent", "Mild", "Moderate", "Severe"),
  y = c(9,59,29,5),
  text = c(9,59,29,5), textposition = "auto",
  marker = list(color =c("#8dd4f8", "#f08d7f","#bfea74","#f5cb81")),
  type = "bar"
) %>%
  layout(title = "Stress Distribution in Healthcare Personnel",
         xaxis = list(title = "Stress Level"),
         yaxis = list(title = "Frequency"))



countsProf= table(dataC$Prof)
countsProf

countsArea = table(dataC$area)
countsArea

modelo.arbol <-C5.0(Class~Prof+area+Dan.Con+Socio+Xeno+Trauma+Comp, data=dataC)
plot(modelo.arbol)

summary(modelo.arbol)
 