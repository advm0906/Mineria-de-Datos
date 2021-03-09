library(TSA)
library(tseries)
data(lynx)
class(lynx)

#Grafica de la Serie
plot(lynx,xlab="Año",ylab="Número de Linces",main="Número de Linces Capturados")

#Pruebas de Regresión Lineal y Varianza
lylm<-lm(lynx~time(lynx),data=lynx)
summary(lylm)
adf.test(lynx,alternative = "stationary")


#Modelo AR
valAICar<-Inf
for (i in 1:5){
  
  aicpar<-AIC(arima(lynx,order=c(i,0,0),method = "ML"))
  if (aicpar<valAICar){
    valAICar<-aicpar
    ARmejor<-i
  }
}

valAICar
ARmejor

#Modelo MA
valAICma<-Inf
for (i in 1:5){
  
  aicpar<-AIC(arima(lynx,order=c(0,0,i),method = "ML"))
  if (aicpar<valAICma){
    valAICma<-aicpar
    MAmejor<-i
  }
}

valAICma
MAmejor


#Modelo ARMA

valAICarma<-Inf
for (j in 1:5){
  for (i in 1:5){
    aicpar<-AIC(arima(lynx,order = c(i,0,j),method="ML"))
    if (aicpar<valAICarma){
      valAICarma<-aicpar
      mejorARMA<-c(i,j)
    }
  }
}

valAICarma
mejorARMA

#Comparación AIC
valAICar
valAICma
valAICarma

#Pronósticos

predARMA<-predict(arima(lynx,order = c(mejorARMA[1],0,mejorARMA[2]),method="ML"),n.ahead=50)$pred
predARMA
plot(lynx,xlab="Año",ylab="Número de Linces",main="Número de Linces Capturados",xlim=c(1820,1980))
lines(predARMA,col="red")
