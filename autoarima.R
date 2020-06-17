#Auto-arima - Modelagem
library(MASS)
library(tidyverse)
library(readxl)
library(DescTools)
#library(xlsx)
library(gridExtra)
#library(alr3)
#library(fpp2)
library(lubridate)
library(scales)
library(ggthemes)
library(reshape2)
#library(zoo)
library(matrixStats)
library(nortest)
#library(googlesheets)
library(forecast)
#library(pear)

Solar <- read_delim("Dados.csv", delim = ",")
rad<- Solar$radGlobalCPM11.radGlobal
rad_hora<- c()
for (i in 1:8784) {
  rad_hora[i]<-mean(rad[(6*i-5):(6*i)])
  
}

matriz <-matrix( rad_hora, nrow = 366, ncol = 24 , byrow = T)

maximos<-0
minimos <-0
maximos<-apply(matriz, 2, max)
minimos<-apply(matriz, 2, min)

dias_min<-0
dias_max<- 0

for (i in 1:24) {
  dias_min[i]<- match(minimos[i],matriz[,i])
  dias_max[i]<-match(maximos[i],matriz[,i])
}

nao_retirar <- union(dias_min, dias_max) #dias que n?o podem ser retirados
banco_treinamento<-0


#teste_dia1<- matriz[366,]#01 de maio, unico dia repetido
#teste_dia2<- matriz[365,]  #27 de novembro, um dia antes de um dia com muitos minimos
#teste_dia3 <-matriz[364,]# 23 ou 25 de fev de 2014, um dia depois de um dia de m?ximo
banco_treinamento<- matriz[-c(362,363,364,365, 366), ]
#matriz_teste<-rbind(teste_dia3,teste_dia2, teste_dia1)
matriz_teste<- matriz[c(362,363,364,365, 366), ]
#?ts

serie<-list(0)
for (i in 1:24) {
  serie[[i]]<-as.ts(banco_treinamento[,i])
}

fitarima<-list(0)

for (i in 1:24) {
  fitarima[[i]]<-auto.arima(serie[[i]])
}

#ajuste manual do arima pra compensar acf dos residuos
manual_fit_10hrs<-Arima(serie[[11]], order= c(9,1,2))
manual_fit_11hrs<-Arima(serie[[12]], order= c(9,1,3))   
manual_fit_12hrs<-Arima(serie[[13]], order= c(2,1,1))
manual_fit_14hrs<-Arima(serie[[15]], order= c(2,1,1))
manual_fit_15hrs<-Arima(serie[[16]], order= c(5,1,2))
manual_fit_18hrs<-Arima(serie[[19]], order= c(4,1,3))

#checagem de res?duos
checkresiduals(manual_fit_18hrs)
mean(manual_fit_18hrs[["residuals"]])
abcde<-Acf(manual_fit_18hrs[["residuals"]])

#2? ajuste manual das 18h
manual_fit2_14hrs<-Arima(serie[[15]], order= c(4,1,1))
manual_fit2_18hrs <-Arima(serie[[19]], order= c(10,1,3))


auto<-plot(forecast(fitarima[[13]],h=5))
manual<- plot(forecast(manual_fit_12hrs,h=5))
auto_prev <-(forecast(fitarima[[13]],h=5))
manual_prev <-(forecast(manual_fit_12hrs,h=5))


