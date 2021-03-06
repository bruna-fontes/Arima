#Resultados, tabelas e gráficos


previsao<-forecast(manual_fit_12hrs,h=5)
aa<-accuracy(prev_auto[[12]],matriz_teste[,12])
 bb<-accuracy(prev,matriz_teste[,12])
cc<- c('-','-','-','-','-','-','-')

dd<-rbind(aa,cc, bb)

prev_auto<- list(0)

for (i in 1:24) {
  prev_auto[[i]]<- forecast(fitarima[[i]],h=5)
}


lista_lower<-list(0)
for (i in 1:24) {
  lista_lower[[i]]<- as.vector(prev_auto[[i]][["lower"]])
}
matriz_prev_low_95 <- matrix(data = NA, nrow = 5, ncol = 24)

for (i in 1:24) {
  matriz_prev_low_95[,i]<- lista_lower[[i]][6:10]
}


matriz_prev_low_80 <- matrix(data = NA, nrow = 5, ncol = 24)

for (i in 1:24) {
  matriz_prev_low_80[,i]<- lista_lower[[i]][1:5]
}

lista_upper<-list(0)
for (i in 1:24) {
  lista_upper[[i]]<- as.vector(prev_auto[[i]][["upper"]])
}
matriz_prev_upper_95 <- matrix(data = NA, nrow = 5, ncol = 24)

for (i in 1:24) {
  matriz_prev_upper_95[,i]<- lista_upper[[i]][6:10]
}


matriz_prev_upper_80 <- matrix(data = NA, nrow = 5, ncol = 24)

for (i in 1:24) {
  matriz_prev_upper_80[,i]<- lista_upper[[i]][1:5]
}

matriz_prev_mean <- matrix(data = NA, nrow = 5, ncol = 24)
for (i in 1:24) {
  matriz_prev_mean[,i]<- as.vector(prev_auto[[i]][["mean"]])
}

#Transformar em vetor pra plotar

low_80<-c(t(matriz_prev_low_80))
low_95<-c(t(matriz_prev_low_95))
mean_auto <-c(t(matriz_prev_mean))
upper_80<-c(t(matriz_prev_upper_80))
upper_95<- c(t(matriz_prev_upper_95))
teste <- c(t(matriz_teste))
cont<- c(1:120)
#plots

graf_80<-ggplot() +
  geom_line(aes(x= cont, y= low_80))+
  geom_line(aes(x= cont, y= upper_80))+
  geom_line(aes(x= cont, y= mean_auto))+
  geom_line(aes(x= cont, y= teste))

graf_95<-ggplot() +
  geom_line(aes(x= cont, y= low_95))+
  geom_line(aes(x= cont, y= upper_95))+
  geom_line(aes(x= cont, y= mean_auto))+
  geom_line(aes(x= cont, y= teste))

graf_80_11h <- ggplot()+
  geom_line(aes(x=c(1:5), y= matriz_prev_low_80[,12]))+
  geom_line(aes(x=c(1:5), y= matriz_prev_upper_80[,12]))+
  geom_line(aes(x=c(1:5), y= matriz_prev_mean[,12]))+
  geom_point(aes(x=c(1:5), y= matriz_teste[,12]))




#metricas de "accuracy"

arima_ac_low_80<-arima_ac_low_95<- arima_ac_up_95<-arima_ac_up_80<- arima_ac_media<- list(0)
for (i in 1:24) {
  arima_ac_low_95[[i]]<-accuracy(matriz_prev_low_95[,i],matriz_teste[,i]) 
  arima_ac_low_80[[i]]<-accuracy(matriz_prev_low_80[,i],matriz_teste[,i]) 
  arima_ac_up_80[[i]]<-accuracy(matriz_prev_upper_80[,i],matriz_teste[,i]) 
  arima_ac_up_95[[i]]<-accuracy(matriz_prev_upper_80[,i],matriz_teste[,i]) 
  arima_ac_media[[i]]<-accuracy(matriz_prev_upper_95[,i],matriz_teste[,i]) 
}
hora_0<-rbind(arima_ac_low_80[1],arima_ac_low_95[1],arima_ac_media[1],arima_ac_up_80[1],arima_ac_up_95[1])

M<-matrix(data=NA,nrow = 5, ncol=24)
nn<-c(arima_ac_low_95[1])
