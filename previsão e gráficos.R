#Resultados, tabelas e gráficos


previsao<-forecast(manual_fit_12hrs,h=5)
aa<-accuracy(auto_prev,matriz_teste[,13])
bb<-accuracy(manual_prev,matriz_teste[,13])
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

