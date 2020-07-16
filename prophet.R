library(prophet)

history<-list(0)
for (i in 1:24) {
  history[[i]] <- data.frame(ds = seq(as.Date('2013-05-01'), as.Date('2014-04-26'), by = 'd'),
                             y = banco_treinamento[,i])
}
m<-list(0)
n<-list(0)
for (i in 1:24) {
  m[[i]] <- prophet(history[[i]])
  n[[i]] <-prophet(history[[i]],yearly.seasonality=TRUE)
}
future <- make_future_dataframe(m[[1]], periods = 5)
forecast_s<-list(0)
forecast_com_saz<-list(0)
for (i in 1:24) {
  forecast_s[[i]] <- predict(m[[i]], future)
  forecast_com_saz[[i]] <- predict(n[[i]], future)
}
prev_s<-list(0)
for (i in 1:24) {
  prev_s[[i]]<-tail(forecast_s[[i]][c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])
}

for (i in 1:24) {
  prev_s[[i]]<-prev_s[[i]][-1,]
}
prev_com_saz<-list(0)
for (i in 1:24) {
  prev_com_saz[[i]]<-tail(forecast_com_saz[[i]][c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])
}

for (i in 1:24) {
  prev_com_saz[[i]]<-prev_com_saz[[i]][-1,]
}
accuracy_s_yhat<-list(0)
accuracy_s_low<-list(0)
accuracy_s_up<-list(0)
for (i in 1:24) {
accuracy_s_yhat[[i]]<-accuracy(prev_s[[i]][["yhat"]],matriz_teste[,i]) 
accuracy_s_low[[i]]<-accuracy(prev_s[[i]][["yhat_lower"]],matriz_teste[,i]) 
accuracy_s_up[[i]]<-accuracy(prev_s[[i]][["yhat_upper"]],matriz_teste[,i]) 
}
com_saz_accuracy_yhat<-list(0)
com_saz_accuracy_low<-list(0)
com_saz_accuracy_up<-list(0)
for (i in 1:24) {
  com_saz_accuracy_yhat[[i]]<-accuracy(prev_com_saz[[i]][["yhat"]],matriz_teste[,i]) 
  com_saz_accuracy_low[[i]]<-accuracy(prev_com_saz[[i]][["yhat_lower"]],matriz_teste[,i]) 
  com_saz_accuracy_up[[i]]<-accuracy(prev_com_saz[[i]][["yhat_upper"]],matriz_teste[,i]) 
}

