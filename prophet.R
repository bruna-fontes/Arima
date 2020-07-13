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
prev<-prev[-1,]
for (i in 1:24) {
  prev_s[[i]]<-prev_s[[i]][-1,]
}
