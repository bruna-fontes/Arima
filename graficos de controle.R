library(qcc)
library(bcc)


controlcharts_n<-list(0)
b_controlcharts<-list(0)



for (i in 1:24) {
  controlcharts_n[[i]]<-qcc(fitarima[[i]][["residuals"]], type = "xbar.one")
}
for (i in 1:24) {
  b_controlcharts[[i]]<-bcc(fitarima[[i]][["residuals"]], type = "1")
}

#bcc(fitarima[[13]][["residuals"]], type = "1")
#bcc
#nvna<-qcc(fitarima[[11]][["residuals"]], type = "xbar.one")

#bcc( fitarima[[9]][["residuals"]], type = "1")

#plot(nvna)
plot(controlcharts_n[[1]])

plot(b_controlcharts[[1]])
