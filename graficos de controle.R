library(qcc)
library(bcc)


controlcharts_n<-list(0)
b_controlcharts<-list(0)




for (i in 1:24) {
  controlcharts_n[[i]]<-qcc(fitarima[[i]][["residuals"]], type = "xbar.one")
}
for (i in 1:24) {
  b_controlcharts[[i]]<-bcc((fitarima[[i]][["residuals"]]-min(fitarima[[i]][["residuals"]]))/(max(fitarima[[i]][["residuals"]])-min(fitarima[[i]][["residuals"]]))
                            , type = "2")
}

#bcc(fitarima[[13]][["residuals"]], type = "1")
#bcc
#nvna<-qcc(fitarima[[11]][["residuals"]], type = "xbar.one")

#bcc( fitarima[[9]][["residuals"]], type = "1")

#plot(nvna)
plot(controlcharts_n[[11]])
(fitarima[[11]][["residuals"]]-min(fitarima[[11]][["residuals"]]))/(max(fitarima[[11]][["residuals"]])-min(fitarima[[11]][["residuals"]]))



plot(b_controlcharts[[11]])
bcc((fitarima[[11]][["residuals"]]-min(fitarima[[11]][["residuals"]]))/(max(fitarima[[11]][["residuals"]])-min(fitarima[[11]][["residuals"]]))
, type = "2")

length((fitarima[[11]][["residuals"]]))

#fazer gráficos corrido
matriz_residuos <- matrix(data = NA, nrow = length((fitarima[[11]][["residuals"]])), ncol = 24)
for (i in 1:24) {
  matriz_residuos[,i]<- as.vector(fitarima[[i]][["residuals"]])
}
residuos<-c(t(matriz_residuos))
graficodecontrole<- qcc(residuos, type = "xbar.one")




pdf(file = "plotbcc_16hhh.pdf",
    width = 12,
    height = 5)
plot(b_controlcharts[[17]])
dev.off()
