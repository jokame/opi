library("tidyverse", quietly = T)
library(lubridate)
library(kohonen)

#ecoDic2016 <- download.file("https://www.ecobici.cdmx.gob.mx/sites/default/files/data/usages/2016-12.csv",
#                            "ecoDic2016.csv")
#ecoNov2016 <- download.file("https://www.ecobici.cdmx.gob.mx/sites/default/files/data/usages/2016-11.csv",
#                            "ecoNov2016.csv")
#ecoOct2016 <- download.file("https://www.ecobici.cdmx.gob.mx/sites/default/files/data/usages/2016-10.csv",
#                            "ecoOct2016.csv")

dfDic <- read.csv("ecoDic2016.csv")
dfNov <- read.csv("ecoNov2016.csv", header = F)
dfOct <- read.csv("ecoOct2016.csv")
colnames(dfNov) <- colnames(dfDic)
dfEcobici <- rbind(dfDic, dfNov, dfOct)

aggregate(dfEcobici$Ciclo_Estacion_Retiro, by=list(dfEcobici$Ciclo_Estacion_Retiro), 
          FUN=length)

retiros <- aggregate(dfEcobici$Ciclo_Estacion_Retiro,
            by=list(dfEcobici$Ciclo_Estacion_Retiro),
            FUN=length)
arribos <- aggregate(dfEcobici$Ciclo_Estacion_Arribo,
                     by=list(dfEcobici$Ciclo_Estacion_Arribo),
                     FUN=length)
colnames(retiros) <- c("estacion", "movimientos")
colnames(arribos) <- c("estacion", "movimientos")

movs <- left_join(retiros, arribos, by="estacion")
colnames(movs) <- c("estacion", "retiros", "arribos")
movs$afluencia <- movs$retiros+movs$arribos

top10 <- movs%>%
  arrange(desc(afluencia)) %>%
  top_n(10)

estacionesTOP <- dfEcobici %>%
  filter(Ciclo_Estacion_Retiro %in% top10$estacion)

estacionesTOP$Hora_Retiro_int <- parse_time(estacionesTOP$Hora_Retiro)
estacionesTOP$Hora_Arribo_int <- parse_time(estacionesTOP$Hora_Arribo)

ggplot(data=estacionesTOP, aes(Hora_Retiro_int)) +
  geom_histogram(bins=24) +
  facet_wrap(~Ciclo_Estacion_Retiro) +
  labs(title="Histogramas de retiro por estacion") +
  labs(x="Hora", y="Frecuencia")

ggplot(data=estacionesTOP, aes(Hora_Arribo_int)) +
  geom_histogram(bins=24) +
  facet_wrap(~Ciclo_Estacion_Retiro) +
  labs(title="Histogramas de arribo por estacion") +
  labs(x="Hora", y="Frecuencia")

ggplot(data=estacionesTOP, aes(Hora_Retiro_int)) +
  geom_histogram(bins=24) +
  labs(title="Histogramas de retiro por estacion") +
  labs(x="Hora", y="Frecuencia")
ggplot(data=estacionesTOP, aes(Hora_Arribo_int)) +
  geom_histogram(bins=24) +
  labs(title="Histogramas de arribo por estacion") +
  labs(x="Hora", y="Frecuencia")

retirosDic <- aggregate(dfDic$Ciclo_Estacion_Retiro,
                     by=list(dfDic$Ciclo_Estacion_Retiro),
                     FUN=length)
retirosNov <- aggregate(dfNov$Ciclo_Estacion_Retiro,
                        by=list(dfNov$Ciclo_Estacion_Retiro),
                        FUN=length)
retirosOct <- aggregate(dfOct$Ciclo_Estacion_Retiro,
                        by=list(dfOct$Ciclo_Estacion_Retiro),
                        FUN=length)

arribosDic <- aggregate(dfDic$Ciclo_Estacion_Arribo,
                     by=list(dfDic$Ciclo_Estacion_Arribo),
                     FUN=length)
arribosNov <- aggregate(dfNov$Ciclo_Estacion_Arribo,
                        by=list(dfNov$Ciclo_Estacion_Arribo),
                        FUN=length)
arribosOct <- aggregate(dfOct$Ciclo_Estacion_Arribo,
                        by=list(dfOct$Ciclo_Estacion_Arribo),
                        FUN=length)
movsDic <- full_join(retirosDic, arribosDic, by="Group.1")
movsNov <- full_join(retirosNov, arribosNov, by="Group.1")
movsOct <- full_join(retirosOct, arribosOct, by="Group.1")

colnames(movsDic) <- c("estacion", "retiro", "arribo")
colnames(movsNov) <- c("estacion", "retiro", "arribo")
colnames(movsOct) <- c("estacion", "retiro", "arribo")

movsDic$total <- movsDic$retiro+movsDic$arribo
movsNov$total <- movsNov$retiro+movsNov$arribo
movsOct$total <- movsOct$retiro+movsOct$arribo

movsMeses <- full_join(
  full_join(
    data.frame(estacion=movsDic$estacion, total=movsDic$total), 
    data.frame(estacion=movsNov$estacion, total=movsNov$total),
    by="estacion"),
  data.frame(estacion=movsOct$estacion, total=movsOct$total),
  by="estacion")

colnames(movsMeses) <- c("estacion", "dic", "nov", "oct")

totalMeses <- rbind(data.frame(estacion=movsMeses$estacion, mes="12", total=movsMeses$dic),
  data.frame(estacion=movsMeses$estacion, mes="11", total=movsMeses$nov),
  data.frame(estacion=movsMeses$estacion, mes="10", total=movsMeses$oct))


alta <- movsMeses %>%
  filter(dic > nov & nov > oct)

baja <- movsMeses %>%
  filter(dic < nov & nov < oct)


ggplot(data= filter(totalMeses, estacion %in% alta$estacion),
       aes(estacion, total)) +
         geom_bar(aes(fill=mes), position = "dodge", stat="identity")

ggplot(data= filter(totalMeses, estacion %in% baja$estacion),
       aes(estacion, total)) +
  geom_bar(aes(fill=mes), position = "stack", stat="identity")


entrena <- data.frame(retiros=movs$retiros, arribos=movs$arribos)
entrenaKohonen <- as.matrix(scale(entrena))
set.seed(3)
som_grid <- somgrid(xdim = 10, ydim=10, topo="hexagonal")
som_model <- som(entrenaKohonen, 
                 grid=som_grid, 
                 rlen=1000, 
                 alpha=c(0.05,0.01), 
                 keep.data = TRUE,
                 n.hood="circular")
plot(som_model, type = "changes")
plot(som_model, type="count")
plot(som_model, type="dist.neighbours")
plot(som_model, type="codes")

mydata <- som_model$codes 
wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var)) 
for (i in 2:15) {
  wss[i] <- sum(kmeans(mydata, centers=i)$withinss)
}
plot(wss)

som_cluster <- cutree(hclust(dist(som_model$codes)), 3)
plot(som_model, type="codes", main = "Clusters") 
add.cluster.boundaries(som_model, som_cluster)
