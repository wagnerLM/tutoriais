# Exemplo Rasch
install.packages("eRm")
library(eRm)
dassbin<-read.csv("https://raw.githubusercontent.com/wagnerLM/netusf/master/dassbin",sep = ";")
View(dassbin)
dasslabels<-scan("https://raw.githubusercontent.com/wagnerLM/netusf/master/dasslabels",what = "character", sep = "\n")
dasslabels
# Itens completos 
dassnames<-scan("https://raw.githubusercontent.com/wagnerLM/netusf/master/dassnames",what = "character", sep = "\n")
dassnames
# Selecionando itens de depressão
dassbin_sub<-dassbin[,c(3,5,10,13,16,17,21)]
dassnames[c(3,5,10,13,16,17,21)]

# Fit do modelo rasch
mod_sub<-RM(dassbin_sub)
summary(mod_sub)
par(mfrow=c(1,1))
plotINFO(mod_sub,type="item")
#medidas de ajuste
pres <- person.parameter(mod_sub)
IC(pres)
itemfit(pres)
personfit(pres)
summary(pres)
gof.res <- gofIRT(pres)
summary(gof.res)
gof.res
#curvas
plotICC(mod_sub)
plotICC(mod_sub, empICC=list("raw"))
plotjointICC(mod_sub, legpos = "left")

plotPImap(mod_sub,sorted=T)
dassnames[c(3,5,10,13,16,17,21)]
