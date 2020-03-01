# instalando os pacotes necessarios, incluind a versao de desenvolvedor do semtools
install.packages("devtools")
library(devtools)
devtools::install_github("simsem/semTools/semTools")
library(semTools)
install.packages("lavaan")
library(lavaan)

# Exemplo: DASS21, instrumento para avaliacao de afetos negativos (estresse, depressao e ansiedade)
# Disponivel em http://www2.psy.unsw.edu.au/dass/Portuguese/DASS%2021%20Brazilian%20Portuguese%20Tucci.pdf
# Amostra: como descrita em http://www.scielo.br/scielo.php?script=sci_arttext&pid=S1413-82712015000200259 

# carregando o banco de dados
fdass<-read.csv("https://raw.githubusercontent.com/wagnerLM/tutoriais/master/dassmgroup",sep=";")
# visualizando o banco de dados
#View(fdass)

# definindo o modelo (fatores e itens) 
dass.model<-'Stress =~ DASS1 + DASS6 + DASS8 + DASS11 + DASS12 + DASS14 + DASS18
Anxiety =~ DASS2 + DASS4 + DASS7 + DASS9 + DASS15 + DASS19 + DASS20
Depression =~ DASS3 + DASS5 + DASS10 + DASS13 + DASS16 + DASS17 + DASS21
'
# estimando o ajuste do modelo aos dados
fdass.fit<-cfa(dass.model,fdass,estimator = "WLSMV",ordered = colnames(fdass[,-1]))
summary(fdass.fit,fit.measures=T,standardized=T)

# calculando a fidedignidade dos fatores por meio do Õmega de McDonald
reliability(fdass.fit)

# estimando o modelo de invariância entre grupos
# a funcao estima quatro modelos de invariancia
# configural (fatores)
# interceptos/thresholds
# loadings (cargas fatoriais)
# mean (medias)
# a invariancia dos termos de erro tambem pode ser estimada, modificando a funcao (ver ?measurementInvariance)
fdassmg.fit<-measurementInvariance(model=dass.model,data=fdass,group="gender",estimator = "WLSMV",ordered = 
                                     colnames(fdass[,-1]),method="satorra.bentler.2010")
# estatísticas para cada grupo
fdassmg.fit

# essa funcao apresenta algumas limitacoes para itens ordinais ou dicotomicos, sendo indicada a funcao atualizada abaixo
# usando a funcao measEq.syntax, voce pode criar modelos fixando diferentes parametros entre os grupos
# caso queira testar a diferenca entre os modelos, use a funcao 'lavTestLRT'
fdassmg.fit_inv <- measEq.syntax(configural.model = dass.model,
                                 data = fdass,
                                 ordered = colnames(fdass[,-1]),
                                 parameterization = "delta",
                                 ID.fac = "std.lv",
                                 ID.cat = "Wu.Estabrook.2016",
                                 group = "gender",
                                 group.equal = c("configural","thresholds","loadings"))

model.inv <- as.character(fdassmg.fit_inv)

fdassmg.fit2 <- cfa(model.inv, data = fdass, group = "gender",
                    ordered =colnames(fdass[,-1]))
summary(fdassmg.fit2,fit.measures=T)

# um artigo ótimo sobre o tema foi indicado pelo Leo Martins (https://www.facebook.com/leofer.martins?comment_id=Y29tbWVudDoyNTUyMTA3NjkxNTg1MDM0XzI1Njk0OTgwNjMxNzkzMzA%3D)
# https://sci-hub.tw/10.1080/10705511.2019.1602776