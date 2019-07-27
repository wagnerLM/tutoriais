library(devtools)
devtools::install_github("m-Py/minDiff")
library("minDiff")
banco<-read.csv(file.choose(),sep=";")
View(banco)
banco_equal <- create_groups(banco, criteria_scale = c(),
                             criteria_nominal = c(),
                             tolerance_nominal = c(), sets_n = 2,
                             repetitions=1000)

#Em que:
#criteria_scale é o argumento das variáveis escalares que se deseja equivalentes entre os grupos
#criteria_nominal é o argumento das variáveis nominais que se deseja equivalentes entre os grupos
#tolerance_nominal é o argumento com o valor da diferença máxima entre os grupos que será tolerado para variáveis nominais
#sets_n é o número de grupos
#repetitions é o argumento com o número de iterações do algoritmo

#cria um novo banco de dados com uma variável de grupo
View(banco_equal)
