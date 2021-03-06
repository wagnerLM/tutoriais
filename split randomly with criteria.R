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
#criteria_scale � o argumento das vari�veis escalares que se deseja equivalentes entre os grupos
#criteria_nominal � o argumento das vari�veis nominais que se deseja equivalentes entre os grupos
#tolerance_nominal � o argumento com o valor da diferen�a m�xima entre os grupos que ser� tolerado para vari�veis nominais
#sets_n � o n�mero de grupos
#repetitions � o argumento com o n�mero de itera��es do algoritmo

#cria um novo banco de dados com uma vari�vel de grupo
View(banco_equal)
