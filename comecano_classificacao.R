# Começando com a questão de classificação
install.packages("fBasics")

rm(list = ls())
banco <- creditcard_projeto
attach(banco)
names(banco)
#####################################################################
# Olhando quais variáveis assumem NA.
# which(apply(banco,2,is.na) == TRUE)
# integer(0)


library(fBasics)
basicStats(banco)
banco <- na.omit(banco)
#####################################################################
q <- seq(1:length(Class))
plot(q,Class)
# Tem pouca quantidade de 1 e muita quantidade de 0.
q1 <- seq(1:50000)
plot(q1,Class[1:50000])
#####################################################################

#####################################################################
# Posições que assumem 
vec <- which(Class == 1)
# São 387 que em Class assume 1.

posicoes <- which(Class == 0)
# São 227459 que em Class assume 1.
set.seed(1992)
vec1 <- sample(x = posicoes, size = 2 * 387)
# Pegando duas vezes a quantidade de 1's porque o banco é muito desbalanceado,
# length(posicoes)/length(vec) = 587.7494. tem 587.7494 cerca de 99,8% de 0's
# em Class.




