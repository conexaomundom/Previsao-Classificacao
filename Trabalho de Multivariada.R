
install.packages("xtable")

rm(list=ls())

# Banco usado nas análises
banco <- creditcard_projeto

attach(banco)
names(banco)

# Resumo das variávies
library(xtable)
xtable(head(banco[ , 1:11]))
xtable(head(banco[ , 12:23]))
xtable(head(banco[ , 23:31]))


# summario, algumas descritivas do banco de dados.
library(xtable)
xtable(summary(banco[ , 1:4]))
xtable(summary(banco[ , 5:8]))
xtable(summary(banco[ , 9:12]))
xtable(summary(banco[ , 13:16]))
xtable(summary(banco[ , 17:20]))
xtable(summary(banco[ , 21:24]))
xtable(summary(banco[ , 25:28]))
xtable(summary(banco[ , 29:31]))

# matriz de correlacao
mc <- cor(banco)
i <- which(mc[ , ] == 1)
ii <- which(mc[ , ] > 0.35)
iii <- which(mc[ , ] < 0.75)

# Nenhuma correlacao acima de 0.75.
i == ii
which(i == ii)
length(i)
length(ii)

# Bom plot, muito ruim
boxplot(banco[ ,2:4], ylim = c(-5,10))

hist(banco[ ,2:4])
