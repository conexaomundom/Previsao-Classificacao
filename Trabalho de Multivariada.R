
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


