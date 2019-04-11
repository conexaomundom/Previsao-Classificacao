install.packages("xtable")
banco <- creditcard_projeto

attach(banco)
names(banco)

# Resumo das variávies
library(xtable)
xtable(head(banco[ , 1:11]))
xtable(head(banco[ , 12:23]))
xtable(head(banco[ , 23:31]))


# summario, algumas descritivas do banco de dados.
xtable(summary(banco[ , 1:5]))
xtable(summary(banco[ , 6:10]))
xtable(summary(banco[ , 1:5]))
xtable(summary(banco[ , 1:5]))
xtable(summary(banco[ , 12:23]))
xtable(summary(banco[ , 23:31]))
