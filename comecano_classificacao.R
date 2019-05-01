# Começando com a questão de classificação
install.packages("fBasics")
install.packages("MASS")
install.packages("rpart")
install.packages("randomForest")
install.packages("glmnet")

rm(list = ls())

library(psych)
library(ggplot2)
library(VGAM)
library(ROCR)
library(pROC)
library(InformationValue)
library(e1071)
library(RSNNS)
##
library(fBasics)
library(rpart)
library(MASS)
library(caret)
library(randomForest)
library(glmnet)



rmse <- function(y,y_hat){ sqrt(mean((y - y_hat)^2)) }
rae <- function(y, y_hat){ sum(abs(y_hat - y)) / sum(abs(mean(y) - y)) }
rrse <- function(y, y_hat){ sum((y_hat - y)^2) / sum((mean(y) - y)^2) }
mae <- function(y, y_hat){ sum( abs(y - y_hat)) /length(y) }



banco <- creditcard_projeto
attach(banco)
names(banco)
#####################################################################
# Olhando quais variáveis assumem NA.
# which(apply(banco,2,is.na) == TRUE)
# integer(0)
basicStats(banco)
banco <- na.omit(banco)
#####################################################################

### Com Amostra balanceada , com todos, com as variáveis simétricas.
### Com amostra Pouco balanceada, com todos, com as variáveis simétricas.
### Com amostra desbalanceada, com todos, com as variáveis simétricas.

################################################################################################
### hist com todas as variáveis explicativas e procurar as mais simetricas. Olhando a simetria das variaveis##
################################################################################################
par(mfrow = c(3,2))
apply(banco[1:6], 2, "hist") # "Time" "V1" "V2" "V3" "V4" "V5"
apply(banco[7:12], 2, "hist") # "V6" "V7" "V8" "V9"  "V10" "V11"
apply(banco[13:18], 2, "hist")# "V12" "V13" "V14" "V15" "V16" "V17"
apply(banco[19:24], 2, "hist") # "V18" "V19" "V20" "V21" "V22" "V23"
apply(banco[25:30], 2, "hist") # "V24" "V25" "V26" "V27" "V28" "Amount"
# As variáveis mais simetricas foram:
par(mfrow = c(3,3))
hist(V4); hist(V9); hist(V11); hist(V13); hist(V15); hist(V19); hist(V24)

###############################################################################################
## Selecionando o banco balanceado, pouco balanceado e muito desbalanceado.                 ###
###############################################################################################
## São 387 que em Class assume 1.   # São 227459 que em Class assume 0.
vec <- which(Class == 1);          posicoes <- which(Class == 0)
# Pegando uma quantidade de 1's porque o banco é muito desbalanceado, mas estamos balanceando 
# length(posicoes)/length(vec) = 587.7494. tem 587.7494 cerca de 99,8% de 0's
# em Class.
set.seed(1992) # Balanceado.
vec1 <- sample(x = posicoes, size = length(vec))
set.seed(1993) # Pouco  Balanceado.
vec2 <- sample(x = posicoes, size = 2 * length(vec))
set.seed(1994) # Muito  Desalanceado.
vec3 <- sample(x = posicoes, size = 3 * length(vec))

po1 <- c(vec,vec1)
po2 <- c(vec,vec2)
po3 <- c(vec,vec3)
# Banco selecionado.
banco1 <- banco[po1, ] 
banco2 <- banco[po2, ] 
banco3 <- banco[po3, ] 
# BAncos a simetria das variaveis     
banco4 <- cbind(banco1$V4, banco1$V9, banco1$V11, banco1$V13, banco1$V15, banco1$V19, banco1$V24)
banco5 <- cbind(banco2$V4, banco2$V9, banco2$V11, banco2$V13, banco2$V15, banco2$V19, banco2$V24)
banco6 <- cbind(banco3$V4, banco3$V9, banco3$V11, banco3$V13, banco3$V15, banco3$V19, banco3$V24)

''

# Separando os 10 folds.
require(caret); 
flds1 <- createDataPartition(banco1$Class, times = cv, p = 0.2, list = TRUE)
flds2 <- createDataPartition(banco2$Class, times = cv, p = 0.2, list = TRUE)
flds3 <- createDataPartition(banco3$Class, times = cv, p = 0.2, list = TRUE)


################################################################
# Lista com os elementos separados para treino. # fold1
################################################################
train1 <- banco1[-flds1[[1]], ]
train2 <- banco1[-flds1[[2]], ]
train3 <- banco1[-flds1[[3]], ]
train4 <- banco1[-flds1[[4]], ]
train5 <- banco1[-flds1[[5]], ]
train6 <- banco1[-flds1[[6]], ]
train7 <- banco1[-flds1[[7]], ]
train8 <- banco1[-flds1[[8]], ]
train9 <- banco1[-flds1[[9]], ]
train10 <- banco1[-flds1[[10]], ]
mat_treino <- list(train1, train2, train3, train4, train5, train6, train7, train8, train9, train10)
# Lista com os elementos separados para teste.
teste1 <- banco1[flds1[[1]], ]
teste2 <- banco1[flds1[[2]], ]
teste3 <- banco1[flds1[[3]], ]
teste4 <- banco1[flds1[[4]], ]
teste5 <- banco1[flds1[[5]], ]
teste6 <- banco1[flds1[[6]], ]
teste7 <- banco1[flds1[[7]], ]
teste8 <- banco1[flds1[[8]], ]
teste9 <- banco1[flds1[[9]], ]
teste10 <- banco1[flds1[[10]], ]
mat_teste <- list(teste1, teste2, teste3, teste4, teste5, teste6, teste7, teste8, teste9, teste10)


################################################################
# Lista com os elementos separados para treino. # fold2
################################################################
train21 <- banco2[-flds2[[1]], ]
train22 <- banco2[-flds2[[2]], ]
train23 <- banco2[-flds2[[3]], ]
train24 <- banco2[-flds2[[4]], ]
train25 <- banco2[-flds2[[5]], ]
train26 <- banco2[-flds2[[6]], ]
train27 <- banco2[-flds2[[7]], ]
train28 <- banco2[-flds2[[8]], ]
train29 <- banco2[-flds2[[9]], ]
train210 <- banco2[-flds2[[10]], ]
mat_treino2 <- list(train1, train2, train3, train4, train5, train6, train7, train8, train9, train10)
# Lista com os elementos separados para teste.
teste21 <- banco2[flds2[[1]], ]
teste22 <- banco2[flds2[[2]], ]
teste23 <- banco2[flds2[[3]], ]
teste24 <- banco2[flds2[[4]], ]
teste25 <- banco2[flds2[[5]], ]
teste26 <- banco2[flds2[[6]], ]
teste27 <- banco2[flds2[[7]], ]
teste28 <- banco2[flds2[[8]], ]
teste29 <- banco2[flds2[[9]], ]
teste210 <- banco2[flds2[[10]], ]
mat_teste2 <- list(teste21, teste22, teste23, teste24, teste25, teste26, teste27, teste28, teste29, teste210)

################################################################
# Lista com os elementos separados para treino. # fold3
################################################################
train31 <- banco3[-flds3[[1]], ]
train32 <- banco3[-flds3[[2]], ]
train33 <- banco3[-flds3[[3]], ]
train34 <- banco3[-flds3[[4]], ]
train35 <- banco3[-flds3[[5]], ]
train36 <- banco3[-flds3[[6]], ]
train37 <- banco3[-flds3[[7]], ]
train38 <- banco3[-flds3[[8]], ]
train39 <- banco3[-flds3[[9]], ]
train310 <- banco3[-flds3[[10]], ]
mat_treino3 <- list(train31, train32, train33, train34, train35, train36, train37, train38, train39, train310)
# Lista com os elementos separados para teste.
teste31 <- banco3[flds3[[1]], ]
teste32 <- banco3[flds3[[2]], ]
teste33 <- banco3[flds3[[3]], ]
teste34 <- banco3[flds3[[4]], ]
teste35 <- banco3[flds3[[5]], ]
teste36 <- banco3[flds3[[6]], ]
teste37 <- banco3[flds3[[7]], ]
teste38 <- banco3[flds3[[8]], ]
teste39 <- banco3[flds3[[9]], ]
teste310 <- banco3[flds3[[10]], ]
mat_teste3 <- list(teste31, teste32, teste33, teste34, teste35, teste36, teste37, teste38, teste39, teste310)


################################################################################################
##                         Fazendo com a distribuição binomial com função de ligação logit    ##
################################################################################################
# Modelo Amostra balanceada, Pouco balanceada e desbalanceada Analise Binomial Logit
m1 <- glm(formula = Class ~ Time + V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + V10 + V11 + V12 + V13 + 
                V14 + V15 + V16 + V17 + V18 + V19 + V20 + V21 + V22 + V23 + V24 + V25 + V26 + V27 + 
                V28 + Amount,family = binomial(link = "logit"), data = banco1); # summary(mabbl1)
t1 <- predict(m1, banco1, type = "response")
rmse1[i,j] <- RMSE(t1,mat_teste[[i]][1])
rae1[i,j] <- rae(t1,mat_teste[[i]][1])
mae1[i,j] <- mae(t1,mat_teste[[i]][1])
rrse1[i,j] <- rrse(t1,mat_teste[[i]][1])
m2 <- glm(formula = Class ~ Time + V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + V10 + V11 + V12 + V13 + 
                V14 + V15 + V16 + V17 + V18 + V19 + V20 + V21 + V22 + V23 + V24 + V25 + V26 + V27 + 
                V28 + Amount,family = binomial(link = "logit"), data = banco2); # summary(mabbl2)
t2 <- predict(m2, banco2, type = "response")
rmse1[i,j] <- RMSE(t1,mat_teste[[i]][1])
rae1[i,j] <- rae(t1,mat_teste[[i]][1])
mae1[i,j] <- mae(t1,mat_teste[[i]][1])
rrse1[i,j] <- rrse(t1,mat_teste[[i]][1])
m3 <- glm(formula = Class ~ Time + V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + V10 + V11 + V12 + V13 + 
                V14 + V15 + V16 + V17 + V18 + V19 + V20 + V21 + V22 + V23 + V24 + V25 + V26 + V27 + 
                V28 + Amount,family = binomial(link = "logit"), data = banco3); # summary(mabbl3)
t3 <- predict(m3, banco3, type = "response")
rmse1[i,j] <- RMSE(t1,mat_teste[[i]][1])
rae1[i,j] <- rae(t1,mat_teste[[i]][1])
mae1[i,j] <- mae(t1,mat_teste[[i]][1])
rrse1[i,j] <- rrse(t1,mat_teste[[i]][1])
##########################
## VARIAVEIS SIMETRICAS ##
##########################
# Modelo Amostra balanceada, Pouco balanceada, desbalanceada Binomial Logit de Variáveis Simetricas.
m4 <- glm(formula = Class ~ V4 + V9 + V11 + V13 + V15 + V19 + V24,family = binomial(link = "logit"), data = banco1); # summary(mabbl11)
fited4 <- predict(m4, banco1, type = "response")
rmse1[i,j] <- RMSE(t1,mat_teste[[i]][1])
rae1[i,j] <- rae(t1,mat_teste[[i]][1])
mae1[i,j] <- mae(t1,mat_teste[[i]][1])
rrse1[i,j] <- rrse(t1,mat_teste[[i]][1])
m5 <- glm(formula = Class ~ V4 + V9 + V11 + V13 + V15 + V19 + V24,family = binomial(link = "logit"), data = banco2); # summary(mabbl22)
fited5 <- predict(m5, banco2, type = "response")
rmse1[i,j] <- RMSE(t1,mat_teste[[i]][1])
rae1[i,j] <- rae(t1,mat_teste[[i]][1])
mae1[i,j] <- mae(t1,mat_teste[[i]][1])
rrse1[i,j] <- rrse(t1,mat_teste[[i]][1])
m6 <- glm(formula = Class ~ V4 + V9 + V11 + V13 + V15 + V19 + V24,family = binomial(link = "logit"), data = banco3); # summary(mabbl33)
fited6 <- predict(m6, banco3, type = "response")
rmse1[i,j] <- RMSE(t1,mat_teste[[i]][1])
rae1[i,j] <- rae(t1,mat_teste[[i]][1])
mae1[i,j] <- mae(t1,mat_teste[[i]][1])
rrse1[i,j] <- rrse(t1,mat_teste[[i]][1])

################################################################################################
##          Fazendo com a distribuição binomial com função de ligação probit               ##
################################################################################################

# Modelo Amostra balanceada, Pouco balanceada e desbalanceadaAnalise Analise de discriminante Quadrático  
m7 <- glm(formula = Class ~ Time + V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + V10 + V11 + V12 + V13 + 
                V14 + V15 + V16 + V17 + V18 + V19 + V20 + V21 + V22 + V23 + V24 + V25 + V26 + V27 + 
                V28 + Amount,family = binomial(link = "probit"), data = banco1); # summary(mabbp1)
fited4 <- predict(m4, banco1, type = "response")
rmse1[i,j] <- RMSE(t1,mat_teste[[i]][1])
rae1[i,j] <- rae(t1,mat_teste[[i]][1])
mae1[i,j] <- mae(t1,mat_teste[[i]][1])
rrse1[i,j] <- rrse(t1,mat_teste[[i]][1])
m7<- glm(formula = Class ~ Time + V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + V10 + V11 + V12 + V13 + 
                V14 + V15 + V16 + V17 + V18 + V19 + V20 + V21 + V22 + V23 + V24 + V25 + V26 + V27 + 
                V28 + Amount,family = binomial(link = "probit"), data = banco2); # summary(mabbp2)
fited5 <- predict(m5, banco2, type = "response")
rmse1[i,j] <- RMSE(t1,mat_teste[[i]][1])
rae1[i,j] <- rae(t1,mat_teste[[i]][1])
mae1[i,j] <- mae(t1,mat_teste[[i]][1])
rrse1[i,j] <- rrse(t1,mat_teste[[i]][1])
m9 <- glm(formula = Class ~ Time + V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + V10 + V11 + V12 + V13 + 
                V14 + V15 + V16 + V17 + V18 + V19 + V20 + V21 + V22 + V23 + V24 + V25 + V26 + V27 + 
                V28 + Amount,family = binomial(link = "probit"), data = banco3); # summary(mabbp3)
fited6 <- predict(m6, banco3, type = "response")
rmse1[i,j] <- RMSE(t1,mat_teste[[i]][1])
rae1[i,j] <- rae(t1,mat_teste[[i]][1])
mae1[i,j] <- mae(t1,mat_teste[[i]][1])
rrse1[i,j] <- rrse(t1,mat_teste[[i]][1])
########################################
##  VARIAVEIS SIMETRICAS             ##
####################################### 
# Modelo Amostra balanceada, Pouco balanceada e desbalanceadaAnalise Simetricas Binomial Probit
m10 <- glm(formula = Class ~ V4 + V9 + V11 + V13 + V15 + V19 + V24,family = binomial(link = "probit"), data = banco1); #summary(mabbp11)
fited6 <- predict(m6, banco3, type = "response")
rmse1[i,j] <- RMSE(t1,mat_teste[[i]][1])
rae1[i,j] <- rae(t1,mat_teste[[i]][1])
mae1[i,j] <- mae(t1,mat_teste[[i]][1])
rrse1[i,j] <- rrse(t1,mat_teste[[i]][1])
m11 <- glm(formula = Class ~ V4 + V9 + V11 + V13 + V15 + V19 + V24,family = binomial(link = "probit"), data = banco2); # summary(mabbp22)
fited6 <- predict(m6, banco3, type = "response")
rmse1[i,j] <- RMSE(t1,mat_teste[[i]][1])
rae1[i,j] <- rae(t1,mat_teste[[i]][1])
mae1[i,j] <- mae(t1,mat_teste[[i]][1])
rrse1[i,j] <- rrse(t1,mat_teste[[i]][1])
m12 <- glm(formula = Class ~ V4 + V9 + V11 + V13 + V15 + V19 + V24,family = binomial(link = "probit"), data = banco3); # summary(mabbp33)
fited6 <- predict(m6, banco3, type = "response")
rmse1[i,j] <- RMSE(t1,mat_teste[[i]][1])
rae1[i,j] <- rae(t1,mat_teste[[i]][1])
mae1[i,j] <- mae(t1,mat_teste[[i]][1])
rrse1[i,j] <- rrse(t1,mat_teste[[i]][1])

################################################################################################
##                         ANALISE LINEAR DE DISCRIMINANTE                                    ##
################################################################################################
# Modelo Amostra balanceada, Pouco balanceada e desbalanceadaAnalise Linear Discriminante.
m13 <- lda(Class ~ Time + V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + V10 + V11 + V12 + V13 + 
                 V14 + V15 + V16 + V17 + V18 + V19 + V20 + V21 + V22 + V23 + V24 + V25 + V26 + V27 + 
                 V28 + Amount, data = banco1); # summary(mabald1)
fited6 <- predict(m6, banco3, type = "response")
rmse1[i,j] <- RMSE(t1,mat_teste[[i]][1])
rae1[i,j] <- rae(t1,mat_teste[[i]][1])
mae1[i,j] <- mae(t1,mat_teste[[i]][1])
rrse1[i,j] <- rrse(t1,mat_teste[[i]][1])
m14 <- lda(formula = Class ~ Time + V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + V10 + V11 + V12 + V13 + 
                 V14 + V15 + V16 + V17 + V18 + V19 + V20 + V21 + V22 + V23 + V24 + V25 + V26 + V27 + 
                 V28 + Amount, data = banco2); # summary(mabald2)
fited6 <- predict(m6, banco3, type = "response")
rmse1[i,j] <- RMSE(t1,mat_teste[[i]][1])
rae1[i,j] <- rae(t1,mat_teste[[i]][1])
mae1[i,j] <- mae(t1,mat_teste[[i]][1])
rrse1[i,j] <- rrse(t1,mat_teste[[i]][1])
m15 <- lda(formula = Class ~ Time + V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + V10 + V11 + V12 + V13 + 
                 V14 + V15 + V16 + V17 + V18 + V19 + V20 + V21 + V22 + V23 + V24 + V25 + V26 + V27 + 
                 V28 + Amount, data = banco3); # summary(mabald3)
fited6 <- predict(m6, banco3, type = "response")
rmse1[i,j] <- RMSE(t1,mat_teste[[i]][1])
rae1[i,j] <- rae(t1,mat_teste[[i]][1])
mae1[i,j] <- mae(t1,mat_teste[[i]][1])
rrse1[i,j] <- rrse(t1,mat_teste[[i]][1])
##############################################
## VARIAVEIS SIMETRICAS                    ##
#############################################
# Modelo Amostra balanceada, Pouco balanceada e desbalanceada Analise Linear Discriminante.
m16 <- lda(formula = Class ~ V4 + V9 + V11 + V13 + V15 + V19 + V24,data = banco1); # summary(mabald11)
fited6 <- predict(m6, banco3, type = "response")
rmse1[i,j] <- RMSE(t1,mat_teste[[i]][1])
rae1[i,j] <- rae(t1,mat_teste[[i]][1])
mae1[i,j] <- mae(t1,mat_teste[[i]][1])
rrse1[i,j] <- rrse(t1,mat_teste[[i]][1])
m17 <- lda(formula = Class ~ V4 + V9 + V11 + V13 + V15 + V19 + V24,data = banco2); # summary(mabald22)
fited6 <- predict(m6, banco3, type = "response")
rmse1[i,j] <- RMSE(t1,mat_teste[[i]][1])
rae1[i,j] <- rae(t1,mat_teste[[i]][1])
mae1[i,j] <- mae(t1,mat_teste[[i]][1])
rrse1[i,j] <- rrse(t1,mat_teste[[i]][1])
m18 <- lda(formula = Class ~ V4 + V9 + V11 + V13 + V15 + V19 + V24,data = banco3); # summary(mabald33)
fited6 <- predict(m6, banco3, type = "response")
rmse1[i,j] <- RMSE(t1,mat_teste[[i]][1])
rae1[i,j] <- rae(t1,mat_teste[[i]][1])
mae1[i,j] <- mae(t1,mat_teste[[i]][1])
rrse1[i,j] <- rrse(t1,mat_teste[[i]][1])

##############################################################################################
##                         ANALISE DE DISCRIMINANTE Quadrático                              ##
##############################################################################################
# Modelo Amostra balanceada, Pouco balanceada e desbalanceada Analise Linear Discriminante.
m19 <- qda(Class ~ Time + V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + V10 + V11 + V12 + V13 + 
                 V14 + V15 + V16 + V17 + V18 + V19 + V20 + V21 + V22 + V23 + V24 + V25 + V26 + V27 + 
                 V28 + Amount, data = banco1); #summary(mabaqd1)
fited6 <- predict(m6, banco3, type = "response")
rmse1[i,j] <- RMSE(t1,mat_teste[[i]][1])
rae1[i,j] <- rae(t1,mat_teste[[i]][1])
mae1[i,j] <- mae(t1,mat_teste[[i]][1])
rrse1[i,j] <- rrse(t1,mat_teste[[i]][1])
m20 <- qda(formula = Class ~ Time + V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + V10 + V11 + V12 + V13 + 
                 V14 + V15 + V16 + V17 + V18 + V19 + V20 + V21 + V22 + V23 + V24 + V25 + V26 + V27 + 
                 V28 + Amount, data = banco2); #summary(mabaqd2)
fited6 <- predict(m6, banco3, type = "response")
rmse1[i,j] <- RMSE(t1,mat_teste[[i]][1])
rae1[i,j] <- rae(t1,mat_teste[[i]][1])
mae1[i,j] <- mae(t1,mat_teste[[i]][1])
rrse1[i,j] <- rrse(t1,mat_teste[[i]][1])
m21 <- qda(formula = Class ~ Time + V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + V10 + V11 + V12 + V13 + 
                 V14 + V15 + V16 + V17 + V18 + V19 + V20 + V21 + V22 + V23 + V24 + V25 + V26 + V27 + 
                 V28 + Amount, data = banco3); #summary(mabaqd3)
fited6 <- predict(m6, banco3, type = "response")
rmse1[i,j] <- RMSE(t1,mat_teste[[i]][1])
rae1[i,j] <- rae(t1,mat_teste[[i]][1])
mae1[i,j] <- mae(t1,mat_teste[[i]][1])
rrse1[i,j] <- rrse(t1,mat_teste[[i]][1])
#######################################
##      VARIAVEIS SIMETRICAS         ##
#######################################
# Modelo Amostra balanceada, Pouco balanceada e desbalanceada Analise Linear Discriminante.
m22 <- qda(formula = Class ~ V4 + V9 + V11 + V13 + V15 + V19 + V24,data = banco1);# summary(mabaqd11)
fited6 <- predict(m6, banco3, type = "response")
rmse1[i,j] <- RMSE(t1,mat_teste[[i]][1])
rae1[i,j] <- rae(t1,mat_teste[[i]][1])
mae1[i,j] <- mae(t1,mat_teste[[i]][1])
rrse1[i,j] <- rrse(t1,mat_teste[[i]][1])
m23 <- qda(formula = Class ~ V4 + V9 + V11 + V13 + V15 + V19 + V24,data = banco2);# summary(mabaqd22)
fited6 <- predict(m6, banco3, type = "response")
rmse1[i,j] <- RMSE(t1,mat_teste[[i]][1])
rae1[i,j] <- rae(t1,mat_teste[[i]][1])
mae1[i,j] <- mae(t1,mat_teste[[i]][1])
rrse1[i,j] <- rrse(t1,mat_teste[[i]][1])
m24 <- qda(formula = Class ~ V4 + V9 + V11 + V13 + V15 + V19 + V24,data = banco3);# summary(mabaqd33)
fited6 <- predict(m6, banco3, type = "response")
rmse1[i,j] <- RMSE(t1,mat_teste[[i]][1])
rae1[i,j] <- rae(t1,mat_teste[[i]][1])
mae1[i,j] <- mae(t1,mat_teste[[i]][1])
rrse1[i,j] <- rrse(t1,mat_teste[[i]][1])

########################################################################################
## ############################## # "5-NN",
######################################################################################
m25 = knnreg(banco1[1:30], as.numeric(banco1[31]))
fited6 <- predict(m6, banco3, type = "response")
rmse1[i,j] <- RMSE(t1,mat_teste[[i]][1])
rae1[i,j] <- rae(t1,mat_teste[[i]][1])
mae1[i,j] <- mae(t1,mat_teste[[i]][1])
rrse1[i,j] <- rrse(t1,mat_teste[[i]][1])
m26 = knnreg(banco2[1:30], as.numeric(banco2[31]))
fited6 <- predict(m6, banco3, type = "response")
rmse1[i,j] <- RMSE(t1,mat_teste[[i]][1])
rae1[i,j] <- rae(t1,mat_teste[[i]][1])
mae1[i,j] <- mae(t1,mat_teste[[i]][1])
rrse1[i,j] <- rrse(t1,mat_teste[[i]][1])
m27 = knnreg(banco3[1:30], as.numeric(banco3[31]))
fited6 <- predict(m6, banco3, type = "response")
rmse1[i,j] <- RMSE(t1,mat_teste[[i]][1])
rae1[i,j] <- rae(t1,mat_teste[[i]][1])
mae1[i,j] <- mae(t1,mat_teste[[i]][1])
rrse1[i,j] <- rrse(t1,mat_teste[[i]][1])

#######################################
##      VARIAVEIS SIMETRICAS         ##
#######################################
m28 = knnreg(banco4, as.numeric(banco1$Class))
fited6 <- predict(m6, banco3, type = "response")
rmse1[i,j] <- RMSE(t1,mat_teste[[i]][1])
rae1[i,j] <- rae(t1,mat_teste[[i]][1])
mae1[i,j] <- mae(t1,mat_teste[[i]][1])
rrse1[i,j] <- rrse(t1,mat_teste[[i]][1])
m29 = knnreg(banco5, as.numeric(banco2$Class))
fited6 <- predict(m6, banco3, type = "response")
rmse1[i,j] <- RMSE(t1,mat_teste[[i]][1])
rae1[i,j] <- rae(t1,mat_teste[[i]][1])
mae1[i,j] <- mae(t1,mat_teste[[i]][1])
rrse1[i,j] <- rrse(t1,mat_teste[[i]][1])
m30 = knnreg(banco6, as.numeric(banco3$Class))
fited6 <- predict(m6, banco3, type = "response")
rmse1[i,j] <- RMSE(t1,mat_teste[[i]][1])
rae1[i,j] <- rae(t1,mat_teste[[i]][1])
mae1[i,j] <- mae(t1,mat_teste[[i]][1])
rrse1[i,j] <- rrse(t1,mat_teste[[i]][1])

########################################################################################
## ##############################  "3-NN",
######################################################################################
m31 = knnreg(banco1[1:30], as.numeric(banco1[31]), k = 3)
fited6 <- predict(m6, banco3, type = "response")
rmse1[i,j] <- RMSE(t1,mat_teste[[i]][1])
rae1[i,j] <- rae(t1,mat_teste[[i]][1])
mae1[i,j] <- mae(t1,mat_teste[[i]][1])
rrse1[i,j] <- rrse(t1,mat_teste[[i]][1])
m32 = knnreg(banco2[1:30], as.numeric(banco2[31]), k = 3)
fited6 <- predict(m6, banco3, type = "response")
rmse1[i,j] <- RMSE(t1,mat_teste[[i]][1])
rae1[i,j] <- rae(t1,mat_teste[[i]][1])
mae1[i,j] <- mae(t1,mat_teste[[i]][1])
rrse1[i,j] <- rrse(t1,mat_teste[[i]][1])
m33 = knnreg(banco3[1:30], as.numeric(banco3[31]), k = 3)
fited6 <- predict(m6, banco3, type = "response")
rmse1[i,j] <- RMSE(t1,mat_teste[[i]][1])
rae1[i,j] <- rae(t1,mat_teste[[i]][1])
mae1[i,j] <- mae(t1,mat_teste[[i]][1])
rrse1[i,j] <- rrse(t1,mat_teste[[i]][1])

#######################################
##      VARIAVEIS SIMETRICAS         ##
#######################################
m34 = knnreg(banco4, as.numeric(banco1$Class), k = 3)
fited6 <- predict(m6, banco3, type = "response")
rmse1[i,j] <- RMSE(t1,mat_teste[[i]][1])
rae1[i,j] <- rae(t1,mat_teste[[i]][1])
mae1[i,j] <- mae(t1,mat_teste[[i]][1])
rrse1[i,j] <- rrse(t1,mat_teste[[i]][1])
m35 = knnreg(banco5, as.numeric(banco2$Class), k = 3)
fited6 <- predict(m6, banco3, type = "response")
rmse1[i,j] <- RMSE(t1,mat_teste[[i]][1])
rae1[i,j] <- rae(t1,mat_teste[[i]][1])
mae1[i,j] <- mae(t1,mat_teste[[i]][1])
rrse1[i,j] <- rrse(t1,mat_teste[[i]][1])
m36 = knnreg(banco6, as.numeric(banco3$Class), k = 3)
fited6 <- predict(m6, banco3, type = "response")
rmse1[i,j] <- RMSE(t1,mat_teste[[i]][1])
rae1[i,j] <- rae(t1,mat_teste[[i]][1])
mae1[i,j] <- mae(t1,mat_teste[[i]][1])
rrse1[i,j] <- rrse(t1,mat_teste[[i]][1])

########################################################################################
## ############################## "10-NN"
######################################################################################
m37 = knnreg(banco1[1:30], as.numeric(banco1[31]), k = 10)
fited6 <- predict(m6, banco3, type = "response")
rmse1[i,j] <- RMSE(t1,mat_teste[[i]][1])
rae1[i,j] <- rae(t1,mat_teste[[i]][1])
mae1[i,j] <- mae(t1,mat_teste[[i]][1])
rrse1[i,j] <- rrse(t1,mat_teste[[i]][1])
m38 = knnreg(banco2[1:30], as.numeric(banco2[31]), k = 10)
fited6 <- predict(m6, banco3, type = "response")
rmse1[i,j] <- RMSE(t1,mat_teste[[i]][1])
rae1[i,j] <- rae(t1,mat_teste[[i]][1])
mae1[i,j] <- mae(t1,mat_teste[[i]][1])
rrse1[i,j] <- rrse(t1,mat_teste[[i]][1])
m39 = knnreg(banco3[1:30], as.numeric(banco3[31]), k = 10)
fited6 <- predict(m6, banco3, type = "response")
rmse1[i,j] <- RMSE(t1,mat_teste[[i]][1])
rae1[i,j] <- rae(t1,mat_teste[[i]][1])
mae1[i,j] <- mae(t1,mat_teste[[i]][1])
rrse1[i,j] <- rrse(t1,mat_teste[[i]][1])

#######################################
##      VARIAVEIS SIMETRICAS         ##
#######################################
m40 = knnreg(banco4, as.numeric(banco1$Class), k = 10)
fited6 <- predict(m6, banco3, type = "response")
rmse1[i,j] <- RMSE(t1,mat_teste[[i]][1])
rae1[i,j] <- rae(t1,mat_teste[[i]][1])
mae1[i,j] <- mae(t1,mat_teste[[i]][1])
rrse1[i,j] <- rrse(t1,mat_teste[[i]][1])
m41 = knnreg(banco5, as.numeric(banco2$Class), k = 10)
fited6 <- predict(m6, banco3, type = "response")
rmse1[i,j] <- RMSE(t1,mat_teste[[i]][1])
rae1[i,j] <- rae(t1,mat_teste[[i]][1])
mae1[i,j] <- mae(t1,mat_teste[[i]][1])
rrse1[i,j] <- rrse(t1,mat_teste[[i]][1])
m42 = knnreg(banco6, as.numeric(banco3$Class), k = 10)
fited6 <- predict(m6, banco3, type = "response")
rmse1[i,j] <- RMSE(t1,mat_teste[[i]][1])
rae1[i,j] <- rae(t1,mat_teste[[i]][1])
mae1[i,j] <- mae(t1,mat_teste[[i]][1])
rrse1[i,j] <- rrse(t1,mat_teste[[i]][1])

########################################################################################
## ############################## # Regressão Ridge
#######################################################################################
m43 = glmnet(banco1[1:30], as.numeric(banco1[31]), alpha = 0) # Regressão Ridge
fited6 <- predict(m6, banco3, type = "response")
rmse1[i,j] <- RMSE(t1,mat_teste[[i]][1])
rae1[i,j] <- rae(t1,mat_teste[[i]][1])
mae1[i,j] <- mae(t1,mat_teste[[i]][1])
rrse1[i,j] <- rrse(t1,mat_teste[[i]][1])
m44 = glmnet(banco2[1:30], as.numeric(banco2[31]), alpha = 0) # Regressão Ridge
fited6 <- predict(m6, banco3, type = "response")
rmse1[i,j] <- RMSE(t1,mat_teste[[i]][1])
rae1[i,j] <- rae(t1,mat_teste[[i]][1])
mae1[i,j] <- mae(t1,mat_teste[[i]][1])
rrse1[i,j] <- rrse(t1,mat_teste[[i]][1])
m45 = glmnet(banco3[1:30], as.numeric(banco3[31]), alpha = 0) # Regressão Ridge
fited6 <- predict(m6, banco3, type = "response")
rmse1[i,j] <- RMSE(t1,mat_teste[[i]][1])
rae1[i,j] <- rae(t1,mat_teste[[i]][1])
mae1[i,j] <- mae(t1,mat_teste[[i]][1])
rrse1[i,j] <- rrse(t1,mat_teste[[i]][1])

## Melhor lambda a regressão ridge
lambda.ridge1 = cv.glmnet(banco1[1:30], unlist(banco1[31]), alpha = 0) # Melhor lambda para a regressão ridge
lambda.ridge2 = cv.glmnet(banco2[1:30], unlist(banco2[31]), alpha = 0) # Melhor lambda para a regressão ridge
lambda.ridge3 = cv.glmnet(banco3[1:30], unlist(banco3[31]), alpha = 0) # Melhor lambda para a regressão ridge
####################################
##      VARIAVEIS SIMETRICAS      ##
####################################
m46 = glmnet(banco4, as.numeric(banco1$Class), alpha = 0) # Regressão Ridge
fited6 <- predict(m6, banco3, type = "response")
rmse1[i,j] <- RMSE(t1,mat_teste[[i]][1])
rae1[i,j] <- rae(t1,mat_teste[[i]][1])
mae1[i,j] <- mae(t1,mat_teste[[i]][1])
rrse1[i,j] <- rrse(t1,mat_teste[[i]][1])
m47 = glmnet(banco5, as.numeric(banco2$Class), alpha = 0) # Regressão Ridge
fited6 <- predict(m6, banco3, type = "response")
rmse1[i,j] <- RMSE(t1,mat_teste[[i]][1])
rae1[i,j] <- rae(t1,mat_teste[[i]][1])
mae1[i,j] <- mae(t1,mat_teste[[i]][1])
rrse1[i,j] <- rrse(t1,mat_teste[[i]][1])
m48 = glmnet(banco6, as.numeric(banco3$Class), alpha = 0) # Regressão Ridge
fited6 <- predict(m6, banco3, type = "response")
rmse1[i,j] <- RMSE(t1,mat_teste[[i]][1])
rae1[i,j] <- rae(t1,mat_teste[[i]][1])
mae1[i,j] <- mae(t1,mat_teste[[i]][1])
rrse1[i,j] <- rrse(t1,mat_teste[[i]][1])


## Melhor lambda a regressão ridge
lambda.ridge11 = cv.glmnet(banco4, as.numeric(banco1$Class), alpha = 0) # Melhor lambda para a regressão ridge
lambda.ridge22 = cv.glmnet(banco5, as.numeric(banco2$Class), alpha = 0) # Melhor lambda para a regressão ridge
lambda.ridge33 = cv.glmnet(banco6, as.numeric(banco3$Class), alpha = 0) # Melhor lambda para a regressão ridge


########################################################################################
## ############################## # LASSO
#######################################################################################
m49 = glmnet(banco1[1:30], unlist(banco1[31]), alpha = 1) # LASSO
fited6 <- predict(m6, banco3, type = "response")
rmse1[i,j] <- RMSE(t1,mat_teste[[i]][1])
rae1[i,j] <- rae(t1,mat_teste[[i]][1])
mae1[i,j] <- mae(t1,mat_teste[[i]][1])
rrse1[i,j] <- rrse(t1,mat_teste[[i]][1])
m50 = glmnet(banco2[1:30], unlist(banco2[31]), alpha = 1) # LASSO
fited6 <- predict(m6, banco3, type = "response")
rmse1[i,j] <- RMSE(t1,mat_teste[[i]][1])
rae1[i,j] <- rae(t1,mat_teste[[i]][1])
mae1[i,j] <- mae(t1,mat_teste[[i]][1])
rrse1[i,j] <- rrse(t1,mat_teste[[i]][1])
m51 = glmnet(banco3[1:30], unlist(banco3[31]), alpha = 1) # LASSO
fited6 <- predict(m6, banco3, type = "response")
rmse1[i,j] <- RMSE(t1,mat_teste[[i]][1])
rae1[i,j] <- rae(t1,mat_teste[[i]][1])
mae1[i,j] <- mae(t1,mat_teste[[i]][1])
rrse1[i,j] <- rrse(t1,mat_teste[[i]][1])

# Melhor lambda para o LASSO
lambda.lasso1 = cv.glmnet(banco1[1:30], unlist(banco1[31]), alpha = 1) # Melhor lambda para o LASSO
lambda.lasso2 = cv.glmnet(banco2[1:30], unlist(banco2[31]), alpha = 1) # Melhor lambda para o LASSO
lambda.lasso3 = cv.glmnet(banco3[1:30], unlist(banco3[31]), alpha = 1) # Melhor lambda para o LASSO
#######################################
##      VARIAVEIS SIMETRICAS         ##
#######################################
m52 = glmnet(banco4, as.numeric(banco1$Class), alpha = 1) # LASSO
fited6 <- predict(m6, banco3, type = "response")
rmse1[i,j] <- RMSE(t1,mat_teste[[i]][1])
rae1[i,j] <- rae(t1,mat_teste[[i]][1])
mae1[i,j] <- mae(t1,mat_teste[[i]][1])
rrse1[i,j] <- rrse(t1,mat_teste[[i]][1])
m53 = glmnet(banco5, as.numeric(banco2$Class), alpha = 1) # LASSO
fited6 <- predict(m6, banco3, type = "response")
rmse1[i,j] <- RMSE(t1,mat_teste[[i]][1])
rae1[i,j] <- rae(t1,mat_teste[[i]][1])
mae1[i,j] <- mae(t1,mat_teste[[i]][1])
rrse1[i,j] <- rrse(t1,mat_teste[[i]][1])
m54 = glmnet(banco6, as.numeric(banco3$Class), alpha = 1) # LASSO
fited6 <- predict(m6, banco3, type = "response")
rmse1[i,j] <- RMSE(t1,mat_teste[[i]][1])
rae1[i,j] <- rae(t1,mat_teste[[i]][1])
mae1[i,j] <- mae(t1,mat_teste[[i]][1])
rrse1[i,j] <- rrse(t1,mat_teste[[i]][1])

# Melhor lambda para o LASSO
lambda.lasso11 = cv.glmnet(banco4, as.numeric(banco1$Class), alpha = 1) # Melhor lambda para o LASSO
lambda.lasso22 = cv.glmnet(banco5, as.numeric(banco2$Class), alpha = 1) # Melhor lambda para o LASSO
lambda.lasso33 = cv.glmnet(banco6, as.numeric(banco3$Class), alpha = 1) # Melhor lambda para o LASSO


########################################################################################
## ############################## # ElasticNet
#######################################################################################
m55 = glmnet(banco1[1:30], unlist(banco1[31]), alpha = 0.5) # ElasticNet
fited6 <- predict(m6, banco3, type = "response")
rmse1[i,j] <- RMSE(t1,mat_teste[[i]][1])
rae1[i,j] <- rae(t1,mat_teste[[i]][1])
mae1[i,j] <- mae(t1,mat_teste[[i]][1])
rrse1[i,j] <- rrse(t1,mat_teste[[i]][1])
m56 = glmnet(banco2[1:30], unlist(banco2[31]), alpha = 0.5) # ElasticNet
fited6 <- predict(m6, banco3, type = "response")
rmse1[i,j] <- RMSE(t1,mat_teste[[i]][1])
rae1[i,j] <- rae(t1,mat_teste[[i]][1])
mae1[i,j] <- mae(t1,mat_teste[[i]][1])
rrse1[i,j] <- rrse(t1,mat_teste[[i]][1])
m57 = glmnet(banco3[1:30], unlist(banco3[31]), alpha = 0.5) # ElasticNet
fited6 <- predict(m6, banco3, type = "response")
rmse1[i,j] <- RMSE(t1,mat_teste[[i]][1])
rae1[i,j] <- rae(t1,mat_teste[[i]][1])
mae1[i,j] <- mae(t1,mat_teste[[i]][1])
rrse1[i,j] <- rrse(t1,mat_teste[[i]][1])

## ###### Melhor lambda para a ElasticNet
lambda.elasticnet1 = cv.glmnet(banco1[1:30], unlist(banco1[31]), alpha = 0.5) # Melhor lambda para a ElasticNet
lambda.elasticnet2 = cv.glmnet(banco2[1:30], unlist(banco2[31]), alpha = 0.5) # Melhor lambda para a ElasticNet
lambda.elasticnet3 = cv.glmnet(banco3[1:30], unlist(banco3[31]), alpha = 0.5) # Melhor lambda para a ElasticNet
#######################################
##      VARIAVEIS SIMETRICAS         ##
#######################################
m58 = glmnet(banco4, as.numeric(banco1$Class), alpha = 0.5) # ElasticNet
fited6 <- predict(m6, banco3, type = "response")
rmse1[i,j] <- RMSE(t1,mat_teste[[i]][1])
rae1[i,j] <- rae(t1,mat_teste[[i]][1])
mae1[i,j] <- mae(t1,mat_teste[[i]][1])
rrse1[i,j] <- rrse(t1,mat_teste[[i]][1])
m59 = glmnet(banco5, as.numeric(banco2$Class), alpha = 0.5) # ElasticNet
fited6 <- predict(m6, banco3, type = "response")
rmse1[i,j] <- RMSE(t1,mat_teste[[i]][1])
rae1[i,j] <- rae(t1,mat_teste[[i]][1])
mae1[i,j] <- mae(t1,mat_teste[[i]][1])
rrse1[i,j] <- rrse(t1,mat_teste[[i]][1])
m60 = glmnet(banco6, as.numeric(banco3$Class), alpha = 0.5) # ElasticNet
fited6 <- predict(m6, banco3, type = "response")
rmse1[i,j] <- RMSE(t1,mat_teste[[i]][1])
rae1[i,j] <- rae(t1,mat_teste[[i]][1])
mae1[i,j] <- mae(t1,mat_teste[[i]][1])
rrse1[i,j] <- rrse(t1,mat_teste[[i]][1])

##Melhor lambda para a ElasticNet
lambda.elasticnet11 = cv.glmnet(banco4, as.numeric(banco1$Class), alpha = 0.5) # Melhor lambda para a ElasticNet
lambda.elasticnet22 = cv.glmnet(banco5, as.numeric(banco2$Class), alpha = 0.5) # Melhor lambda para a ElasticNet
lambda.elasticnet33 = cv.glmnet(banco6, as.numeric(banco3$Class), alpha = 0.5) # Melhor lambda para a ElasticNet


########################################################################################
## #############################Cresce a árvore 
#######################################################################################
m61 <- rpart( Class ~ Time + V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + V10 + V11 + V12 + V13 + 
                V14 + V15 + V16 + V17 + V18 + V19 + V20 + V21 + V22 + V23 + V24 + V25 + V26 + V27 + 
                V28 + Amount, data = banco1,method="class")
fited6 <- predict(m6, banco3, type = "response")
rmse1[i,j] <- RMSE(t1,mat_teste[[i]][1])
rae1[i,j] <- rae(t1,mat_teste[[i]][1])
mae1[i,j] <- mae(t1,mat_teste[[i]][1])
rrse1[i,j] <- rrse(t1,mat_teste[[i]][1])
m62 <- rpart( Class ~ Time + V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + V10 + V11 + V12 + V13 + 
                V14 + V15 + V16 + V17 + V18 + V19 + V20 + V21 + V22 + V23 + V24 + V25 + V26 + V27 + 
                V28 + Amount, data = banco2,method="class")
fited6 <- predict(m6, banco3, type = "response")
rmse1[i,j] <- RMSE(t1,mat_teste[[i]][1])
rae1[i,j] <- rae(t1,mat_teste[[i]][1])
mae1[i,j] <- mae(t1,mat_teste[[i]][1])
rrse1[i,j] <- rrse(t1,mat_teste[[i]][1])
m63 <- rpart( Class ~ Time + V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + V10 + V11 + V12 + V13 + 
                V14 + V15 + V16 + V17 + V18 + V19 + V20 + V21 + V22 + V23 + V24 + V25 + V26 + V27 + 
                V28 + Amount, data = banco3,method="class")
fited6 <- predict(m6, banco3, type = "response")
rmse1[i,j] <- RMSE(t1,mat_teste[[i]][1])
rae1[i,j] <- rae(t1,mat_teste[[i]][1])
mae1[i,j] <- mae(t1,mat_teste[[i]][1])
rrse1[i,j] <- rrse(t1,mat_teste[[i]][1])

#######################################
##      VARIAVEIS SIMETRICAS         ##
#######################################
m64 <- rpart(Class ~ V4 + V9 + V11 + V13 + V15 + V19 + V24, data = banco1,method="class")
fited6 <- predict(m6, banco3, type = "response")
rmse1[i,j] <- RMSE(t1,mat_teste[[i]][1])
rae1[i,j] <- rae(t1,mat_teste[[i]][1])
mae1[i,j] <- mae(t1,mat_teste[[i]][1])
rrse1[i,j] <- rrse(t1,mat_teste[[i]][1])
m65 <- rpart(Class ~ V4 + V9 + V11 + V13 + V15 + V19 + V24, data = banco2,method="class")
fited6 <- predict(m6, banco3, type = "response")
rmse1[i,j] <- RMSE(t1,mat_teste[[i]][1])
rae1[i,j] <- rae(t1,mat_teste[[i]][1])
mae1[i,j] <- mae(t1,mat_teste[[i]][1])
rrse1[i,j] <- rrse(t1,mat_teste[[i]][1])
m66 <- rpart(Class ~ V4 + V9 + V11 + V13 + V15 + V19 + V24, data = banco3,method="class")
fited6 <- predict(m6, banco3, type = "response")
rmse1[i,j] <- RMSE(t1,mat_teste[[i]][1])
rae1[i,j] <- rae(t1,mat_teste[[i]][1])
mae1[i,j] <- mae(t1,mat_teste[[i]][1])
rrse1[i,j] <- rrse(t1,mat_teste[[i]][1])

########################################################################################
## ############################# # random Forest
#######################################################################################
m67 <- randomForest(Class ~ Time + V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + V10 + V11 + V12 + V13 + 
                       V14 + V15 + V16 + V17 + V18 + V19 + V20 + V21 + V22 + V23 + V24 + V25 + V26 + V27 + 
                       V28 + Amount, data = banco1, ntree = 500)
fited6 <- predict(m6, banco3, type = "response")
rmse1[i,j] <- RMSE(t1,mat_teste[[i]][1])
rae1[i,j] <- rae(t1,mat_teste[[i]][1])
mae1[i,j] <- mae(t1,mat_teste[[i]][1])
rrse1[i,j] <- rrse(t1,mat_teste[[i]][1])
m68 <- randomForest(Class ~ Time + V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + V10 + V11 + V12 + V13 + 
                       V14 + V15 + V16 + V17 + V18 + V19 + V20 + V21 + V22 + V23 + V24 + V25 + V26 + V27 + 
                       V28 + Amount, data = banco2, ntree = 500)
fited6 <- predict(m6, banco3, type = "response")
rmse1[i,j] <- RMSE(t1,mat_teste[[i]][1])
rae1[i,j] <- rae(t1,mat_teste[[i]][1])
mae1[i,j] <- mae(t1,mat_teste[[i]][1])
rrse1[i,j] <- rrse(t1,mat_teste[[i]][1])
m69 <- randomForest(Class ~ Time + V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + V10 + V11 + V12 + V13 + 
                       V14 + V15 + V16 + V17 + V18 + V19 + V20 + V21 + V22 + V23 + V24 + V25 + V26 + V27 + 
                       V28 + Amount, data = banco3, ntree = 500)
fited6 <- predict(m6, banco3, type = "response")
rmse1[i,j] <- RMSE(t1,mat_teste[[i]][1])
rae1[i,j] <- rae(t1,mat_teste[[i]][1])
mae1[i,j] <- mae(t1,mat_teste[[i]][1])
rrse1[i,j] <- rrse(t1,mat_teste[[i]][1])

#######################################
##      VARIAVEIS SIMETRICAS         ##
#######################################
m70 <- randomForest(Class ~ V4 + V9 + V11 + V13 + V15 + V19 + V24, data = banco1, ntree = 500)
fited6 <- predict(m6, banco3, type = "response")
rmse1[i,j] <- RMSE(t1,mat_teste[[i]][1])
rae1[i,j] <- rae(t1,mat_teste[[i]][1])
mae1[i,j] <- mae(t1,mat_teste[[i]][1])
rrse1[i,j] <- rrse(t1,mat_teste[[i]][1])
m71 <- randomForest(Class ~ V4 + V9 + V11 + V13 + V15 + V19 + V24, data = banco2, ntree = 500)
fited6 <- predict(m6, banco3, type = "response")
rmse1[i,j] <- RMSE(t1,mat_teste[[i]][1])
rae1[i,j] <- rae(t1,mat_teste[[i]][1])
mae1[i,j] <- mae(t1,mat_teste[[i]][1])
rrse1[i,j] <- rrse(t1,mat_teste[[i]][1])
m72 <- randomForest(Class ~ V4 + V9 + V11 + V13 + V15 + V19 + V24, data = banco3, ntree = 500)
fited6 <- predict(m6, banco3, type = "response")
rmse1[i,j] <- RMSE(t1,mat_teste[[i]][1])
rae1[i,j] <- rae(t1,mat_teste[[i]][1])
mae1[i,j] <- mae(t1,mat_teste[[i]][1])
rrse1[i,j] <- rrse(t1,mat_teste[[i]][1])

# Para prever todos esses valores é usar o predict.
# fited1 <- predict(m1,banco_teste1, type = "response")