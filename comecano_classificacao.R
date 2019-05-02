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
a <- c(31, 5, 10, 12, 14, 16, 20, 25)
banco4 <- banco[po1,a] 
banco5 <- banco[po2,a] 
banco6 <- banco[po3,a] 

# Separando os 10 folds.
require(caret); 
cv <- 10
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
mat_treino1 <- list(train1, train2, train3, train4, train5, train6, train7, train8, train9, train10)
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
mat_teste1 <- list(teste1, teste2, teste3, teste4, teste5, teste6, teste7, teste8, teste9, teste10)
################################################################
# Lista com os elementos separados para treino. # fold11
################################################################
train41 <- banco4[-flds1[[1]], ]
train42 <- banco4[-flds1[[2]], ]
train43 <- banco4[-flds1[[3]], ]
train44 <- banco4[-flds1[[4]], ]
train45 <- banco4[-flds1[[5]], ]
train46 <- banco4[-flds1[[6]], ]
train47 <- banco4[-flds1[[7]], ]
train48 <- banco4[-flds1[[8]], ]
train49 <- banco4[-flds1[[9]], ]
train410 <- banco4[-flds1[[10]], ]
mat_treino4 <- list(train41, train42, train43, train44, train45, train46, train47, train48, train49, train410)
# Lista com os elementos separados para teste.
teste41 <- banco4[flds1[[1]], ]
teste42 <- banco4[flds1[[2]], ]
teste43 <- banco4[flds1[[3]], ]
teste44 <- banco4[flds1[[4]], ]
teste45 <- banco4[flds1[[5]], ]
teste46 <- banco4[flds1[[6]], ]
teste47 <- banco4[flds1[[7]], ]
teste48 <- banco4[flds1[[8]], ]
teste49 <- banco4[flds1[[9]], ]
teste410 <- banco4[flds1[[10]], ]
mat_teste4 <- list(teste41, teste42, teste43, teste44, teste45, teste46, teste47, teste48, teste49, teste410)


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
# Lista com os elementos separados para treino. # fold22
################################################################
train51 <- banco5[-flds2[[1]], ]
train52 <- banco5[-flds2[[2]], ]
train53 <- banco5[-flds2[[3]], ]
train54 <- banco5[-flds2[[4]], ]
train55 <- banco5[-flds2[[5]], ]
train56 <- banco5[-flds2[[6]], ]
train57 <- banco5[-flds2[[7]], ]
train58 <- banco5[-flds2[[8]], ]
train59 <- banco5[-flds2[[9]], ]
train510 <- banco5[-flds2[[10]], ]
mat_treino5 <- list(train51, train52, train53, train54, train55, train56, train57, train58, train59, train510)
# Lista com os elementos separados para teste.
teste21 <- banco5[flds2[[1]], ]
teste22 <- banco5[flds2[[2]], ]
teste23 <- banco5[flds2[[3]], ]
teste24 <- banco5[flds2[[4]], ]
teste25 <- banco5[flds2[[5]], ]
teste26 <- banco5[flds2[[6]], ]
teste27 <- banco5[flds2[[7]], ]
teste28 <- banco5[flds2[[8]], ]
teste29 <- banco5[flds2[[9]], ]
teste210 <- banco5[flds2[[10]], ]
mat_teste5 <- list(teste51, teste52, teste53, teste54, teste55, teste56, teste57, teste58, teste59, teste510)


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
################################################################
# Lista com os elementos separados para treino. # fold33
################################################################
train61 <- banco6[-flds3[[1]], ]
train62 <- banco6[-flds3[[2]], ]
train63 <- banco6[-flds3[[3]], ]
train64 <- banco6[-flds3[[4]], ]
train65 <- banco6[-flds3[[5]], ]
train66 <- banco6[-flds3[[6]], ]
train67 <- banco6[-flds3[[7]], ]
train68 <- banco6[-flds3[[8]], ]
train69 <- banco6[-flds3[[9]], ]
train610 <- banco6[-flds3[[10]], ]
mat_treino6 <- list(train61, train62, train63, train64, train65, train66, train67, train68, train69, train610)
# Lista com os elementos separados para teste.
teste61 <- banco6[flds3[[1]], ]
teste62 <- banco6[flds3[[2]], ]
teste63 <- banco6[flds3[[3]], ]
teste64 <- banco6[flds3[[4]], ]
teste65 <- banco6[flds3[[5]], ]
teste66 <- banco6[flds3[[6]], ]
teste67 <- banco6[flds3[[7]], ]
teste68 <- banco6[flds3[[8]], ]
teste69 <- banco6[flds3[[9]], ]
teste610 <- banco6[flds3[[10]], ]
mat_teste6 <- list(teste61, teste62, teste63, teste64, teste65, teste66, teste67, teste68, teste69, teste610)


################################################################################################
##                         Fazendo com a distribuição binomial com função de ligação logit    ##
################################################################################################
# Modelo Amostra balanceada, Pouco balanceada e desbalanceada Analise Binomial Logit
m1 <- glm(formula = Class ~ Time + V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + V10 + V11 + V12 + V13 + 
                V14 + V15 + V16 + V17 + V18 + V19 + V20 + V21 + V22 + V23 + V24 + V25 + V26 + V27 + 
                V28 + Amount,family = binomial(link = "logit"), data = mat_treino1[[i]]); # summary(mabbl1)
t1 <- predict(m1, mat_teste1[[i]], type = "response")
rmse1[i,j] <- RMSE(t1,mat_teste1[[i]][31])
rae1[i,j] <- rae(t1,mat_teste1[[i]][31])
mae1[i,j] <- mae(t1,mat_teste1[[i]][31])
rrse1[i,j] <- rrse(t1,mat_teste1[[i]][31])
m2 <- glm(formula = Class ~ Time + V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + V10 + V11 + V12 + V13 + 
                V14 + V15 + V16 + V17 + V18 + V19 + V20 + V21 + V22 + V23 + V24 + V25 + V26 + V27 + 
                V28 + Amount,family = binomial(link = "logit"), data = mat_treino2[[i]]); # summary(mabbl2)
t2 <- predict(m2, mat_teste2[[i]], type = "response")
rmse2[i,j] <- RMSE(t2,mat_teste2[[i]][31])
rae2[i,j] <- rae(t2,mat_teste2[[i]][31])
mae2[i,j] <- mae(t2,mat_teste2[[i]][31])
rrse2[i,j] <- rrse(t2,mat_teste2[[i]][31])
m3 <- glm(formula = Class ~ Time + V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + V10 + V11 + V12 + V13 + 
                V14 + V15 + V16 + V17 + V18 + V19 + V20 + V21 + V22 + V23 + V24 + V25 + V26 + V27 + 
                V28 + Amount,family = binomial(link = "logit"), data = mat_treino3[[i]]); # summary(mabbl3)
t3 <- predict(m3, mat_teste3[[i]], type = "response")
rmse3[i,j] <- RMSE(t3,mat_teste3[[i]][31])
rae3[i,j] <- rae(t3,mat_teste3[[i]][31])
mae3[i,j] <- mae(t3,mat_teste3[[i]][31])
rrse3[i,j] <- rrse(t3,mat_teste3[[i]][31])
##########################
## VARIAVEIS SIMETRICAS ##
##########################
# Modelo Amostra balanceada, Pouco balanceada, desbalanceada Binomial Logit de Variáveis Simetricas.
m4 <- glm(formula =Class ~ V4 + V9 + V11 + V13 + V15 + V19 + V24,family = binomial(link = "logit"), data = mat_treino4[[i]]); # summary(mabbl11)
t4 <- predict(m4, mat_teste4[[i]], type = "response")
rmse4[i,j] <- RMSE(t4,mat_teste4[[i]][1])
rae4[i,j] <- rae(t4,mat_teste4[[i]][1])
mae4[i,j] <- mae(t4,mat_teste4[[i]][1])
rrse4[i,j] <- rrse(t4,mat_teste4[[i]][1])
m5 <- glm(formula = Class ~ V4 + V9 + V11 + V13 + V15 + V19 + V24,family = binomial(link = "logit"), data = mat_treino5[[i]]); # summary(mabbl22)
t5 <- predict(m5, mat_teste5[[i]], type = "response")
rmse5[i,j] <- RMSE(t5,mat_teste5[[i]][1])
rae5[i,j] <- rae(t5,mat_teste5[[i]][1])
mae5[i,j] <- mae(t5,mat_teste5[[i]][1])
rrse5[i,j] <- rrse(t5,mat_teste5[[i]][1])
m6 <- glm(formula = Class ~ V4 + V9 + V11 + V13 + V15 + V19 + V24,family = binomial(link = "logit"), data = mat_treino6[[i]]); # summary(mabbl33)
t6 <- predict(m6, mat_teste6[[i]], type = "response")
rmse6[i,j] <- RMSE(t6,mat_teste6[[i]][1])
rae6[i,j] <- rae(t6,mat_teste6[[i]][1])
mae6[i,j] <- mae(t6,mat_teste6[[i]][1])
rrse6[i,j] <- rrse(t6,mat_teste6[[i]][1])

################################################################################################
##          Fazendo com a distribuição binomial com função de ligação probit               ##
################################################################################################

# Modelo Amostra balanceada, Pouco balanceada e desbalanceadaAnalise Analise de discriminante Quadrático  
m7 <- glm(formula = Class ~ Time + V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + V10 + V11 + V12 + V13 + 
                V14 + V15 + V16 + V17 + V18 + V19 + V20 + V21 + V22 + V23 + V24 + V25 + V26 + V27 + 
                V28 + Amount,family = binomial(link = "probit"), data = mat_treino1[[i]]); # summary(mabbp1)
t7 <- predict(m7, mat_teste1[[i]], type = "response")
rmse7[i,j] <- RMSE(t7,mat_teste1[[i]][31])
rae7[i,j] <- rae(t7,mat_teste1[[i]][31])
mae7[i,j] <- mae(t7,mat_teste1[[i]][31])
rrse7[i,j] <- rrse(t7,mat_teste1[[i]][31])
m8<- glm(formula = Class ~ Time + V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + V10 + V11 + V12 + V13 + 
                V14 + V15 + V16 + V17 + V18 + V19 + V20 + V21 + V22 + V23 + V24 + V25 + V26 + V27 + 
                V28 + Amount,family = binomial(link = "probit"), data = mat_treino2[[i]]); # summary(mabbp2)
t8 <- predict(m8, mat_teste2[[i]], type = "response")
rmse8[i,j] <- RMSE(t8,mat_teste2[[i]][31])
rae8[i,j] <- rae(t8,mat_teste2[[i]][31])
mae8[i,j] <- mae(t8,mat_teste2[[i]][31])
rrse8[i,j] <- rrse(t8,mat_teste2[[i]][31])
m9 <- glm(formula = Class ~ Time + V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + V10 + V11 + V12 + V13 + 
                V14 + V15 + V16 + V17 + V18 + V19 + V20 + V21 + V22 + V23 + V24 + V25 + V26 + V27 + 
                V28 + Amount,family = binomial(link = "probit"), data = mat_treino3[[i]]); # summary(mabbp3)
t9 <- predict(m9, mat_teste3[[i]], type = "response")
rmse9[i,j] <- RMSE(t9,mat_teste3[[i]][31])
rae9[i,j] <- rae(t9,mat_teste3[[i]][31])
mae9[i,j] <- mae(t9,mat_teste3[[i]][31])
rrse9[i,j] <- rrse(t9,mat_teste3[[i]][31])
########################################
##  VARIAVEIS SIMETRICAS             ##
####################################### 
# Modelo Amostra balanceada, Pouco balanceada e desbalanceadaAnalise Simetricas Binomial Probit
m10 <- glm(formula = Class ~ V4 + V9 + V11 + V13 + V15 + V19 + V24,family = binomial(link = "probit"), data = mat_treino4[[i]]); #summary(mabbp11)
t10 <- predict(m10, mat_teste4[[i]], type = "response")
rmse10[i,j] <- RMSE(t10,mat_teste4[[i]][1])
rae10[i,j] <- rae(t10,mat_teste4[[i]][1])
mae10[i,j] <- mae(t10,mat_teste4[[i]][1])
rrse10[i,j] <- rrse(t10,mat_teste4[[i]][1])
m11 <- glm(formula = Class ~ V4 + V9 + V11 + V13 + V15 + V19 + V24,family = binomial(link = "probit"), data = mat_treino5[[i]]); # summary(mabbp22)
t11 <- predict(m11, mat_teste5[[i]], type = "response")
rmse11[i,j] <- RMSE(t11,mat_teste5[[i]][1])
rae11[i,j] <- rae(t11,mat_teste5[[i]][1])
mae11[i,j] <- mae(t11,mat_teste5[[i]][1])
rrse11[i,j] <- rrse(t11,mat_teste5[[i]][1])
m12 <- glm(formula = Class ~ V4 + V9 + V11 + V13 + V15 + V19 + V24,family = binomial(link = "probit"), data = mat_treino6[[i]]); # summary(mabbp33)
t12 <- predict(m12, mat_teste6[[i]], type = "response")
rmse12[i,j] <- RMSE(t12,mat_teste6[[i]][1])
rae12[i,j] <- rae(t12,mat_teste6[[i]][1])
mae12[i,j] <- mae(t12,mat_teste6[[i]][1])
rrse12[i,j] <- rrse(t12,mat_teste6[[i]][1])

################################################################################################
##                         ANALISE LINEAR DE DISCRIMINANTE                                    ##
################################################################################################
# Modelo Amostra balanceada, Pouco balanceada e desbalanceadaAnalise Linear Discriminante.
m13 <- lda(Class ~ Time + V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + V10 + V11 + V12 + V13 + 
                 V14 + V15 + V16 + V17 + V18 + V19 + V20 + V21 + V22 + V23 + V24 + V25 + V26 + V27 + 
                 V28 + Amount, data = mat_treino1[[i]]); # summary(mabald1)
t13 <- predict(m13, mat_teste1[[i]], type = "response")
rmse13[i,j] <- RMSE(t13,mat_teste1[[i]][31])
rae13[i,j] <- rae(t13,mat_teste1[[i]][31])
mae13[i,j] <- mae(t13,mat_teste1[[i]][31])
rrse13[i,j] <- rrse(t13,mat_teste1[[i]][31])
m14 <- lda(formula = Class ~ Time + V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + V10 + V11 + V12 + V13 + 
                 V14 + V15 + V16 + V17 + V18 + V19 + V20 + V21 + V22 + V23 + V24 + V25 + V26 + V27 + 
                 V28 + Amount, data = mat_treino2[[i]]); # summary(mabald2)
t14 <- predict(m14, mat_teste2[[i]], type = "response")
rmse14[i,j] <- RMSE(t14,mat_teste2[[i]][31])
rae14[i,j] <- rae(t14,mat_teste2[[i]][31])
mae14[i,j] <- mae(t14,mat_teste2[[i]][31])
rrse14[i,j] <- rrse(t14,mat_teste2[[i]][31])
m15 <- lda(formula = Class ~ Time + V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + V10 + V11 + V12 + V13 + 
                 V14 + V15 + V16 + V17 + V18 + V19 + V20 + V21 + V22 + V23 + V24 + V25 + V26 + V27 + 
                 V28 + Amount, data = mat_treino3[[i]]); # summary(mabald3)
t15 <- predict(m15, mat_teste3[[i]], type = "response")
rmse15[i,j] <- RMSE(t15,mat_teste3[[i]][31])
rae15[i,j] <- rae(t15,mat_teste3[[i]][31])
mae15[i,j] <- mae(t15,mat_teste3[[i]][31])
rrse15[i,j] <- rrse(t15,mat_teste3[[i]][31])
##############################################
## VARIAVEIS SIMETRICAS                    ##
#############################################
# Modelo Amostra balanceada, Pouco balanceada e desbalanceada Analise Linear Discriminante.
m16 <- lda(formula = Class ~ V4 + V9 + V11 + V13 + V15 + V19 + V24,data = mat_treino4[[i]]); # summary(mabald11)
t16 <- predict(m16, mat_teste4[[i]], type = "response")
rmse16[i,j] <- RMSE(t16,mat_teste4[[i]][1])
rae16[i,j] <- rae(t16,mat_teste4[[i]][1])
mae16[i,j] <- mae(t16,mat_teste4[[i]][1])
rrse16[i,j] <- rrse(t16,mat_teste4[[i]][1])
m17 <- lda(formula = Class ~ V4 + V9 + V11 + V13 + V15 + V19 + V24,data = mat_treino5[[i]]); # summary(mabald22)
t17 <- predict(m17, mat_teste5[[i]], type = "response")
rmse17[i,j] <- RMSE(t17,mat_teste5[[i]][1])
rae17[i,j] <- rae(t17,mat_teste5[[i]][1])
mae17[i,j] <- mae(t17,mat_teste5[[i]][1])
rrse17[i,j] <- rrse(t17,mat_teste5[[i]][1])
m18 <- lda(formula = Class ~ V4 + V9 + V11 + V13 + V15 + V19 + V24,data = mat_treino6[[i]]); # summary(mabald33)
t18 <- predict(m17, mat_teste6[[i]], type = "response")
rmse18[i,j] <- RMSE(t18,mat_teste6[[i]][1])
rae18[i,j] <- rae(t18,mat_teste6[[i]][1])
mae18[i,j] <- mae(t18,mat_teste6[[i]][1])
rrse18[i,j] <- rrse(t18,mat_teste6[[i]][1])

##############################################################################################
##                         ANALISE DE DISCRIMINANTE Quadrático                              ##
##############################################################################################
# Modelo Amostra balanceada, Pouco balanceada e desbalanceada Analise Linear Discriminante.
m19 <- qda(Class ~ Time + V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + V10 + V11 + V12 + V13 + 
                 V14 + V15 + V16 + V17 + V18 + V19 + V20 + V21 + V22 + V23 + V24 + V25 + V26 + V27 + 
                 V28 + Amount, data = mat_treino1[[i]]); #summary(mabaqd1)
t19 <- predict(m19, mat_teste1[[i]], type = "response")
rmse19[i,j] <- RMSE(t19,mat_teste1[[i]][31])
rae19[i,j] <- rae(t19,mat_teste1[[i]][31])
mae19[i,j] <- mae(t19,mat_teste1[[i]][31])
rrse19[i,j] <- rrse(t19,mat_teste1[[i]][31])
m20 <- qda(formula = Class ~ Time + V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + V10 + V11 + V12 + V13 + 
                 V14 + V15 + V16 + V17 + V18 + V19 + V20 + V21 + V22 + V23 + V24 + V25 + V26 + V27 + 
                 V28 + Amount, data = mat_treino2[[i]]); #summary(mabaqd2)
t20 <- predict(m20, mat_teste2[[i]], type = "response")
rmse20[i,j] <- RMSE(t20,mat_teste2[[i]][31])
rae20[i,j] <- rae(t20,mat_teste2[[i]][31])
mae20[i,j] <- mae(t20,mat_teste2[[i]][31])
rrse20[i,j] <- rrse(t20,mat_teste2[[i]][31])
m21 <- qda(formula = Class ~ Time + V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + V10 + V11 + V12 + V13 + 
                 V14 + V15 + V16 + V17 + V18 + V19 + V20 + V21 + V22 + V23 + V24 + V25 + V26 + V27 + 
                 V28 + Amount, data = mat_treino3[[i]]); #summary(mabaqd3)
t21 <- predict(m21, mat_teste3[[i]], type = "response")
rmse21[i,j] <- RMSE(t21,mat_teste3[[i]][31])
rae21[i,j] <- rae(t21,mat_teste3[[i]][31])
mae21[i,j] <- mae(t21,mat_teste3[[i]][31])
rrse21[i,j] <- rrse(t21,mat_teste3[[i]][31])
#######################################
##      VARIAVEIS SIMETRICAS         ##
#######################################
# Modelo Amostra balanceada, Pouco balanceada e desbalanceada Analise Linear Discriminante.
m22 <- qda(formula = Class ~ V4 + V9 + V11 + V13 + V15 + V19 + V24,data = mat_treino4[[i]]);# summary(mabaqd11)
t22 <- predict(m22, mat_teste4[[i]], type = "response")
rmse22[i,j] <- RMSE(t22,mat_teste4[[i]][1])
rae22[i,j] <- rae(t22,mat_teste4[[i]][1])
mae22[i,j] <- mae(t22,mat_teste4[[i]][1])
rrse22[i,j] <- rrse(t22,mat_teste4[[i]][1])
m23 <- qda(formula = Class ~ V4 + V9 + V11 + V13 + V15 + V19 + V24,data = mat_treino5[[i]]);# summary(mabaqd22)
t23 <- predict(m6, mat_teste5[[i]], type = "response")
rmse23[i,j] <- RMSE(t23,mat_teste5[[i]][1])
rae23[i,j] <- rae(t23,mat_teste5[[i]][1])
mae23[i,j] <- mae(t23,mat_teste5[[i]][1])
rrse23[i,j] <- rrse(t23,mat_teste5[[i]][1])
m24 <- qda(formula = Class ~ V4 + V9 + V11 + V13 + V15 + V19 + V24,data = mat_treino6[[i]]);# summary(mabaqd33)
t24 <- predict(m6, mat_teste6[[i]], type = "response")
rmse24[i,j] <- RMSE(t24,mat_teste6[[i]][1])
rae24[i,j] <- rae(t24,mat_teste6[[i]][1])
mae24[i,j] <- mae(t24,mat_teste6[[i]][1])
rrse24[i,j] <- rrse(t24,mat_teste6[[i]][1])

########################################################################################
## ############################## # "5-NN",
######################################################################################
m25 = knnreg(Class ~ Time + V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + V10 + V11 + V12 + V13 + 
               V14 + V15 + V16 + V17 + V18 + V19 + V20 + V21 + V22 + V23 + V24 + V25 + V26 + V27 + 
               V28 + Amount, data = mat_treino1[[i]])
t25 <- predict(m6, mat_teste1[[i]], type = "response")
rmse1[i,j] <- RMSE(t1,mat_teste[[i]][31])
rae1[i,j] <- rae(t1,mat_teste[[i]][31])
mae1[i,j] <- mae(t1,mat_teste[[i]][31])
rrse1[i,j] <- rrse(t1,mat_teste[[i]][31])
m26 = knnreg(Class ~ Time + V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + V10 + V11 + V12 + V13 + 
               V14 + V15 + V16 + V17 + V18 + V19 + V20 + V21 + V22 + V23 + V24 + V25 + V26 + V27 + 
               V28 + Amount, data = mat_treino2[[i]])
fited6 <- predict(m6, mat_teste2[[i]], type = "response")
rmse1[i,j] <- RMSE(t1,mat_tes*te[[i]][31])
rae1[i,j] <- rae(t1,mat_teste[[i]][31])
mae1[i,j] <- mae(t1,mat_teste[[i]][31])
rrse1[i,j] <- rrse(t1,mat_teste[[i]][31])
m27 = knnreg(Class ~ Time + V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + V10 + V11 + V12 + V13 + 
               V14 + V15 + V16 + V17 + V18 + V19 + V20 + V21 + V22 + V23 + V24 + V25 + V26 + V27 + 
               V28 + Amount, data = mat_treino3[[i]])
fited6 <- predict(m6, mat_teste3[[i]], type = "response")
rmse1[i,j] <- RMSE(t1,mat_teste[[i]][31])
rae1[i,j] <- rae(t1,mat_teste[[i]][31])
mae1[i,j] <- mae(t1,mat_teste[[i]][31])
rrse1[i,j] <- rrse(t1,mat_teste[[i]][31])

#######################################
##      VARIAVEIS SIMETRICAS         ##
#######################################
m28 = knnreg(Class ~ V4 + V9 + V11 + V13 + V15 + V19 + V24,data = mat_treino4[[i]])
fited6 <- predict(m6, mat_teste4[[i]], type = "response")
rmse1[i,j] <- RMSE(t1,mat_teste[[i]][1])
rae1[i,j] <- rae(t1,mat_teste[[i]][1])
mae1[i,j] <- mae(t1,mat_teste[[i]][1])
rrse1[i,j] <- rrse(t1,mat_teste[[i]][1])
m29 = knnreg(Class ~ V4 + V9 + V11 + V13 + V15 + V19 + V24,data = mat_treino5[[i]])
fited6 <- predict(m6, mat_teste5[[i]], type = "response")
rmse1[i,j] <- RMSE(t1,mat_teste[[i]][1])
rae1[i,j] <- rae(t1,mat_teste[[i]][1])
mae1[i,j] <- mae(t1,mat_teste[[i]][1])
rrse1[i,j] <- rrse(t1,mat_teste[[i]][1])
m30 = knnreg(Class ~ V4 + V9 + V11 + V13 + V15 + V19 + V24,data = mat_treino6[[i]])
fited6 <- predict(m6, mat_teste6[[i]], type = "response")
rmse1[i,j] <- RMSE(t1,mat_teste[[i]][1])
rae1[i,j] <- rae(t1,mat_teste[[i]][1])
mae1[i,j] <- mae(t1,mat_teste[[i]][1])
rrse1[i,j] <- rrse(t1,mat_teste[[i]][1])

########################################################################################
## ##############################  "3-NN",
######################################################################################
m31 = knnreg(Class ~ Time + V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + V10 + V11 + V12 + V13 + 
               V14 + V15 + V16 + V17 + V18 + V19 + V20 + V21 + V22 + V23 + V24 + V25 + V26 + V27 + 
               V28 + Amount, data = mat_treino1[[i]], k = 3)
fited6 <- predict(m6, mat_teste1[[i]], type = "response")
rmse1[i,j] <- RMSE(t1,mat_teste[[i]][31])
rae1[i,j] <- rae(t1,mat_teste[[i]][31])
mae1[i,j] <- mae(t1,mat_teste[[i]][31])
rrse1[i,j] <- rrse(t1,mat_teste[[i]][31])
m32 = knnreg(Class ~ Time + V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + V10 + V11 + V12 + V13 + 
               V14 + V15 + V16 + V17 + V18 + V19 + V20 + V21 + V22 + V23 + V24 + V25 + V26 + V27 + 
               V28 + Amount, data = mat_treino2[[i]], k = 3)
fited6 <- predict(m6, mat_teste2[[i]], type = "response")
rmse1[i,j] <- RMSE(t1,mat_teste[[i]][31])
rae1[i,j] <- rae(t1,mat_teste[[i]][31])
mae1[i,j] <- mae(t1,mat_teste[[i]][31])
rrse1[i,j] <- rrse(t1,mat_teste[[i]][31])
m33 = knnreg(Class ~ Time + V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + V10 + V11 + V12 + V13 + 
               V14 + V15 + V16 + V17 + V18 + V19 + V20 + V21 + V22 + V23 + V24 + V25 + V26 + V27 + 
               V28 + Amount, data = mat_treino3[[i]], k = 3)
fited6 <- predict(m6, mat_teste3[[i]], type = "response")
rmse1[i,j] <- RMSE(t1,mat_teste[[i]][31])
rae1[i,j] <- rae(t1,mat_teste[[i]][31])
mae1[i,j] <- mae(t1,mat_teste[[i]][31])
rrse1[i,j] <- rrse(t1,mat_teste[[i]][31])

#######################################
##      VARIAVEIS SIMETRICAS         ##
#######################################
m34 = knnreg(Class ~ V4 + V9 + V11 + V13 + V15 + V19 + V24,data = mat_treino4[[i]], k = 3)
fited6 <- predict(m6, mat_teste4[[i]], type = "response")
rmse1[i,j] <- RMSE(t1,mat_teste[[i]][1])
rae1[i,j] <- rae(t1,mat_teste[[i]][1])
mae1[i,j] <- mae(t1,mat_teste[[i]][1])
rrse1[i,j] <- rrse(t1,mat_teste[[i]][1])
m35 = knnreg(Class ~ V4 + V9 + V11 + V13 + V15 + V19 + V24,data = mat_treino5[[i]], k = 3)
fited6 <- predict(m6, mat_teste5[[i]], type = "response")
rmse1[i,j] <- RMSE(t1,mat_teste[[i]][1])
rae1[i,j] <- rae(t1,mat_teste[[i]][1])
mae1[i,j] <- mae(t1,mat_teste[[i]][1])
rrse1[i,j] <- rrse(t1,mat_teste[[i]][1])
m36 = knnreg(Class ~ V4 + V9 + V11 + V13 + V15 + V19 + V24,data = mat_treino6[[i]], k = 3)
fited6 <- predict(m6, mat_teste6[[i]], type = "response")
rmse1[i,j] <- RMSE(t1,mat_teste[[i]][1])
rae1[i,j] <- rae(t1,mat_teste[[i]][1])
mae1[i,j] <- mae(t1,mat_teste[[i]][1])
rrse1[i,j] <- rrse(t1,mat_teste[[i]][1])

########################################################################################
## ############################## "10-NN"
######################################################################################
m37 = knnreg(Class ~ Time + V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + V10 + V11 + V12 + V13 + 
               V14 + V15 + V16 + V17 + V18 + V19 + V20 + V21 + V22 + V23 + V24 + V25 + V26 + V27 + 
               V28 + Amount, data = mat_treino1[[i]], k = 10)
fited6 <- predict(m6, mat_teste1[[i]], type = "response")
rmse1[i,j] <- RMSE(t1,mat_teste[[i]][1])
rae1[i,j] <- rae(t1,mat_teste[[i]][1])
mae1[i,j] <- mae(t1,mat_teste[[i]][1])
rrse1[i,j] <- rrse(t1,mat_teste[[i]][1])
m38 = knnreg(Class ~ Time + V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + V10 + V11 + V12 + V13 + 
               V14 + V15 + V16 + V17 + V18 + V19 + V20 + V21 + V22 + V23 + V24 + V25 + V26 + V27 + 
               V28 + Amount, data = mat_treino2[[i]], k = 10)
fited6 <- predict(m6, mat_teste2[[i]], type = "response")
rmse1[i,j] <- RMSE(t1,mat_teste[[i]][1])
rae1[i,j] <- rae(t1,mat_teste[[i]][1])
mae1[i,j] <- mae(t1,mat_teste[[i]][1])
rrse1[i,j] <- rrse(t1,mat_teste[[i]][1])
m39 = knnreg(Class ~ Time + V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + V10 + V11 + V12 + V13 + 
               V14 + V15 + V16 + V17 + V18 + V19 + V20 + V21 + V22 + V23 + V24 + V25 + V26 + V27 + 
               V28 + Amount, data = mat_treino3[[i]], k = 10)
fited6 <- predict(m6, mat_teste3[[i]], type = "response")
rmse1[i,j] <- RMSE(t1,mat_teste[[i]][1])
rae1[i,j] <- rae(t1,mat_teste[[i]][1])
mae1[i,j] <- mae(t1,mat_teste[[i]][1])
rrse1[i,j] <- rrse(t1,mat_teste[[i]][1])

#######################################
##      VARIAVEIS SIMETRICAS         ##
#######################################
m40 = knnreg(Class ~ V4 + V9 + V11 + V13 + V15 + V19 + V24,data = mat_treino4[[i]], k = 10)
fited6 <- predict(m6, mat_teste4[[i]], type = "response")
rmse1[i,j] <- RMSE(t1,mat_teste[[i]][1])
rae1[i,j] <- rae(t1,mat_teste[[i]][1])
mae1[i,j] <- mae(t1,mat_teste[[i]][1])
rrse1[i,j] <- rrse(t1,mat_teste[[i]][1])
m41 = knnreg(Class ~ V4 + V9 + V11 + V13 + V15 + V19 + V24,data = mat_treino5[[i]], k = 10)
fited6 <- predict(m6, mat_teste5[[i]], type = "response")
rmse1[i,j] <- RMSE(t1,mat_teste[[i]][1])
rae1[i,j] <- rae(t1,mat_teste[[i]][1])
mae1[i,j] <- mae(t1,mat_teste[[i]][1])
rrse1[i,j] <- rrse(t1,mat_teste[[i]][1])
m42 = knnreg(Class ~ V4 + V9 + V11 + V13 + V15 + V19 + V24,data = mat_treino6[[i]], k = 10)
fited6 <- predict(m6, mat_teste6[[i]], type = "response")
rmse1[i,j] <- RMSE(t1,mat_teste[[i]][1])
rae1[i,j] <- rae(t1,mat_teste[[i]][1])
mae1[i,j] <- mae(t1,mat_teste[[i]][1])
rrse1[i,j] <- rrse(t1,mat_teste[[i]][1])

########################################################################################
## ############################## # Regressão Ridge
#######################################################################################
m43 = glmnet(Class ~ Time + V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + V10 + V11 + V12 + V13 + 
               V14 + V15 + V16 + V17 + V18 + V19 + V20 + V21 + V22 + V23 + V24 + V25 + V26 + V27 + 
               V28 + Amount, data = mat_treino1[[i]], alpha = 0) # Regressão Ridge
fited6 <- predict(m6, mat_teste1[[i]], type = "response")
rmse1[i,j] <- RMSE(t1,mat_teste[[i]][1])
rae1[i,j] <- rae(t1,mat_teste[[i]][1])
mae1[i,j] <- mae(t1,mat_teste[[i]][1])
rrse1[i,j] <- rrse(t1,mat_teste[[i]][1])
m44 = glmnet(Class ~ Time + V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + V10 + V11 + V12 + V13 + 
               V14 + V15 + V16 + V17 + V18 + V19 + V20 + V21 + V22 + V23 + V24 + V25 + V26 + V27 + 
               V28 + Amount, data = mat_treino2[[i]], alpha = 0) # Regressão Ridge
fited6 <- predict(m6, mat_teste2[[i]], type = "response")
rmse1[i,j] <- RMSE(t1,mat_teste[[i]][1])
rae1[i,j] <- rae(t1,mat_teste[[i]][1])
mae1[i,j] <- mae(t1,mat_teste[[i]][1])
rrse1[i,j] <- rrse(t1,mat_teste[[i]][1])
m45 = glmnet(Class ~ Time + V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + V10 + V11 + V12 + V13 + 
               V14 + V15 + V16 + V17 + V18 + V19 + V20 + V21 + V22 + V23 + V24 + V25 + V26 + V27 + 
               V28 + Amount, data = mat_treino3[[i]], alpha = 0) # Regressão Ridge
fited6 <- predict(m6, mat_teste3[[i]], type = "response")
rmse1[i,j] <- RMSE(t1,mat_teste[[i]][1])
rae1[i,j] <- rae(t1,mat_teste[[i]][1])
mae1[i,j] <- mae(t1,mat_teste[[i]][1])
rrse1[i,j] <- rrse(t1,mat_teste[[i]][1])

## Melhor lambda a regressão ridge
lambda.ridge1 = cv.glmnet(Class ~ Time + V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + V10 + V11 + V12 + V13 + 
                            V14 + V15 + V16 + V17 + V18 + V19 + V20 + V21 + V22 + V23 + V24 + V25 + V26 + V27 + 
                            V28 + Amount, data = mat_treino1[[i]], alpha = 0) # Melhor lambda para a regressão ridge
lambda.ridge2 = cv.glmnet(Class ~ Time + V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + V10 + V11 + V12 + V13 + 
                            V14 + V15 + V16 + V17 + V18 + V19 + V20 + V21 + V22 + V23 + V24 + V25 + V26 + V27 + 
                            V28 + Amount, data = mat_treino2[[i]], alpha = 0) # Melhor lambda para a regressão ridge
lambda.ridge3 = cv.glmnet(Class ~ Time + V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + V10 + V11 + V12 + V13 + 
                            V14 + V15 + V16 + V17 + V18 + V19 + V20 + V21 + V22 + V23 + V24 + V25 + V26 + V27 + 
                            V28 + Amount, data = mat_treino3[[i]], alpha = 0) # Melhor lambda para a regressão ridge
####################################
##      VARIAVEIS SIMETRICAS      ##
####################################
m46 = glmnet(Class ~ V4 + V9 + V11 + V13 + V15 + V19 + V24,data = mat_treino4[[i]], alpha = 0) # Regressão Ridge
fited6 <- predict(m6, mat_teste4[[i]], type = "response")
rmse1[i,j] <- RMSE(t1,mat_teste[[i]][1])
rae1[i,j] <- rae(t1,mat_teste[[i]][1])
mae1[i,j] <- mae(t1,mat_teste[[i]][1])
rrse1[i,j] <- rrse(t1,mat_teste[[i]][1])
m47 = glmnet(Class ~ V4 + V9 + V11 + V13 + V15 + V19 + V24,data = mat_treino5[[i]], alpha = 0) # Regressão Ridge
fited6 <- predict(m6, mat_teste5[[i]], type = "response")
rmse1[i,j] <- RMSE(t1,mat_teste[[i]][1])
rae1[i,j] <- rae(t1,mat_teste[[i]][1])
mae1[i,j] <- mae(t1,mat_teste[[i]][1])
rrse1[i,j] <- rrse(t1,mat_teste[[i]][1])
m48 = glmnet(Class ~ V4 + V9 + V11 + V13 + V15 + V19 + V24,data = mat_treino6[[i]], alpha = 0) # Regressão Ridge
fited6 <- predict(m6, mat_teste6[[i]], type = "response")
rmse1[i,j] <- RMSE(t1,mat_teste[[i]][1])
rae1[i,j] <- rae(t1,mat_teste[[i]][1])
mae1[i,j] <- mae(t1,mat_teste[[i]][1])
rrse1[i,j] <- rrse(t1,mat_teste[[i]][1])


## Melhor lambda a regressão ridge
lambda.ridge11 = cv.glmnet(Class ~ V4 + V9 + V11 + V13 + V15 + V19 + V24,data = mat_treino4[[i]], alpha = 0) # Melhor lambda para a regressão ridge
lambda.ridge22 = cv.glmnet(Class ~ V4 + V9 + V11 + V13 + V15 + V19 + V24,data = mat_treino5[[i]], alpha = 0) # Melhor lambda para a regressão ridge
lambda.ridge33 = cv.glmnet(Class ~ V4 + V9 + V11 + V13 + V15 + V19 + V24,data = mat_treino6[[i]], alpha = 0) # Melhor lambda para a regressão ridge


########################################################################################
## ############################## # LASSO
#######################################################################################
m49 = glmnet(Class ~ Time + V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + V10 + V11 + V12 + V13 + 
               V14 + V15 + V16 + V17 + V18 + V19 + V20 + V21 + V22 + V23 + V24 + V25 + V26 + V27 + 
               V28 + Amount, data = mat_treino1[[i]], alpha = 1) # LASSO
fited6 <- predict(m6, mat_teste1[[i]], type = "response")
rmse1[i,j] <- RMSE(t1,mat_teste[[i]][1])
rae1[i,j] <- rae(t1,mat_teste[[i]][1])
mae1[i,j] <- mae(t1,mat_teste[[i]][1])
rrse1[i,j] <- rrse(t1,mat_teste[[i]][1])
m50 = glmnet(Class ~ Time + V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + V10 + V11 + V12 + V13 + 
               V14 + V15 + V16 + V17 + V18 + V19 + V20 + V21 + V22 + V23 + V24 + V25 + V26 + V27 + 
               V28 + Amount, data = mat_treino2[[i]], alpha = 1) # LASSO
fited6 <- predict(m6, mat_teste2[[i]], type = "response")
rmse1[i,j] <- RMSE(t1,mat_teste[[i]][1])
rae1[i,j] <- rae(t1,mat_teste[[i]][1])
mae1[i,j] <- mae(t1,mat_teste[[i]][1])
rrse1[i,j] <- rrse(t1,mat_teste[[i]][1])
m51 = glmnet(Class ~ Time + V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + V10 + V11 + V12 + V13 + 
               V14 + V15 + V16 + V17 + V18 + V19 + V20 + V21 + V22 + V23 + V24 + V25 + V26 + V27 + 
               V28 + Amount, data = mat_treino3[[i]], alpha = 1) # LASSO
fited6 <- predict(m6, mat_teste3[[i]], type = "response")
rmse1[i,j] <- RMSE(t1,mat_teste[[i]][1])
rae1[i,j] <- rae(t1,mat_teste[[i]][1])
mae1[i,j] <- mae(t1,mat_teste[[i]][1])
rrse1[i,j] <- rrse(t1,mat_teste[[i]][1])

# Melhor lambda para o LASSO
lambda.lasso1 = cv.glmnet(Class ~ Time + V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + V10 + V11 + V12 + V13 + 
                            V14 + V15 + V16 + V17 + V18 + V19 + V20 + V21 + V22 + V23 + V24 + V25 + V26 + V27 + 
                            V28 + Amount, data = mat_treino1[[i]], alpha = 1) # Melhor lambda para o LASSO
lambda.lasso2 = cv.glmnet(Class ~ Time + V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + V10 + V11 + V12 + V13 + 
                            V14 + V15 + V16 + V17 + V18 + V19 + V20 + V21 + V22 + V23 + V24 + V25 + V26 + V27 + 
                            V28 + Amount, data = mat_treino2[[i]], alpha = 1) # Melhor lambda para o LASSO
lambda.lasso3 = cv.glmnet(Class ~ Time + V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + V10 + V11 + V12 + V13 + 
                            V14 + V15 + V16 + V17 + V18 + V19 + V20 + V21 + V22 + V23 + V24 + V25 + V26 + V27 + 
                            V28 + Amount, data = mat_treino3[[i]], alpha = 1) # Melhor lambda para o LASSO
#######################################
##      VARIAVEIS SIMETRICAS         ##
#######################################
m52 = glmnet(Class ~ V4 + V9 + V11 + V13 + V15 + V19 + V24,data = mat_treino4[[i]], alpha = 1) # LASSO
fited6 <- predict(m6, mat_teste4[[i]], type = "response")
rmse1[i,j] <- RMSE(t1,mat_teste[[i]][1])
rae1[i,j] <- rae(t1,mat_teste[[i]][1])
mae1[i,j] <- mae(t1,mat_teste[[i]][1])
rrse1[i,j] <- rrse(t1,mat_teste[[i]][1])
m53 = glmnet(Class ~ V4 + V9 + V11 + V13 + V15 + V19 + V24,data = mat_treino5[[i]], alpha = 1) # LASSO
fited6 <- predict(m6, mat_teste5[[i]], type = "response")
rmse1[i,j] <- RMSE(t1,mat_teste[[i]][1])
rae1[i,j] <- rae(t1,mat_teste[[i]][1])
mae1[i,j] <- mae(t1,mat_teste[[i]][1])
rrse1[i,j] <- rrse(t1,mat_teste[[i]][1])
m54 = glmnet(Class ~ V4 + V9 + V11 + V13 + V15 + V19 + V24,data = mat_treino6[[i]], alpha = 1) # LASSO
fited6 <- predict(m6, mat_teste6[[i]], type = "response")
rmse1[i,j] <- RMSE(t1,mat_teste[[i]][1])
rae1[i,j] <- rae(t1,mat_teste[[i]][1])
mae1[i,j] <- mae(t1,mat_teste[[i]][1])
rrse1[i,j] <- rrse(t1,mat_teste[[i]][1])

# Melhor lambda para o LASSO
lambda.lasso11 = cv.glmnet(Class ~ V4 + V9 + V11 + V13 + V15 + V19 + V24,data = mat_treino4[[i]], alpha = 1) # Melhor lambda para o LASSO
lambda.lasso22 = cv.glmnet(Class ~ V4 + V9 + V11 + V13 + V15 + V19 + V24,data = mat_treino5[[i]], alpha = 1) # Melhor lambda para o LASSO
lambda.lasso33 = cv.glmnet(Class ~ V4 + V9 + V11 + V13 + V15 + V19 + V24,data = mat_treino6[[i]], alpha = 1) # Melhor lambda para o LASSO


########################################################################################
## ############################## # ElasticNet
#######################################################################################
m55 = glmnet(Class ~ Time + V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + V10 + V11 + V12 + V13 + 
               V14 + V15 + V16 + V17 + V18 + V19 + V20 + V21 + V22 + V23 + V24 + V25 + V26 + V27 + 
               V28 + Amount, data = mat_treino1[[i]], alpha = 0.5) # ElasticNet
t55 <- predict(m6, mat_teste1[[i]], type = "response")
rmse1[i,j] <- RMSE(t1,mat_teste[[i]][1])
rae1[i,j] <- rae(t1,mat_teste[[i]][1])
mae1[i,j] <- mae(t1,mat_teste[[i]][1])
rrse1[i,j] <- rrse(t1,mat_teste[[i]][1])
m56 = glmnet(Class ~ Time + V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + V10 + V11 + V12 + V13 + 
               V14 + V15 + V16 + V17 + V18 + V19 + V20 + V21 + V22 + V23 + V24 + V25 + V26 + V27 + 
               V28 + Amount, data = mat_treino2[[i]], alpha = 0.5) # ElasticNet
t56 <- predict(m6, mat_teste2[[i]], type = "response")
rmse1[i,j] <- RMSE(t1,mat_teste[[i]][1])
rae1[i,j] <- rae(t1,mat_teste[[i]][1])
mae1[i,j] <- mae(t1,mat_teste[[i]][1])
rrse1[i,j] <- rrse(t1,mat_teste[[i]][1])
m57 = glmnet(Class ~ Time + V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + V10 + V11 + V12 + V13 + 
               V14 + V15 + V16 + V17 + V18 + V19 + V20 + V21 + V22 + V23 + V24 + V25 + V26 + V27 + 
               V28 + Amount, data = mat_treino3[[i]], alpha = 0.5) # ElasticNet
t57 <- predict(m6, mat_teste3[[i]], type = "response")
rmse1[i,j] <- RMSE(t1,mat_teste[[i]][1])
rae1[i,j] <- rae(t1,mat_teste[[i]][1])
mae1[i,j] <- mae(t1,mat_teste[[i]][1])
rrse1[i,j] <- rrse(t1,mat_teste[[i]][1])

## ###### Melhor lambda para a ElasticNet
lambda.elasticnet1 = cv.glmnet(Class ~ Time + V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + V10 + V11 + V12 + V13 + 
                                 V14 + V15 + V16 + V17 + V18 + V19 + V20 + V21 + V22 + V23 + V24 + V25 + V26 + V27 + 
                                 V28 + Amount, data = mat_treino1[[i]], alpha = 0.5) # Melhor lambda para a ElasticNet
lambda.elasticnet2 = cv.glmnet(Class ~ Time + V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + V10 + V11 + V12 + V13 + 
                                 V14 + V15 + V16 + V17 + V18 + V19 + V20 + V21 + V22 + V23 + V24 + V25 + V26 + V27 + 
                                 V28 + Amount, data = mat_treino2[[i]], alpha = 0.5) # Melhor lambda para a ElasticNet
lambda.elasticnet3 = cv.glmnet(Class ~ Time + V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + V10 + V11 + V12 + V13 + 
                                 V14 + V15 + V16 + V17 + V18 + V19 + V20 + V21 + V22 + V23 + V24 + V25 + V26 + V27 + 
                                 V28 + Amount, data = mat_treino3[[i]], alpha = 0.5) # Melhor lambda para a ElasticNet
#######################################
##      VARIAVEIS SIMETRICAS         ##
#######################################
m58 = glmnet(bClass ~ V4 + V9 + V11 + V13 + V15 + V19 + V24,data = mat_treino4[[i]], alpha = 0.5) # ElasticNet
fited6 <- predict(m6, mat_teste4[[i]], type = "response")
rmse1[i,j] <- RMSE(t1,mat_teste[[i]][1])
rae1[i,j] <- rae(t1,mat_teste[[i]][1])
mae1[i,j] <- mae(t1,mat_teste[[i]][1])
rrse1[i,j] <- rrse(t1,mat_teste[[i]][1])
m59 = glmnet(Class ~ V4 + V9 + V11 + V13 + V15 + V19 + V24,data = mat_treino5[[i]], alpha = 0.5) # ElasticNet
fited6 <- predict(m6, mat_teste5[[i]], type = "response")
rmse1[i,j] <- RMSE(t1,mat_teste[[i]][1])
rae1[i,j] <- rae(t1,mat_teste[[i]][1])
mae1[i,j] <- mae(t1,mat_teste[[i]][1])
rrse1[i,j] <- rrse(t1,mat_teste[[i]][1])
m60 = glmnet(Class ~ V4 + V9 + V11 + V13 + V15 + V19 + V24,data = mat_treino6[[i]], alpha = 0.5) # ElasticNet
fited6 <- predict(m6, mat_teste6[[i]], type = "response")
rmse1[i,j] <- RMSE(t1,mat_teste[[i]][1])
rae1[i,j] <- rae(t1,mat_teste[[i]][1])
mae1[i,j] <- mae(t1,mat_teste[[i]][1])
rrse1[i,j] <- rrse(t1,mat_teste[[i]][1])

##Melhor lambda para a ElasticNet
lambda.elasticnet11 = cv.glmnet(Class ~ V4 + V9 + V11 + V13 + V15 + V19 + V24,data = mat_treino4[[i]], alpha = 0.5) # Melhor lambda para a ElasticNet
lambda.elasticnet22 = cv.glmnet(Class ~ V4 + V9 + V11 + V13 + V15 + V19 + V24,data = mat_treino5[[i]], alpha = 0.5) # Melhor lambda para a ElasticNet
lambda.elasticnet33 = cv.glmnet(Class ~ V4 + V9 + V11 + V13 + V15 + V19 + V24,data = mat_treino6[[i]], alpha = 0.5) # Melhor lambda para a ElasticNet


########################################################################################
## #############################Cresce a árvore 
#######################################################################################
m61 <- rpart( Class ~ Time + V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + V10 + V11 + V12 + V13 + 
                V14 + V15 + V16 + V17 + V18 + V19 + V20 + V21 + V22 + V23 + V24 + V25 + V26 + V27 + 
                V28 + Amount, data = mat_treino1[[i]],method="class")
fited6 <- predict(m6, mat_teste1[[i]], type = "response")
rmse1[i,j] <- RMSE(t1,mat_teste[[i]][1])
rae1[i,j] <- rae(t1,mat_teste[[i]][1])
mae1[i,j] <- mae(t1,mat_teste[[i]][1])
rrse1[i,j] <- rrse(t1,mat_teste[[i]][1])
m62 <- rpart( Class ~ Time + V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + V10 + V11 + V12 + V13 + 
                V14 + V15 + V16 + V17 + V18 + V19 + V20 + V21 + V22 + V23 + V24 + V25 + V26 + V27 + 
                V28 + Amount, data = mat_treino2[[i]],method="class")
fited6 <- predict(m6, mat_teste2[[i]], type = "response")
rmse1[i,j] <- RMSE(t1,mat_teste[[i]][1])
rae1[i,j] <- rae(t1,mat_teste[[i]][1])
mae1[i,j] <- mae(t1,mat_teste[[i]][1])
rrse1[i,j] <- rrse(t1,mat_teste[[i]][1])
m63 <- rpart( Class ~ Time + V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + V10 + V11 + V12 + V13 + 
                V14 + V15 + V16 + V17 + V18 + V19 + V20 + V21 + V22 + V23 + V24 + V25 + V26 + V27 + 
                V28 + Amount, data = mat_treino3[[i]],method="class")
fited6 <- predict(m6, mat_teste3[[i]], type = "response")
rmse1[i,j] <- RMSE(t1,mat_teste[[i]][1])
rae1[i,j] <- rae(t1,mat_teste[[i]][1])
mae1[i,j] <- mae(t1,mat_teste[[i]][1])
rrse1[i,j] <- rrse(t1,mat_teste[[i]][1])

#######################################
##      VARIAVEIS SIMETRICAS         ##
#######################################
m64 <- rpart(Class ~ V4 + V9 + V11 + V13 + V15 + V19 + V24, data = mat_treino4[[i]],method="class")
fited6 <- predict(m6, mat_teste4[[i]], type = "response")
rmse1[i,j] <- RMSE(t1,mat_teste[[i]][1])
rae1[i,j] <- rae(t1,mat_teste[[i]][1])
mae1[i,j] <- mae(t1,mat_teste[[i]][1])
rrse1[i,j] <- rrse(t1,mat_teste[[i]][1])
m65 <- rpart(Class ~ V4 + V9 + V11 + V13 + V15 + V19 + V24, data = mat_treino5[[i]],method="class")
fited6 <- predict(m6, mat_teste5[[i]], type = "response")
rmse1[i,j] <- RMSE(t1,mat_teste[[i]][1])
rae1[i,j] <- rae(t1,mat_teste[[i]][1])
mae1[i,j] <- mae(t1,mat_teste[[i]][1])
rrse1[i,j] <- rrse(t1,mat_teste[[i]][1])
m66 <- rpart(Class ~ V4 + V9 + V11 + V13 + V15 + V19 + V24, data = mat_treino6[[i]],method="class")
fited6 <- predict(m6, mat_teste6[[i]], type = "response")
rmse1[i,j] <- RMSE(t1,mat_teste[[i]][1])
rae1[i,j] <- rae(t1,mat_teste[[i]][1])
mae1[i,j] <- mae(t1,mat_teste[[i]][1])
rrse1[i,j] <- rrse(t1,mat_teste[[i]][1])

########################################################################################
## ############################# # random Forest
#######################################################################################
m67 <- randomForest(Class ~ Time + V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + V10 + V11 + V12 + V13 + 
                       V14 + V15 + V16 + V17 + V18 + V19 + V20 + V21 + V22 + V23 + V24 + V25 + V26 + V27 + 
                       V28 + Amount, data = mat_treino1[[i]], ntree = 500)
fited6 <- predict(m6, mat_teste1[[i]], type = "response")
rmse1[i,j] <- RMSE(t1,mat_teste[[i]][1])
rae1[i,j] <- rae(t1,mat_teste[[i]][1])
mae1[i,j] <- mae(t1,mat_teste[[i]][1])
rrse1[i,j] <- rrse(t1,mat_teste[[i]][1])
m68 <- randomForest(Class ~ Time + V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + V10 + V11 + V12 + V13 + 
                       V14 + V15 + V16 + V17 + V18 + V19 + V20 + V21 + V22 + V23 + V24 + V25 + V26 + V27 + 
                       V28 + Amount, data = mat_treino2[[i]], ntree = 500)
fited6 <- predict(m6, mat_teste2[[i]], type = "response")
rmse1[i,j] <- RMSE(t1,mat_teste[[i]][1])
rae1[i,j] <- rae(t1,mat_teste[[i]][1])
mae1[i,j] <- mae(t1,mat_teste[[i]][1])
rrse1[i,j] <- rrse(t1,mat_teste[[i]][1])
m69 <- randomForest(Class ~ Time + V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + V10 + V11 + V12 + V13 + 
                       V14 + V15 + V16 + V17 + V18 + V19 + V20 + V21 + V22 + V23 + V24 + V25 + V26 + V27 + 
                       V28 + Amount, data = mat_treino3[[i]], ntree = 500)
fited6 <- predict(m6, mat_teste3[[i]], type = "response")
rmse1[i,j] <- RMSE(t1,mat_teste[[i]][1])
rae1[i,j] <- rae(t1,mat_teste[[i]][1])
mae1[i,j] <- mae(t1,mat_teste[[i]][1])
rrse1[i,j] <- rrse(t1,mat_teste[[i]][1])

#######################################
##      VARIAVEIS SIMETRICAS         ##
#######################################
m70 <- randomForest(Class ~ V4 + V9 + V11 + V13 + V15 + V19 + V24, data = mat_treino4[[i]], ntree = 500)
fited6 <- predict(m6, mat_teste4[[i]], type = "response")
rmse1[i,j] <- RMSE(t1,mat_teste[[i]][1])
rae1[i,j] <- rae(t1,mat_teste[[i]][1])
mae1[i,j] <- mae(t1,mat_teste[[i]][1])
rrse1[i,j] <- rrse(t1,mat_teste[[i]][1])
m71 <- randomForest(Class ~ V4 + V9 + V11 + V13 + V15 + V19 + V24, data = mat_treino5[[i]], ntree = 500)
fited6 <- predict(m6, mat_teste5[[i]], type = "response")
rmse1[i,j] <- RMSE(t1,mat_teste[[i]][1])
rae1[i,j] <- rae(t1,mat_teste[[i]][1])
mae1[i,j] <- mae(t1,mat_teste[[i]][1])
rrse1[i,j] <- rrse(t1,mat_teste[[i]][1])
m72 <- randomForest(Class ~ V4 + V9 + V11 + V13 + V15 + V19 + V24, data = mat_treino6[[i]], ntree = 500)
fited6 <- predict(m6, mat_teste6[[i]], type = "response")
rmse1[i,j] <- RMSE(t1,mat_teste[[i]][1])
rae1[i,j] <- rae(t1,mat_teste[[i]][1])
mae1[i,j] <- mae(t1,mat_teste[[i]][1])
rrse1[i,j] <- rrse(t1,mat_teste[[i]][1])

# Para prever todos esses valores é usar o predict.
# fited1 <- predict(m1,banco_teste1, type = "response")