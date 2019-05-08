#################################################################################
# Começando com a questão de classificação
install.packages("fBasics")
install.packages("MASS")
install.packages("rpart")
install.packages("randomForest")
install.packages("glmnet")
install.packages("class")

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
library(class)

correct_prediction <- function(c){ (c[1]+c[4])/sum(c) }
false_positivo <- function(c){ c[2]/sum(c) }
false_negativo <- function(c){ c[3]/sum(c) }
true_positivo <- function(c){ c[4]/sum(c) }
true_negativo <- function(c){ c[1]/sum(c) }

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

cv <- 10
k <- 10
# Matrizes que vao receber as méidas das estimativas de rood mean square error
# a quantidade dessas matrizes 
##############################################################################
cp1 <- matrix(0,cv,k)
cp2 <- matrix(0,cv,k)
cp3 <- matrix(0,cv,k)
cp4 <- matrix(0,cv,k)
cp5 <- matrix(0,cv,k)
cp6 <- matrix(0,cv,k)
cp7 <- matrix(0,cv,k)
cp8 <- matrix(0,cv,k)
cp9 <- matrix(0,cv,k)
cp10 <- matrix(0,cv,k)
cp11 <- matrix(0,cv,k)
cp12 <- matrix(0,cv,k)
cp13 <- matrix(0,cv,k)
cp14 <- matrix(0,cv,k)
cp15 <- matrix(0,cv,k)
cp16 <- matrix(0,cv,k)
cp17 <- matrix(0,cv,k)
cp18 <- matrix(0,cv,k)
cp19 <- matrix(0,cv,k)
cp20 <- matrix(0,cv,k)
cp21 <- matrix(0,cv,k)
cp22 <- matrix(0,cv,k)
cp23 <- matrix(0,cv,k)
cp24 <- matrix(0,cv,k)
cp25 <- matrix(0,cv,k)
cp26 <- matrix(0,cv,k)
cp27 <- matrix(0,cv,k)
cp28 <- matrix(0,cv,k)
cp29 <- matrix(0,cv,k)
cp30 <- matrix(0,cv,k)
cp31 <- matrix(0,cv,k)
cp32 <- matrix(0,cv,k)
cp33 <- matrix(0,cv,k)
cp34 <- matrix(0,cv,k)
cp35 <- matrix(0,cv,k)
cp36 <- matrix(0,cv,k)
cp37 <- matrix(0,cv,k)
cp38 <- matrix(0,cv,k)
cp39 <- matrix(0,cv,k)
cp40 <- matrix(0,cv,k)
cp41 <- matrix(0,cv,k)
cp42 <- matrix(0,cv,k)
cp43 <- matrix(0,cv,k)
cp44 <- matrix(0,cv,k)
cp45 <- matrix(0,cv,k)
cp46 <- matrix(0,cv,k)
cp47 <- matrix(0,cv,k)
cp48 <- matrix(0,cv,k)
cp49 <- matrix(0,cv,k)
cp50 <- matrix(0,cv,k)
cp51 <- matrix(0,cv,k)
cp52 <- matrix(0,cv,k)
cp53 <- matrix(0,cv,k)
cp54 <- matrix(0,cv,k)
cp55 <- matrix(0,cv,k)
cp56 <- matrix(0,cv,k)
cp57 <- matrix(0,cv,k)
cp58 <- matrix(0,cv,k)
cp59 <- matrix(0,cv,k)
cp60 <- matrix(0,cv,k)
cp61 <- matrix(0,cv,k)
cp62 <- matrix(0,cv,k)
cp63 <- matrix(0,cv,k)
cp64 <- matrix(0,cv,k)
cp65 <- matrix(0,cv,k)
cp66 <- matrix(0,cv,k)
cp67 <- matrix(0,cv,k)
cp68 <- matrix(0,cv,k)
cp69 <- matrix(0,cv,k)
cp70 <- matrix(0,cv,k)
cp71 <- matrix(0,cv,k)
cp72 <- matrix(0,cv,k)


###############################################################################################
for(i in 1:k){
  for(j in 1:cv){  

###############################################################################################
## Selecionando o banco balanceado, pouco balanceado e muito desbalanceado.                 ###
###############################################################################################
## São 387 que em Class assume 1.   # São 227459 que em Class assume 0.
vec <- which(Class == 1);          posicoes <- which(Class == 0)
# Pegando uma quantidade de 1's porque o banco é muito desbalanceado, mas estamos balanceando 
# length(posicoes)/length(vec) = 587.7494. tem 587.7494 cerca de 99,8% de 0's
# em Class.
# Balanceado.
vec1 <- sample(x = posicoes, size = length(vec))
# Pouco  Balanceado.
vec2 <- sample(x = posicoes, size = 2 * length(vec) + 1)
# Muito  Desalanceado.
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
flds1 <- createDataPartition(banco1$Class, times = cv, p = 0.5, list = TRUE)
flds2 <- createDataPartition(banco2$Class, times = cv, p = 0.5, list = TRUE)
flds3 <- createDataPartition(banco3$Class, times = cv, p = 0.5, list = TRUE)
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
teste51 <- banco5[flds2[[1]], ]
teste52 <- banco5[flds2[[2]], ]
teste53 <- banco5[flds2[[3]], ]
teste54 <- banco5[flds2[[4]], ]
teste55 <- banco5[flds2[[5]], ]
teste56 <- banco5[flds2[[6]], ]
teste57 <- banco5[flds2[[7]], ]
teste58 <- banco5[flds2[[8]], ]
teste59 <- banco5[flds2[[9]], ]
teste510 <- banco5[flds2[[10]], ]
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
t1 <- ifelse(t1 > 0.5, 1, 0)
cp1[i,j] <- correct_prediction(table(t1,mat_teste1[[i]]$Class))

m2 <- glm(formula = Class ~ Time + V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + V10 + V11 + V12 + V13 + 
                V14 + V15 + V16 + V17 + V18 + V19 + V20 + V21 + V22 + V23 + V24 + V25 + V26 + V27 + 
                V28 + Amount,family = binomial(link = "logit"), data = mat_treino2[[i]]); # summary(mabbl2)
t2 <- predict(m2, mat_teste2[[i]], type = "response")
t2 <- ifelse(t2 > 0.5, 1, 0)
cp2[i,j] <- correct_prediction(table(t2,mat_teste2[[i]]$Class))

m3 <- glm(formula = Class ~ Time + V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + V10 + V11 + V12 + V13 + 
                V14 + V15 + V16 + V17 + V18 + V19 + V20 + V21 + V22 + V23 + V24 + V25 + V26 + V27 + 
                V28 + Amount,family = binomial(link = "logit"), data = mat_treino3[[i]]); # summary(mabbl3)
t3 <- predict(m3, mat_teste3[[i]], type = "response")
t3 <- ifelse(t3 > 0.5, 1, 0)
cp3[i,j] <- correct_prediction(table(t3,mat_teste3[[i]]$Class))

##########################
## VARIAVEIS SIMETRICAS ##
##########################
# Modelo Amostra balanceada, Pouco balanceada, desbalanceada Binomial Logit de Variáveis Simetricas.
m4 <- glm(formula =Class ~ V4 + V9 + V11 + V13 + V15 + V19 + V24,family = binomial(link = "logit"), data = mat_treino4[[i]]); # summary(mabbl11)
t4 <- predict(m4, mat_teste4[[i]], type = "response")
t4 <- ifelse(t4 > 0.5, 1, 0)
cp4[i,j] <- correct_prediction(table(t4,mat_teste4[[i]]$Class))

m5 <- glm(formula = Class ~ V4 + V9 + V11 + V13 + V15 + V19 + V24,family = binomial(link = "logit"), data = mat_treino5[[i]]); # summary(mabbl22)
t5 <- predict(m5, mat_teste5[[i]], type = "response")
t5 <- ifelse(t5 > 0.5, 1, 0)
cp5[i,j] <- correct_prediction(table(t5,mat_teste5[[i]]$Class))

m6 <- glm(formula = Class ~ V4 + V9 + V11 + V13 + V15 + V19 + V24,family = binomial(link = "logit"), data = mat_treino6[[i]]); # summary(mabbl33)
t6 <- predict(m6, mat_teste6[[i]], type = "response")
t6 <- ifelse(t6 > 0.5, 1, 0)
cp6[i,j] <- correct_prediction(table(t6,mat_teste6[[i]]$Class))

################################################################################################
##          Fazendo com a distribuição binomial com função de ligação probit               ##
################################################################################################

# Modelo Amostra balanceada, Pouco balanceada e desbalanceadaAnalise Analise de discriminante Quadrático  
m7 <- glm(formula = Class ~ Time + V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + V10 + V11 + V12 + V13 + 
                V14 + V15 + V16 + V17 + V18 + V19 + V20 + V21 + V22 + V23 + V24 + V25 + V26 + V27 + 
                V28 + Amount,family = binomial(link = "probit"), data = mat_treino1[[i]]); # summary(mabbp1)
t7 <- predict(m7, mat_teste1[[i]], type = "response")
t7 <- ifelse(t7 > 0.5, 1, 0)
cp7[i,j] <- correct_prediction(table(t7,mat_teste1[[i]]$Class))

m8<- glm(formula = Class ~ Time + V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + V10 + V11 + V12 + V13 + 
                V14 + V15 + V16 + V17 + V18 + V19 + V20 + V21 + V22 + V23 + V24 + V25 + V26 + V27 + 
                V28 + Amount,family = binomial(link = "probit"), data = mat_treino2[[i]]); # summary(mabbp2)
t8 <- predict(m8, mat_teste2[[i]], type = "response")
t8 <- ifelse(t8 > 0.5, 1, 0)
cp8[i,j] <- correct_prediction(table(t8,mat_teste2[[i]]$Class))

m9 <- glm(formula = Class ~ Time + V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + V10 + V11 + V12 + V13 + 
                V14 + V15 + V16 + V17 + V18 + V19 + V20 + V21 + V22 + V23 + V24 + V25 + V26 + V27 + 
                V28 + Amount,family = binomial(link = "probit"), data = mat_treino3[[i]]); # summary(mabbp3)
t9 <- predict(m9, mat_teste3[[i]], type = "response")
t9 <- ifelse(t9 > 0.5, 1, 0)
cp9[i,j] <- correct_prediction(table(t9,mat_teste3[[i]]$Class))


########################################
##  VARIAVEIS SIMETRICAS             ##
####################################### 
# Modelo Amostra balanceada, Pouco balanceada e desbalanceadaAnalise Simetricas Binomial Probit
m10 <- glm(formula = Class ~ V4 + V9 + V11 + V13 + V15 + V19 + V24,family = binomial(link = "probit"), data = mat_treino4[[i]]); #summary(mabbp11)
t10 <- predict(m10, mat_teste4[[i]], type = "response")
t10 <- ifelse(t10 > 0.5, 1, 0)
cp10[i,j] <- correct_prediction(table(t10,mat_teste4[[i]]$Class))


m11 <- glm(formula = Class ~ V4 + V9 + V11 + V13 + V15 + V19 + V24,family = binomial(link = "probit"), data = mat_treino5[[i]]); # summary(mabbp22)
t11 <- predict(m11, mat_teste5[[i]], type = "response")
t11 <- ifelse(t11 > 0.5, 1, 0)
cp11[i,j] <- correct_prediction(table(t11,mat_teste5[[i]]$Class))


m12 <- glm(formula = Class ~ V4 + V9 + V11 + V13 + V15 + V19 + V24,family = binomial(link = "probit"), data = mat_treino6[[i]]); # summary(mabbp33)
t12 <- predict(m12, mat_teste6[[i]], type = "response")
t12 <- ifelse(t12 > 0.5, 1, 0)
cp12[i,j] <- correct_prediction(table(t12,mat_teste6[[i]]$Class))


################################################################################################
##                         ANALISE LINEAR DE DISCRIMINANTE                                    ##
################################################################################################
# Modelo Amostra balanceada, Pouco balanceada e desbalanceadaAnalise Linear Discriminante.
m13 <- lda(Class ~ Time + V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + V10 + V11 + V12 + V13 + 
                 V14 + V15 + V16 + V17 + V18 + V19 + V20 + V21 + V22 + V23 + V24 + V25 + V26 + V27 + 
                 V28 + Amount, data = mat_treino1[[i]]); # summary(mabald1)
t13 <- predict(m13, mat_teste1[[i]], type = "response")
t13 <- ifelse(t13$class == 1, 1, 0)
cp13[i,j] <- correct_prediction(table(t13,mat_teste1[[i]]$Class))

m14 <- lda(formula = Class ~ Time + V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + V10 + V11 + V12 + V13 + 
                 V14 + V15 + V16 + V17 + V18 + V19 + V20 + V21 + V22 + V23 + V24 + V25 + V26 + V27 + 
                 V28 + Amount, data = mat_treino2[[i]]); # summary(mabald2)
t14 <- predict(m14, mat_teste2[[i]], type = "response")
t14 <- ifelse(t14$class == 1, 1, 0)
cp14[i,j] <- correct_prediction(table(t14,mat_teste2[[i]]$Class))

m15 <- lda(formula = Class ~ Time + V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + V10 + V11 + V12 + V13 + 
                 V14 + V15 + V16 + V17 + V18 + V19 + V20 + V21 + V22 + V23 + V24 + V25 + V26 + V27 + 
                 V28 + Amount, data = mat_treino3[[i]]); # summary(mabald3)
t15 <- predict(m15, mat_teste3[[i]], type = "response")
t15 <- ifelse(t15$class == 1, 1, 0)
cp15[i,j] <- correct_prediction(table(t15,mat_teste3[[i]]$Class))

##############################################
## VARIAVEIS SIMETRICAS                    ##
#############################################
# Modelo Amostra balanceada, Pouco balanceada e desbalanceada Analise Linear Discriminante.
m16 <- lda(formula = Class ~ V4 + V9 + V11 + V13 + V15 + V19 + V24,data = mat_treino4[[i]]); # summary(mabald11)
t16 <- predict(m16, mat_teste4[[i]], type = "response")
t16 <- ifelse(t16$class == 1, 1, 0)
cp16[i,j] <- correct_prediction(table(t16,mat_teste4[[i]]$Class))

m17 <- lda(formula = Class ~ V4 + V9 + V11 + V13 + V15 + V19 + V24,data = mat_treino5[[i]]); # summary(mabald22)
t17 <- predict(m17, mat_teste5[[i]], type = "response")
t17 <- ifelse(t17$class == 1, 1, 0)
cp17[i,j] <- correct_prediction(table(t17,mat_teste5[[i]]$Class))

m18 <- lda(formula = Class ~ V4 + V9 + V11 + V13 + V15 + V19 + V24,data = mat_treino6[[i]]); # summary(mabald33)
t18 <- predict(m18, mat_teste6[[i]], type = "response")
t18 <- ifelse(t18$class == 1, 1, 0)
cp18[i,j] <- correct_prediction(table(t18,mat_teste6[[i]]$Class))

##############################################################################################
##                         ANALISE DE DISCRIMINANTE Quadrático                              ##
##############################################################################################
# Modelo Amostra balanceada, Pouco balanceada e desbalanceada Analise Linear Discriminante.
m19 <- qda(Class ~ Time + V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + V10 + V11 + V12 + V13 + 
                 V14 + V15 + V16 + V17 + V18 + V19 + V20 + V21 + V22 + V23 + V24 + V25 + V26 + V27 + 
                 V28 + Amount, data = mat_treino1[[i]]); #summary(mabaqd1)
t19 <- predict(m19, mat_teste1[[i]], type = "response")
t19 <- ifelse(t19$class == 1, 1, 0)
cp19[i,j] <- correct_prediction(table(t19,mat_teste1[[i]]$Class))

m20 <- qda(formula = Class ~ Time + V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + V10 + V11 + V12 + V13 + 
                 V14 + V15 + V16 + V17 + V18 + V19 + V20 + V21 + V22 + V23 + V24 + V25 + V26 + V27 + 
                 V28 + Amount, data = mat_treino2[[i]]); #summary(mabaqd2)
t20 <- predict(m20, mat_teste2[[i]], type = "response")
t20 <- ifelse(t20$class == 1, 1, 0)
cp20[i,j] <- correct_prediction(table(t20,mat_teste2[[i]]$Class))

m21 <- qda(formula = Class ~ Time + V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + V10 + V11 + V12 + V13 + 
                 V14 + V15 + V16 + V17 + V18 + V19 + V20 + V21 + V22 + V23 + V24 + V25 + V26 + V27 + 
                 V28 + Amount, data = mat_treino3[[i]]); #summary(mabaqd3)
t21 <- predict(m21, mat_teste3[[i]], type = "response")
t21 <- ifelse(t21$class == 1, 1, 0)
cp21[i,j] <- correct_prediction(table(t21,mat_teste3[[i]]$Class))

#######################################
##      VARIAVEIS SIMETRICAS         ##
#######################################
# Modelo Amostra balanceada, Pouco balanceada e desbalanceada Analise Linear Discriminante.
m22 <- qda(formula = Class ~ V4 + V9 + V11 + V13 + V15 + V19 + V24,data = mat_treino4[[i]]);# summary(mabaqd11)
t22 <- predict(m22, mat_teste4[[i]], type = "response")
t22 <- ifelse(t22$class == 1, 1, 0)
cp22[i,j] <- correct_prediction(table(t22,mat_teste4[[i]]$Class))

m23 <- qda(formula = Class ~ V4 + V9 + V11 + V13 + V15 + V19 + V24,data = mat_treino5[[i]]);# summary(mabaqd22)
t23 <- predict(m23, mat_teste5[[i]], type = "response")
t23 <- ifelse(t23$class == 1, 1, 0)
cp23[i,j] <- correct_prediction(table(t23,mat_teste5[[i]]$Class))

m24 <- qda(formula = Class ~ V4 + V9 + V11 + V13 + V15 + V19 + V24,data = mat_treino6[[i]]);# summary(mabaqd33)
t24 <- predict(m24, mat_teste6[[i]], type = "response")
t24 <- ifelse(t24$class == 1, 1, 0)
cp24[i,j] <- correct_prediction(table(t24,mat_teste6[[i]]$Class))

########################################################################################
## ############################## # "5-NN",
######################################################################################
m25 = knnreg(mat_treino1[[i]][-31], mat_treino1[[i]]$Class)
t25 <- predict(m25, mat_teste1[[i]][-31])
t25 <- ifelse(t25 > 0.5, 1, 0)
cp25[i,j] <- correct_prediction(table(t25,mat_teste1[[i]]$Class))

m26 = knnreg(mat_treino2[[i]][-31], mat_treino2[[i]]$Class)
t26 <- predict(m26, mat_teste2[[i]][-31])
t26 <- ifelse(t26 > 0.5, 1, 0)
cp26[i,j] <- correct_prediction(table(t26,mat_teste2[[i]]$Class))

m27 = knnreg(mat_treino3[[i]][-31], mat_treino3[[i]]$Class)
t27 <- predict(m27, mat_teste3[[i]][-31])
t27 <- ifelse(t27 > 0.5, 1, 0)
cp27[i,j] <- correct_prediction(table(t27,mat_teste3[[i]]$Class))

#######################################
##      VARIAVEIS SIMETRICAS         ##
######################################
m28 = knnreg(mat_treino4[[i]][-1], mat_treino4[[i]]$Class)
t28 <- predict(m28, mat_teste4[[i]][-1])
t28 <- ifelse(t28 > 0.5, 1, 0)
cp28[i,j] <- correct_prediction(table(t28,mat_teste4[[i]]$Class))

m29 = knnreg(mat_treino5[[i]][-1], mat_treino5[[i]]$Class)
t29 <- predict(m29, mat_teste5[[i]][-1])
t29 <- ifelse(t29 > 0.5, 1, 0)
cp29[i,j] <- correct_prediction(table(t29,mat_teste5[[i]]$Class))

m30 = knnreg(mat_treino6[[i]][-1], mat_treino6[[i]]$Class)
t30 <- predict(m30, mat_teste6[[i]][-1])
t30 <- ifelse(t30 > 0.5, 1, 0)
cp30[i,j] <- correct_prediction(table(t30,mat_teste6[[i]]$Class))

########################################################################################
## ##############################  "3-NN",
######################################################################################
m31 = knnreg(mat_treino1[[i]][-31], mat_treino1[[i]]$Class, k = 3)
t31 <- predict(m31, mat_teste1[[i]][-31])
t31 <- ifelse(t31 > 0.5, 1, 0)
cp31[i,j] <- correct_prediction(table(t31,mat_teste1[[i]]$Class))

m32 = knnreg(mat_treino2[[i]][-31], mat_treino2[[i]]$Class, k = 3)
t32 <- predict(m32, mat_teste2[[i]][-31])
t32 <- ifelse(t32 > 0.5, 1, 0)
cp32[i,j] <- correct_prediction(table(t32,mat_teste2[[i]]$Class))

m33 = knnreg(mat_treino3[[i]][-31], mat_treino3[[i]]$Class, k = 3)
t33 <- predict(m33, mat_teste3[[i]][-31])
t33 <- ifelse(t33 > 0.5, 1, 0)
cp33[i,j] <- correct_prediction(table(t33,mat_teste3[[i]]$Class))

#######################################
##      VARIAVEIS SIMETRICAS         ##
######################################
m34 = knnreg(mat_treino4[[i]][-1], mat_treino4[[i]]$Class, k = 3)
t34 <- predict(m34, mat_teste4[[i]][-1])
t34 <- ifelse(t34 > 0.5, 1, 0)
cp34[i,j] <- correct_prediction(table(t34,mat_teste4[[i]]$Class))

m35 = knnreg(mat_treino5[[i]][-1], mat_treino5[[i]]$Class, k = 3)
t35 <- predict(m35, mat_teste5[[i]][-1])
t35 <- ifelse(t35 > 0.5, 1, 0)
cp35[i,j] <- correct_prediction(table(t35,mat_teste2[[i]]$Class))

m36 = knnreg(mat_treino6[[i]][-1], mat_treino6[[i]]$Class, k = 3)
t36 <- predict(m36, mat_teste6[[i]][-1])
t36 <- ifelse(t36 > 0.5, 1, 0)
cp36[i,j] <- correct_prediction(table(t36,mat_teste6[[i]]$Class))

########################################################################################
## ############################## "10-NN"
######################################################################################
m37 = knnreg(mat_treino1[[i]][-31], mat_treino1[[i]]$Class, k = 10)
t37 <- predict(m37, mat_teste1[[i]][-31])
t37 <- ifelse(t37 > 0.5, 1, 0)
cp37[i,j] <- correct_prediction(table(t37,mat_teste1[[i]]$Class))

m38 = knnreg(mat_treino2[[i]][-31], mat_treino2[[i]]$Class, k = 10)
t38 <- predict(m38, mat_teste2[[i]][-31])
t38 <- ifelse(t38 > 0.5, 1, 0)
cp38[i,j] <- correct_prediction(table(t38,mat_teste2[[i]]$Class))

m39 = knnreg(mat_treino3[[i]][-31], mat_treino3[[i]]$Class, k = 10)
t39 <- predict(m39, mat_teste3[[i]][-31])
t39 <- ifelse(t39 > 0.5, 1, 0)
cp39[i,j] <- correct_prediction(table(t39,mat_teste3[[i]]$Class))

#######################################
##      VARIAVEIS SIMETRICAS         ##
######################################
m40 = knnreg(mat_treino4[[i]][-1], mat_treino4[[i]]$Class, k = 10)
t40 <- predict(m40, mat_teste4[[i]][-1])
t40 <- ifelse(t40 > 0.5, 1, 0)
cp40[i,j] <- correct_prediction(table(t40,mat_teste4[[i]]$Class))

m41 = knnreg(mat_treino5[[i]][-1], mat_treino5[[i]]$Class, k = 10)
t41 <- predict(m41, mat_teste5[[i]][-1])
t41 <- ifelse(t41 > 0.5, 1, 0)
cp41[i,j] <- correct_prediction(table(t41,mat_teste2[[i]]$Class))

m42 = knnreg(mat_treino6[[i]][-1], mat_treino6[[i]]$Class, k = 10)
t42 <- predict(m42, mat_teste6[[i]][-1])
t42 <- ifelse(t42 > 0.5, 1, 0)
cp42[i,j] <- correct_prediction(table(t42,mat_teste6[[i]]$Class))

########################################################################################
## ############################## # Regressão Ridge
#######################################################################################
m43 = glmnet(as.matrix(mat_treino1[[i]][-31]), mat_treino1[[i]]$Class, alpha = 0) # Regressão Ridge
lambda.ridge1 <- cv.glmnet(as.matrix(mat_treino1[[i]][-31]), mat_treino1[[i]]$Class, alpha = 0) # Melhor lambda para a regressão ridge
t43 <- predict(m43, as.matrix(mat_teste1[[i]][-31]), s = lambda.ridge1$lambda.min)
t43 <- ifelse(t43 > 0.5, 1, 0)
cp43[i,j] <- correct_prediction(table(t43,mat_teste1[[i]]$Class))

m44 = glmnet(as.matrix(mat_treino2[[i]][-31]), mat_treino2[[i]]$Class, alpha = 0) # Regressão Ridge
lambda.ridge2 <- cv.glmnet(as.matrix(mat_treino2[[i]][-31]), mat_treino2[[i]]$Class, alpha = 0) # Melhor lambda para a regressão ridge
t44 <- predict(m44, as.matrix(mat_teste2[[i]][-31]), s = lambda.ridge2$lambda.min)
t44 <- ifelse(t44 > 0.5, 1, 0)
cp44[i,j] <- correct_prediction(table(t44,mat_teste2[[i]]$Class))


m45 = glmnet(as.matrix(mat_treino3[[i]][-31]), mat_treino3[[i]]$Class, alpha = 0) # Regressão Ridge
lambda.ridge3 <- cv.glmnet(as.matrix(mat_treino3[[i]][-31]), mat_treino3[[i]]$Class, alpha = 0) # Melhor lambda para a regressão ridge
t45 <- predict(m45, as.matrix(mat_teste3[[i]][-31]), s = lambda.ridge3$lambda.min)
t45 <- ifelse(t45 > 0.5, 1, 0)
cp45[i,j] <- correct_prediction(table(t45,mat_teste3[[i]]$Class))

####################################
##      VARIAVEIS SIMETRICAS      ##
####################################
m46 = glmnet(as.matrix(mat_treino4[[i]][-1]), mat_treino4[[i]]$Class, alpha = 0) # Regressão Ridge
lambda.ridge4 <- cv.glmnet(as.matrix(mat_treino4[[i]][-1]), mat_treino4[[i]]$Class, alpha = 0) # Melhor lambda para a regressão ridge
t46 <- predict(m46, as.matrix(mat_teste4[[i]][-1]), s = lambda.ridge4$lambda.min)
t46 <- ifelse(t46 > 0.5, 1, 0)
cp46[i,j] <- correct_prediction(table(t46,mat_teste4[[i]]$Class))

m47 = glmnet(as.matrix(mat_treino5[[i]][-1]), mat_treino5[[i]]$Class, alpha = 0) # Regressão Ridge
lambda.ridge5 <- cv.glmnet(as.matrix(mat_treino5[[i]][-1]), mat_treino5[[i]]$Class, alpha = 0) # Melhor lambda para a regressão ridge
t47 <- predict(m47, as.matrix(mat_teste5[[i]][-1]), s = lambda.ridge5$lambda.min)
t47 <- ifelse(t47 > 0.5, 1, 0)
cp47[i,j] <- correct_prediction(table(t47,mat_teste5[[i]]$Class))

m48 = glmnet(as.matrix(mat_treino6[[i]][-1]), mat_treino6[[i]]$Class, alpha = 0) # Regressão Ridge
lambda.ridge6 <- cv.glmnet(as.matrix(mat_treino6[[i]][-1]), mat_treino6[[i]]$Class, alpha = 0) # Melhor lambda para a regressão ridge
t48 <- predict(m48, as.matrix(mat_teste6[[i]][-1]), s = lambda.ridge6$lambda.min)
t48 <- ifelse(t48 > 0.5, 1, 0)
cp48[i,j] <- correct_prediction(table(t48,mat_teste6[[i]]$Class))

########################################################################################
## ############################## # LASSO
#######################################################################################
m49 = glmnet(as.matrix(mat_treino1[[i]][-31]), mat_treino1[[i]]$Class, alpha = 1) # Regressão Ridge
lambda.lasso1 <- cv.glmnet(as.matrix(mat_treino1[[i]][-31]), mat_treino1[[i]]$Class, alpha = 1) # Melhor lambda para a regressão ridge
t49 <- predict(m49, as.matrix(mat_teste1[[i]][-31]), s = lambda.lasso1$lambda.min)
t49 <- ifelse(t49 > 0.5, 1, 0)
cp49[i,j] <- correct_prediction(table(t49,mat_teste1[[i]]$Class))

m50 = glmnet(as.matrix(mat_treino2[[i]][-31]), mat_treino2[[i]]$Class, alpha = 1) # Regressão Ridge
lambda.lasso2 <- cv.glmnet(as.matrix(mat_treino2[[i]][-31]), mat_treino2[[i]]$Class, alpha = 1) # Melhor lambda para a regressão ridge
t50 <- predict(m50, as.matrix(mat_teste2[[i]][-31]), s = lambda.lasso2$lambda.min)
t50 <- ifelse(t50 > 0.5, 1, 0)
cp50[i,j] <- correct_prediction(table(t50,mat_teste2[[i]]$Class))


m51 = glmnet(as.matrix(mat_treino3[[i]][-31]), mat_treino3[[i]]$Class, alpha = 1) # Regressão Ridge
lambda.ridge3 <- cv.glmnet(as.matrix(mat_treino3[[i]][-31]), mat_treino3[[i]]$Class, alpha = 1) # Melhor lambda para a regressão ridge
t51 <- predict(m51, as.matrix(mat_teste3[[i]][-31]), s = lambda.lasso3$lambda.min)
t51 <- ifelse(t51 > 0.5, 1, 0)
cp51[i,j] <- correct_prediction(table(t51,mat_teste3[[i]]$Class))

####################################
##      VARIAVEIS SIMETRICAS      ##
####################################
m52 = glmnet(as.matrix(mat_treino4[[i]][-1]), mat_treino4[[i]]$Class, alpha = 1) # Regressão Ridge
lambda.lasso4 <- cv.glmnet(as.matrix(mat_treino4[[i]][-1]), mat_treino4[[i]]$Class, alpha = 1) # Melhor lambda para a regressão ridge
t52 <- predict(m52, as.matrix(mat_teste4[[i]][-1]), s = lambda.lasso4$lambda.min)
t52 <- ifelse(t52 > 0.5, 1, 0)
cp52[i,j] <- correct_prediction(table(t52,mat_teste4[[i]]$Class))

m53 = glmnet(as.matrix(mat_treino5[[i]][-1]), mat_treino5[[i]]$Class, alpha = 1) # Regressão Ridge
lambda.lasso5 <- cv.glmnet(as.matrix(mat_treino5[[i]][-1]), mat_treino5[[i]]$Class, alpha = 1) # Melhor lambda para a regressão ridge
t53 <- predict(m53, as.matrix(mat_teste5[[i]][-1]), s = lambda.lasso5$lambda.min)
t53 <- ifelse(t53 > 0.5, 1, 0)
cp53[i,j] <- correct_prediction(table(t53,mat_teste5[[i]]$Class))

m54 = glmnet(as.matrix(mat_treino6[[i]][-1]), mat_treino6[[i]]$Class, alpha = 1) # Regressão Ridge
lambda.lasso6 <- cv.glmnet(as.matrix(mat_treino6[[i]][-1]), mat_treino6[[i]]$Class, alpha = 1) # Melhor lambda para a regressão ridge
t54 <- predict(m54, as.matrix(mat_teste6[[i]][-1]), s = lambda.lasso6$lambda.min)
t54 <- ifelse(t54 > 0.5, 1, 0)
cp54[i,j] <- correct_prediction(table(t54,mat_teste6[[i]]$Class))

########################################################################################
## ############################## # ElasticNet
#######################################################################################
m55 = glmnet(Class ~ Time + V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + V10 + V11 + V12 + V13 + 
               V14 + V15 + V16 + V17 + V18 + V19 + V20 + V21 + V22 + V23 + V24 + V25 + V26 + V27 + 
               V28 + Amount, data = mat_treino1[[i]], alpha = 0.5) # ElasticNet
t55 <- predict(m55, mat_teste1[[i]], type = "response")
cp55[i,j] <- cp(t55,mat_teste1[[i]][31])
rae55[i,j] <- rae(t55,mat_teste1[[i]][31])
mae55[i,j] <- mae(t55,mat_teste1[[i]][31])
rrse55[i,j] <- rrse(t55,mat_teste1[[i]][31])
m56 = glmnet(Class ~ Time + V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + V10 + V11 + V12 + V13 + 
               V14 + V15 + V16 + V17 + V18 + V19 + V20 + V21 + V22 + V23 + V24 + V25 + V26 + V27 + 
               V28 + Amount, data = mat_treino2[[i]], alpha = 0.5) # ElasticNet
t56 <- predict(m56, mat_teste2[[i]], type = "response")
cp56[i,j] <- cp(t56,mat_teste2[[i]][31])
rae56[i,j] <- rae(t56,mat_teste2[[i]][31])
mae56[i,j] <- mae(t56,mat_teste2[[i]][31])
rrse56[i,j] <- rrse(t56,mat_teste2[[i]][31])
m57 = glmnet(Class ~ Time + V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + V10 + V11 + V12 + V13 + 
               V14 + V15 + V16 + V17 + V18 + V19 + V20 + V21 + V22 + V23 + V24 + V25 + V26 + V27 + 
               V28 + Amount, data = mat_treino3[[i]], alpha = 0.5) # ElasticNet
t57 <- predict(m57, mat_teste3[[i]], type = "response")
cp57[i,j] <- cp(t57,mat_teste3[[i]][31])
rae57[i,j] <- rae(t57,mat_teste3[[i]][31])
mae57[i,j] <- mae(t57,mat_teste3[[i]][31])
rrse57[i,j] <- rrse(t57,mat_teste3[[i]][31])

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
t58 <- predict(m58, mat_teste4[[i]], type = "response")
cp58[i,j] <- cp(t58,mat_teste4[[i]][1])
rae58[i,j] <- rae(t58,mat_teste4[[i]][1])
mae58[i,j] <- mae(t58,mat_teste4[[i]][1])
rrse58[i,j] <- rrse(t58,mat_teste4[[i]][1])
m59 = glmnet(Class ~ V4 + V9 + V11 + V13 + V15 + V19 + V24,data = mat_treino5[[i]], alpha = 0.5) # ElasticNet
t59 <- predict(m59, mat_teste5[[i]], type = "response")
cp59[i,j] <- cp(t59,mat_teste5[[i]][1])
rae59[i,j] <- rae(t59,mat_teste5[[i]][1])
mae59[i,j] <- mae(t59,mat_teste5[[i]][1])
rrse59[i,j] <- rrse(t59,mat_teste5[[i]][1])
m60 = glmnet(Class ~ V4 + V9 + V11 + V13 + V15 + V19 + V24,data = mat_treino6[[i]], alpha = 0.5) # ElasticNet
t60 <- predict(m60, mat_teste6[[i]], type = "response")
cp60[i,j] <- cp(t60,mat_teste6[[i]][1])
rae60[i,j] <- rae(t60,mat_teste6[[i]][1])
mae60[i,j] <- mae(t60,mat_teste6[[i]][1])
rrse60[i,j] <- rrse(t60,mat_teste6[[i]][1])

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
t61 <- predict(m61, mat_teste1[[i]], type = "response")
cp61[i,j] <- cp(t61,mat_teste1[[i]][31])
rae61[i,j] <- rae(t61,mat_teste1[[i]][31])
mae61[i,j] <- mae(t61,mat_teste1[[i]][31])
rrse61[i,j] <- rrse(t61,mat_teste1[[i]][31])
m62 <- rpart( Class ~ Time + V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + V10 + V11 + V12 + V13 + 
                V14 + V15 + V16 + V17 + V18 + V19 + V20 + V21 + V22 + V23 + V24 + V25 + V26 + V27 + 
                V28 + Amount, data = mat_treino2[[i]],method="class")
t62 <- predict(m62, mat_teste2[[i]], type = "response")
cp62[i,j] <- cp(t62,mat_teste2[[i]][31])
rae62[i,j] <- rae(t62,mat_teste2[[i]][31])
mae62[i,j] <- mae(t62,mat_teste2[[i]][31])
rrse62[i,j] <- rrse(t62,mat_teste2[[i]][31])
m63 <- rpart( Class ~ Time + V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + V10 + V11 + V12 + V13 + 
                V14 + V15 + V16 + V17 + V18 + V19 + V20 + V21 + V22 + V23 + V24 + V25 + V26 + V27 + 
                V28 + Amount, data = mat_treino3[[i]],method="class")
t63 <- predict(m63, mat_teste3[[i]], type = "response")
cp63[i,j] <- cp(t63,mat_teste3[[i]][31])
rae63[i,j] <- rae(t63,mat_teste3[[i]][31])
mae63[i,j] <- mae(t63,mat_teste3[[i]][31])
rrse63[i,j] <- rrse(t63,mat_teste3[[i]][31])

#######################################
##      VARIAVEIS SIMETRICAS         ##
#######################################
m64 <- rpart(Class ~ V4 + V9 + V11 + V13 + V15 + V19 + V24, data = mat_treino4[[i]],method="class")
t64 <- predict(m64, mat_teste4[[i]], type = "response")
cp64[i,j] <- cp(t64,mat_teste4[[i]][1])
rae64[i,j] <- rae(t64,mat_teste4[[i]][1])
mae64[i,j] <- mae(t64,mat_teste4[[i]][1])
rrse64[i,j] <- rrse(t64,mat_teste4[[i]][1])
m65 <- rpart(Class ~ V4 + V9 + V11 + V13 + V15 + V19 + V24, data = mat_treino5[[i]],method="class")
t65 <- predict(m65, mat_teste5[[i]], type = "response")
cp65[i,j] <- cp(t65,mat_teste5[[i]][1])
rae65[i,j] <- rae(t65,mat_teste5[[i]][1])
mae65[i,j] <- mae(t65,mat_teste5[[i]][1])
rrse65[i,j] <- rrse(t65,mat_teste5[[i]][1])
m66 <- rpart(Class ~ V4 + V9 + V11 + V13 + V15 + V19 + V24, data = mat_treino6[[i]],method="class")
t66 <- predict(m66, mat_teste6[[i]], type = "response")
cp66[i,j] <- cp(t66,mat_teste6[[i]][1])
rae66[i,j] <- rae(t66,mat_teste6[[i]][1])
mae66[i,j] <- mae(t66,mat_teste6[[i]][1])
rrse66[i,j] <- rrse(t66,mat_teste6[[i]][1])

########################################################################################
## ############################# # random Forest
#######################################################################################
m67 <- randomForest(Class ~ Time + V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + V10 + V11 + V12 + V13 + 
                       V14 + V15 + V16 + V17 + V18 + V19 + V20 + V21 + V22 + V23 + V24 + V25 + V26 + V27 + 
                       V28 + Amount, data = mat_treino1[[i]], ntree = 500)
t67 <- predict(m67, mat_teste1[[i]], type = "response")
cp67[i,j] <- cp(t67,mat_teste1[[i]][31])
rae67[i,j] <- rae(t67,mat_teste1[[i]][31])
mae67[i,j] <- mae(t67,mat_teste1[[i]][31])
rrse67[i,j] <- rrse(t67,mat_teste1[[i]][31])
m68 <- randomForest(Class ~ Time + V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + V10 + V11 + V12 + V13 + 
                       V14 + V15 + V16 + V17 + V18 + V19 + V20 + V21 + V22 + V23 + V24 + V25 + V26 + V27 + 
                       V28 + Amount, data = mat_treino2[[i]], ntree = 500)
t68 <- predict(m68, mat_teste2[[i]], type = "response")
cp68[i,j] <- cp(t68,mat_teste2[[i]][31])
rae68[i,j] <- rae(t68,mat_teste2[[i]][31])
mae68[i,j] <- mae(t68,mat_teste2[[i]][31])
rrse68[i,j] <- rrse(t68,mat_teste2[[i]][31])
m69 <- randomForest(Class ~ Time + V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + V10 + V11 + V12 + V13 + 
                       V14 + V15 + V16 + V17 + V18 + V19 + V20 + V21 + V22 + V23 + V24 + V25 + V26 + V27 + 
                       V28 + Amount, data = mat_treino3[[i]], ntree = 500)
t69 <- predict(m69, mat_teste3[[i]], type = "response")
cp69[i,j] <- cp(t69,mat_teste3[[i]][31])
rae69[i,j] <- rae(t69,mat_teste3[[i]][31])
mae69[i,j] <- mae(t69,mat_teste3[[i]][31])
rrse69[i,j] <- rrse(t69,mat_teste3[[i]][31])

#######################################
##      VARIAVEIS SIMETRICAS         ##
#######################################
m70 <- randomForest(Class ~ V4 + V9 + V11 + V13 + V15 + V19 + V24, data = mat_treino4[[i]], ntree = 500)
t70 <- predict(m70, mat_teste4[[i]], type = "response")
cp70[i,j] <- cp(t70,mat_teste4[[i]][1])
rae70[i,j] <- rae(t70,mat_teste4[[i]][1])
mae70[i,j] <- mae(t70,mat_teste4[[i]][1])
rrse70[i,j] <- rrse(t70,mat_teste4[[i]][1])
m71 <- randomForest(Class ~ V4 + V9 + V11 + V13 + V15 + V19 + V24, data = mat_treino5[[i]], ntree = 500)
t71 <- predict(m71, mat_teste5[[i]], type = "response")
cp71[i,j] <- cp(t71,mat_teste5[[i]][1])
rae71[i,j] <- rae(t71,mat_teste5[[i]][1])
mae71[i,j] <- mae(t71,mat_teste5[[i]][1])
rrse71[i,j] <- rrse(t71,mat_teste5[[i]][1])
m72 <- randomForest(Class ~ V4 + V9 + V11 + V13 + V15 + V19 + V24, data = mat_treino6[[i]], ntree = 500)
t72 <- predict(m72, mat_teste6[[i]], type = "response")
cp72[i,j] <- cp(t72,mat_teste6[[i]][1])

}
}
# Para prever todos esses valores é usar o predict.
# fited1 <- predict(m1,banco_teste1, type = "response")
