#################################################################################
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

cv <- 10
k <- 10
# Matrizes que vao receber as méidas das estimativas de rood mean square error
# a quantidade dessas matrizes 
##############################################################################
rmse1 <- matrix(0,cv,k)
rmse2 <- matrix(0,cv,k)
rmse3 <- matrix(0,cv,k)
rmse4 <- matrix(0,cv,k)
rmse5 <- matrix(0,cv,k)
rmse6 <- matrix(0,cv,k)
rmse7 <- matrix(0,cv,k)
rmse8 <- matrix(0,cv,k)
rmse9 <- matrix(0,cv,k)
rmse10 <- matrix(0,cv,k)
rmse11 <- matrix(0,cv,k)
rmse12 <- matrix(0,cv,k)
rmse13 <- matrix(0,cv,k)
rmse14 <- matrix(0,cv,k)
rmse15 <- matrix(0,cv,k)
rmse16 <- matrix(0,cv,k)
rmse17 <- matrix(0,cv,k)
rmse18 <- matrix(0,cv,k)
rmse19 <- matrix(0,cv,k)
rmse20 <- matrix(0,cv,k)
rmse21 <- matrix(0,cv,k)
rmse22 <- matrix(0,cv,k)
rmse23 <- matrix(0,cv,k)
rmse24 <- matrix(0,cv,k)
rmse25 <- matrix(0,cv,k)
rmse26 <- matrix(0,cv,k)
rmse27 <- matrix(0,cv,k)
rmse28 <- matrix(0,cv,k)
rmse29 <- matrix(0,cv,k)
rmse30 <- matrix(0,cv,k)
rmse31 <- matrix(0,cv,k)
rmse32 <- matrix(0,cv,k)
rmse33 <- matrix(0,cv,k)
rmse34 <- matrix(0,cv,k)
rmse35 <- matrix(0,cv,k)
rmse36 <- matrix(0,cv,k)
rmse37 <- matrix(0,cv,k)
rmse38 <- matrix(0,cv,k)
rmse39 <- matrix(0,cv,k)
rmse40 <- matrix(0,cv,k)
rmse41 <- matrix(0,cv,k)
rmse42 <- matrix(0,cv,k)
rmse43 <- matrix(0,cv,k)
rmse44 <- matrix(0,cv,k)
rmse45 <- matrix(0,cv,k)
rmse46 <- matrix(0,cv,k)
rmse47 <- matrix(0,cv,k)
rmse48 <- matrix(0,cv,k)
rmse49 <- matrix(0,cv,k)
rmse50 <- matrix(0,cv,k)
rmse51 <- matrix(0,cv,k)
rmse52 <- matrix(0,cv,k)
rmse53 <- matrix(0,cv,k)
rmse54 <- matrix(0,cv,k)
rmse55 <- matrix(0,cv,k)
rmse56 <- matrix(0,cv,k)
rmse57 <- matrix(0,cv,k)
rmse58 <- matrix(0,cv,k)
rmse59 <- matrix(0,cv,k)
rmse60 <- matrix(0,cv,k)
rmse61 <- matrix(0,cv,k)
rmse62 <- matrix(0,cv,k)
rmse63 <- matrix(0,cv,k)
rmse64 <- matrix(0,cv,k)
rmse65 <- matrix(0,cv,k)
rmse66 <- matrix(0,cv,k)
rmse67 <- matrix(0,cv,k)
rmse68 <- matrix(0,cv,k)
rmse69 <- matrix(0,cv,k)
rmse70 <- matrix(0,cv,k)
rmse71 <- matrix(0,cv,k)
rmse72 <- matrix(0,cv,k)


# rae
##################################################################################
rae1 <- matrix(0,cv,k)
rae2 <- matrix(0,cv,k)
rae3 <- matrix(0,cv,k)
rae4 <- matrix(0,cv,k)
rae5 <- matrix(0,cv,k)
rae6 <- matrix(0,cv,k)
rae7 <- matrix(0,cv,k)
rae8 <- matrix(0,cv,k)
rae9 <- matrix(0,cv,k)
rae10 <- matrix(0,cv,k)
rae11 <- matrix(0,cv,k)
rae12 <- matrix(0,cv,k)
rae13 <- matrix(0,cv,k)
rae14 <- matrix(0,cv,k)
rae15 <- matrix(0,cv,k)
rae16 <- matrix(0,cv,k)
rae17 <- matrix(0,cv,k)
rae18 <- matrix(0,cv,k)
rae19 <- matrix(0,cv,k)
rae20 <- matrix(0,cv,k)
rae21 <- matrix(0,cv,k)
rae22 <- matrix(0,cv,k)
rae23 <- matrix(0,cv,k)
rae24 <- matrix(0,cv,k)
rae25 <- matrix(0,cv,k)
rae26 <- matrix(0,cv,k)
rae27 <- matrix(0,cv,k)
rae28 <- matrix(0,cv,k)
rae29 <- matrix(0,cv,k)
rae30 <- matrix(0,cv,k)
rae31 <- matrix(0,cv,k)
rae32 <- matrix(0,cv,k)
rae33 <- matrix(0,cv,k)
rae34 <- matrix(0,cv,k)
rae35 <- matrix(0,cv,k)
rae36 <- matrix(0,cv,k)
rae37 <- matrix(0,cv,k)
rae38 <- matrix(0,cv,k)
rae39 <- matrix(0,cv,k)
rae40 <- matrix(0,cv,k)
rae41 <- matrix(0,cv,k)
rae42 <- matrix(0,cv,k)
rae43 <- matrix(0,cv,k)
rae44 <- matrix(0,cv,k)
rae45 <- matrix(0,cv,k)
rae46 <- matrix(0,cv,k)
rae47 <- matrix(0,cv,k)
rae48 <- matrix(0,cv,k)
rae49 <- matrix(0,cv,k)
rae50 <- matrix(0,cv,k)
rae51 <- matrix(0,cv,k)
rae52 <- matrix(0,cv,k)
rae53 <- matrix(0,cv,k)
rae54 <- matrix(0,cv,k)
rae55 <- matrix(0,cv,k)
rae56 <- matrix(0,cv,k)
rae57 <- matrix(0,cv,k)
rae58 <- matrix(0,cv,k)
rae59 <- matrix(0,cv,k)
rae60 <- matrix(0,cv,k)
rae61 <- matrix(0,cv,k)
rae62 <- matrix(0,cv,k)
rae63 <- matrix(0,cv,k)
rae64 <- matrix(0,cv,k)
rae65 <- matrix(0,cv,k)
rae66 <- matrix(0,cv,k)
rae67 <- matrix(0,cv,k)
rae68 <- matrix(0,cv,k)
rae69 <- matrix(0,cv,k)
rae70 <- matrix(0,cv,k)
rae71 <- matrix(0,cv,k)
rae72 <- matrix(0,cv,k)

# mae
####################################################################################
mae1 <- matrix(0,cv,k)
mae2 <- matrix(0,cv,k)
mae3 <- matrix(0,cv,k)
mae4 <- matrix(0,cv,k)
mae5 <- matrix(0,cv,k)
mae6 <- matrix(0,cv,k)
mae7 <- matrix(0,cv,k)
mae8 <- matrix(0,cv,k)
mae9 <- matrix(0,cv,k)
mae10 <- matrix(0,cv,k)
mae11 <- matrix(0,cv,k)
mae12 <- matrix(0,cv,k)
mae13 <- matrix(0,cv,k)
mae14 <- matrix(0,cv,k)
mae15 <- matrix(0,cv,k)
mae16 <- matrix(0,cv,k)
mae17 <- matrix(0,cv,k)
mae18 <- matrix(0,cv,k)
mae19 <- matrix(0,cv,k)
mae20 <- matrix(0,cv,k)
mae21 <- matrix(0,cv,k)
mae22 <- matrix(0,cv,k)
mae23 <- matrix(0,cv,k)
mae24 <- matrix(0,cv,k)
mae25 <- matrix(0,cv,k)
mae26 <- matrix(0,cv,k)
mae27 <- matrix(0,cv,k)
mae28 <- matrix(0,cv,k)
mae29 <- matrix(0,cv,k)
mae30 <- matrix(0,cv,k)
mae31 <- matrix(0,cv,k)
mae32 <- matrix(0,cv,k)
mae33 <- matrix(0,cv,k)
mae34 <- matrix(0,cv,k)
mae35 <- matrix(0,cv,k)
mae36 <- matrix(0,cv,k)
mae37 <- matrix(0,cv,k)
mae38 <- matrix(0,cv,k)
mae39 <- matrix(0,cv,k)
mae40 <- matrix(0,cv,k)
mae41 <- matrix(0,cv,k)
mae42 <- matrix(0,cv,k)
mae43 <- matrix(0,cv,k)
mae44 <- matrix(0,cv,k)
mae45 <- matrix(0,cv,k)
mae46 <- matrix(0,cv,k)
mae47 <- matrix(0,cv,k)
mae48 <- matrix(0,cv,k)
mae49 <- matrix(0,cv,k)
mae50 <- matrix(0,cv,k)
mae51 <- matrix(0,cv,k)
mae52 <- matrix(0,cv,k)
mae53 <- matrix(0,cv,k)
mae54 <- matrix(0,cv,k)
mae55 <- matrix(0,cv,k)
mae56 <- matrix(0,cv,k)
mae57 <- matrix(0,cv,k)
mae58 <- matrix(0,cv,k)
mae59 <- matrix(0,cv,k)
mae60 <- matrix(0,cv,k)
mae61 <- matrix(0,cv,k)
mae62 <- matrix(0,cv,k)
mae63 <- matrix(0,cv,k)
mae64 <- matrix(0,cv,k)
mae65 <- matrix(0,cv,k)
mae66 <- matrix(0,cv,k)
mae67 <- matrix(0,cv,k)
mae68 <- matrix(0,cv,k)
mae69 <- matrix(0,cv,k)
mae70 <- matrix(0,cv,k)
mae71 <- matrix(0,cv,k)
mae72 <- matrix(0,cv,k)

# rrse
######################################################################################
rrse1 <- matrix(0,cv,k)
rrse2 <- matrix(0,cv,k)
rrse3 <- matrix(0,cv,k)
rrse4 <- matrix(0,cv,k)
rrse5 <- matrix(0,cv,k)
rrse6 <- matrix(0,cv,k)
rrse7 <- matrix(0,cv,k)
rrse8 <- matrix(0,cv,k)
rrse9 <- matrix(0,cv,k)
rrse10 <- matrix(0,cv,k)
rrse11 <- matrix(0,cv,k)
rrse12 <- matrix(0,cv,k)
rrse13 <- matrix(0,cv,k)
rrse14 <- matrix(0,cv,k)
rrse15 <- matrix(0,cv,k)
rrse16 <- matrix(0,cv,k)
rrse17 <- matrix(0,cv,k)
rrse18 <- matrix(0,cv,k)
rrse19 <- matrix(0,cv,k)
rrse20 <- matrix(0,cv,k)
rrse21 <- matrix(0,cv,k)
rrse22 <- matrix(0,cv,k)
rrse23 <- matrix(0,cv,k)
rrse24 <- matrix(0,cv,k)
rrse25 <- matrix(0,cv,k)
rrse26 <- matrix(0,cv,k)
rrse27 <- matrix(0,cv,k)
rrse28 <- matrix(0,cv,k)
rrse29 <- matrix(0,cv,k)
rrse30 <- matrix(0,cv,k)
rrse31 <- matrix(0,cv,k)
rrse32 <- matrix(0,cv,k)
rrse33 <- matrix(0,cv,k)
rrse34 <- matrix(0,cv,k)
rrse35 <- matrix(0,cv,k)
rrse36 <- matrix(0,cv,k)
rrse37 <- matrix(0,cv,k)
rrse38 <- matrix(0,cv,k)
rrse39 <- matrix(0,cv,k)
rrse40 <- matrix(0,cv,k)
rrse41 <- matrix(0,cv,k)
rrse42 <- matrix(0,cv,k)
rrse43 <- matrix(0,cv,k)
rrse44 <- matrix(0,cv,k)
rrse45 <- matrix(0,cv,k)
rrse46 <- matrix(0,cv,k)
rrse47 <- matrix(0,cv,k)
rrse48 <- matrix(0,cv,k)
rrse49 <- matrix(0,cv,k)
rrse50 <- matrix(0,cv,k)
rrse51 <- matrix(0,cv,k)
rrse52 <- matrix(0,cv,k)
rrse53 <- matrix(0,cv,k)
rrse54 <- matrix(0,cv,k)
rrse55 <- matrix(0,cv,k)
rrse56 <- matrix(0,cv,k)
rrse57 <- matrix(0,cv,k)
rrse58 <- matrix(0,cv,k)
rrse59 <- matrix(0,cv,k)
rrse60 <- matrix(0,cv,k)
rrse61 <- matrix(0,cv,k)
rrse62 <- matrix(0,cv,k)
rrse63 <- matrix(0,cv,k)
rrse64 <- matrix(0,cv,k)
rrse65 <- matrix(0,cv,k)
rrse66 <- matrix(0,cv,k)
rrse67 <- matrix(0,cv,k)
rrse68 <- matrix(0,cv,k)
rrse69 <- matrix(0,cv,k)
rrse70 <- matrix(0,cv,k)
rrse71 <- matrix(0,cv,k)
rrse72 <- matrix(0,cv,k)


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
vec2 <- sample(x = posicoes, size = 2 * length(vec))
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
t13 <- predict(m13, mat_teste1[[i]])
b13 <- ifelse(t13$class == 1, 1, 0)
rmse13[i,j] <- RMSE(b13,mat_teste1[[i]][31])

rae13[i,j] <- rae(t13$class,mat_teste1[[i]][31])

mae13[i,j] <- mae(t13$class,mat_teste1[[i]][31])

rrse13[i,j] <- rrse(t13$class,mat_teste1[[i]][31])

m14 <- lda(formula = Class ~ Time + V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + V10 + V11 + V12 + V13 + 
                 V14 + V15 + V16 + V17 + V18 + V19 + V20 + V21 + V22 + V23 + V24 + V25 + V26 + V27 + 
                 V28 + Amount, data = mat_treino2[[i]]); # summary(mabald2)
t14 <- predict(m14, mat_teste2[[i]], type = "response")
rmse14[i,j] <- RMSE(t14$class,mat_teste2[[i]][31])
rae14[i,j] <- rae(t14$class,mat_teste2[[i]][31])
mae14[i,j] <- mae(t14$class,mat_teste2[[i]][31])
rrse14[i,j] <- rrse(t14$class,mat_teste2[[i]][31])
m15 <- lda(formula = Class ~ Time + V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + V10 + V11 + V12 + V13 + 
                 V14 + V15 + V16 + V17 + V18 + V19 + V20 + V21 + V22 + V23 + V24 + V25 + V26 + V27 + 
                 V28 + Amount, data = mat_treino3[[i]]); # summary(mabald3)
t15 <- predict(m15, mat_teste3[[i]], type = "response")
rmse15[i,j] <- RMSE(t15$class,mat_teste3[[i]][31])
rae15[i,j] <- rae(t15$class,mat_teste3[[i]][31])
mae15[i,j] <- mae(t15$class,mat_teste3[[i]][31])
rrse15[i,j] <- rrse(t15$class,mat_teste3[[i]][31])
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
t18 <- predict(m18, mat_teste6[[i]], type = "response")
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
t23 <- predict(m23, mat_teste5[[i]], type = "response")
rmse23[i,j] <- RMSE(t23,mat_teste5[[i]][1])
rae23[i,j] <- rae(t23,mat_teste5[[i]][1])
mae23[i,j] <- mae(t23,mat_teste5[[i]][1])
rrse23[i,j] <- rrse(t23,mat_teste5[[i]][1])
m24 <- qda(formula = Class ~ V4 + V9 + V11 + V13 + V15 + V19 + V24,data = mat_treino6[[i]]);# summary(mabaqd33)
t24 <- predict(m24, mat_teste6[[i]], type = "response")
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
t25 <- predict(m25, mat_teste1[[i]], type = "response")
rmse25[i,j] <- RMSE(t25,mat_teste1[[i]][31])
rae25[i,j] <- rae(t25,mat_teste1[[i]][31])
mae25[i,j] <- mae(t25,mat_teste1[[i]][31])
rrse25[i,j] <- rrse(t25,mat_teste1[[i]][31])
m26 = knnreg(Class ~ Time + V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + V10 + V11 + V12 + V13 + 
               V14 + V15 + V16 + V17 + V18 + V19 + V20 + V21 + V22 + V23 + V24 + V25 + V26 + V27 + 
               V28 + Amount, data = mat_treino2[[i]])
t26 <- predict(m26, mat_teste2[[i]], type = "response")
rmse26[i,j] <- RMSE(t26,mat_teste2[[i]][31])
rae26[i,j] <- rae(t26,mat_teste2[[i]][31])
mae26[i,j] <- mae(t26,mat_teste2[[i]][31])
rrse26[i,j] <- rrse(t26,mat_teste2[[i]][31])
m27 = knnreg(Class ~ Time + V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + V10 + V11 + V12 + V13 + 
               V14 + V15 + V16 + V17 + V18 + V19 + V20 + V21 + V22 + V23 + V24 + V25 + V26 + V27 + 
               V28 + Amount, data = mat_treino3[[i]])
t27 <- predict(m27, mat_teste3[[i]], type = "response")
rmse27[i,j] <- RMSE(t27,mat_teste3[[i]][31])
rae27[i,j] <- rae(t27,mat_teste3[[i]][31])
mae27[i,j] <- mae(t27,mat_teste3[[i]][31])
rrse27[i,j] <- rrse(t27,mat_teste3[[i]][31])

#######################################
##      VARIAVEIS SIMETRICAS         ##
#######################################
m28 = knnreg(Class ~ V4 + V9 + V11 + V13 + V15 + V19 + V24,data = mat_treino4[[i]])
t28 <- predict(m28, mat_teste4[[i]], type = "response")
rmse28[i,j] <- RMSE(t27,mat_teste4[[i]][1])
rae28[i,j] <- rae(t27,mat_teste4[[i]][1])
mae28[i,j] <- mae(t27,mat_teste4[[i]][1])
rrse28[i,j] <- rrse(t27,mat_teste4[[i]][1])
m29 = knnreg(Class ~ V4 + V9 + V11 + V13 + V15 + V19 + V24,data = mat_treino5[[i]])
t29 <- predict(m29, mat_teste5[[i]], type = "response")
rmse29[i,j] <- RMSE(t29,mat_teste5[[i]][1])
rae29[i,j] <- rae(t29,mat_teste5[[i]][1])
mae29[i,j] <- mae(t29,mat_teste5[[i]][1])
rrse29[i,j] <- rrse(t29,mat_teste5[[i]][1])
m30 = knnreg(Class ~ V4 + V9 + V11 + V13 + V15 + V19 + V24,data = mat_treino6[[i]])
t30 <- predict(m30, mat_teste6[[i]], type = "response")
rmse30[i,j] <- RMSE(t30,mat_teste6[[i]][1])
rae30[i,j] <- rae(t30,mat_teste6[[i]][1])
mae30[i,j] <- mae(t30,mat_teste6[[i]][1])
rrse30[i,j] <- rrse(t30,mat_teste6[[i]][1])

########################################################################################
## ##############################  "3-NN",
######################################################################################
m31 = knnreg(Class ~ Time + V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + V10 + V11 + V12 + V13 + 
               V14 + V15 + V16 + V17 + V18 + V19 + V20 + V21 + V22 + V23 + V24 + V25 + V26 + V27 + 
               V28 + Amount, data = mat_treino1[[i]], k = 3)
t31 <- predict(m31, mat_teste1[[i]], type = "response")
rmse31[i,j] <- RMSE(t31,mat_teste1[[i]][31])
rae31[i,j] <- rae(t31,mat_teste1[[i]][31])
mae31[i,j] <- mae(t31,mat_teste1[[i]][31])
rrse31[i,j] <- rrse(t31,mat_teste1[[i]][31])
m32 = knnreg(Class ~ Time + V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + V10 + V11 + V12 + V13 + 
               V14 + V15 + V16 + V17 + V18 + V19 + V20 + V21 + V22 + V23 + V24 + V25 + V26 + V27 + 
               V28 + Amount, data = mat_treino2[[i]], k = 3)
t32 <- predict(m32, mat_teste2[[i]], type = "response")
rmse32[i,j] <- RMSE(t32,mat_teste2[[i]][31])
rae32[i,j] <- rae(t32,mat_teste2[[i]][31])
mae32[i,j] <- mae(t32,mat_teste2[[i]][31])
rrse32[i,j] <- rrse(t32,mat_teste2[[i]][31])
m33 = knnreg(Class ~ Time + V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + V10 + V11 + V12 + V13 + 
               V14 + V15 + V16 + V17 + V18 + V19 + V20 + V21 + V22 + V23 + V24 + V25 + V26 + V27 + 
               V28 + Amount, data = mat_treino3[[i]], k = 3)
t33 <- predict(m33, mat_teste3[[i]], type = "response")
rmse33[i,j] <- RMSE(t33,mat_teste3[[i]][31])
rae33[i,j] <- rae(t3,mat_teste3[[i]][31])
mae33[i,j] <- mae(t33,mat_teste3[[i]][31])
rrse33[i,j] <- rrse(t33,mat_teste3[[i]][31])

#######################################
##      VARIAVEIS SIMETRICAS         ##
#######################################
m34 = knnreg(Class ~ V4 + V9 + V11 + V13 + V15 + V19 + V24,data = mat_treino4[[i]], k = 3)
t34 <- predict(m34, mat_teste4[[i]], type = "response")
rmse34[i,j] <- RMSE(t34,mat_teste4[[i]][1])
rae34[i,j] <- rae(t34,mat_teste4[[i]][1])
mae34[i,j] <- mae(t34,mat_teste4[[i]][1])
rrse34[i,j] <- rrse(t1,mat_teste4[[i]][1])
m35 = knnreg(Class ~ V4 + V9 + V11 + V13 + V15 + V19 + V24,data = mat_treino5[[i]], k = 3)
t35 <- predict(m35, mat_teste5[[i]], type = "response")
rmse35[i,j] <- RMSE(t35,mat_teste5[[i]][1])
rae35[i,j] <- rae(t35,mat_teste5[[i]][1])
mae35[i,j] <- mae(t35,mat_teste5[[i]][1])
rrse35[i,j] <- rrse(t1,mat_teste5[[i]][1])
m36 = knnreg(Class ~ V4 + V9 + V11 + V13 + V15 + V19 + V24,data = mat_treino6[[i]], k = 3)
t36 <- predict(m36, mat_teste6[[i]], type = "response")
rmse36[i,j] <- RMSE(t36,mat_teste6[[i]][1])
rae36[i,j] <- rae(t36,mat_teste6[[i]][1])
mae36[i,j] <- mae(t36,mat_teste6[[i]][1])
rrse36[i,j] <- rrse(t36,mat_teste6[[i]][1])

########################################################################################
## ############################## "10-NN"
######################################################################################
m37 = knnreg(Class ~ Time + V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + V10 + V11 + V12 + V13 + 
               V14 + V15 + V16 + V17 + V18 + V19 + V20 + V21 + V22 + V23 + V24 + V25 + V26 + V27 + 
               V28 + Amount, data = mat_treino1[[i]], k = 10)
t37 <- predict(m37, mat_teste1[[i]], type = "response")
rmse37[i,j] <- RMSE(t37,mat_teste1[[i]][31])
rae37[i,j] <- rae(t37,mat_teste1[[i]][31])
mae37[i,j] <- mae(t37,mat_teste1[[i]][31])
rrse37[i,j] <- rrse(t37,mat_teste1[[i]][31])
m38 = knnreg(Class ~ Time + V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + V10 + V11 + V12 + V13 + 
               V14 + V15 + V16 + V17 + V18 + V19 + V20 + V21 + V22 + V23 + V24 + V25 + V26 + V27 + 
               V28 + Amount, data = mat_treino2[[i]], k = 10)
t38 <- predict(m38, mat_teste2[[i]], type = "response")
rmse38[i,j] <- RMSE(t38,mat_teste2[[i]][31])
rae38[i,j] <- rae(t38,mat_teste2[[i]][31])
mae38[i,j] <- mae(t38,mat_teste2[[i]][31])
rrse38[i,j] <- rrse(t38,mat_teste2[[i]][31])
m39 = knnreg(Class ~ Time + V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + V10 + V11 + V12 + V13 + 
               V14 + V15 + V16 + V17 + V18 + V19 + V20 + V21 + V22 + V23 + V24 + V25 + V26 + V27 + 
               V28 + Amount, data = mat_treino3[[i]], k = 10)
t39 <- predict(m39, mat_teste3[[i]], type = "response")
rmse39[i,j] <- RMSE(t39,mat_teste3[[i]][31])
rae39[i,j] <- rae(t39,mat_teste3[[i]][31])
mae39[i,j] <- mae(t39,mat_teste3[[i]][31])
rrse39[i,j] <- rrse(t39,mat_teste3[[i]][31])

#######################################
##      VARIAVEIS SIMETRICAS         ##
#######################################
m40 = knnreg(Class ~ V4 + V9 + V11 + V13 + V15 + V19 + V24,data = mat_treino4[[i]], k = 10)
t40 <- predict(m40, mat_teste4[[i]], type = "response")
rmse40[i,j] <- RMSE(t40,mat_teste4[[i]][1])
rae40[i,j] <- rae(t40,mat_teste4[[i]][1])
mae40[i,j] <- mae(t40,mat_teste4[[i]][1])
rrse40[i,j] <- rrse(t40,mat_teste4[[i]][1])
m41 = knnreg(Class ~ V4 + V9 + V11 + V13 + V15 + V19 + V24,data = mat_treino5[[i]], k = 10)
t41 <- predict(m41, mat_teste5[[i]], type = "response")
rmse41[i,j] <- RMSE(t41,mat_teste5[[i]][1])
rae41[i,j] <- rae(t41,mat_teste5[[i]][1])
mae41[i,j] <- mae(t41,mat_teste5[[i]][1])
rrse41[i,j] <- rrse(t41,mat_teste5[[i]][1])
m42 = knnreg(Class ~ V4 + V9 + V11 + V13 + V15 + V19 + V24,data = mat_treino6[[i]], k = 10)
t42 <- predict(m42, mat_teste6[[i]], type = "response")
rmse42[i,j] <- RMSE(t42,mat_teste6[[i]][1])
rae42[i,j] <- rae(t42,mat_teste6[[i]][1])
mae42[i,j] <- mae(t42,mat_teste6[[i]][1])
rrse42[i,j] <- rrse(t42,mat_teste6[[i]][1])

########################################################################################
## ############################## # Regressão Ridge
#######################################################################################
m43 = glmnet(Class ~ Time + V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + V10 + V11 + V12 + V13 + 
               V14 + V15 + V16 + V17 + V18 + V19 + V20 + V21 + V22 + V23 + V24 + V25 + V26 + V27 + 
               V28 + Amount, data = mat_treino1[[i]], alpha = 0) # Regressão Ridge
t43 <- predict(m43, mat_teste1[[i]], type = "response")
rmse43[i,j] <- RMSE(t43,mat_teste1[[i]][31])
rae43[i,j] <- rae(t43,mat_teste1[[i]][31])
mae43[i,j] <- mae(t43,mat_teste1[[i]][31])
rrse43[i,j] <- rrse(t43,mat_teste1[[i]][31])
m44 = glmnet(Class ~ Time + V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + V10 + V11 + V12 + V13 + 
               V14 + V15 + V16 + V17 + V18 + V19 + V20 + V21 + V22 + V23 + V24 + V25 + V26 + V27 + 
               V28 + Amount, data = mat_treino2[[i]], alpha = 0) # Regressão Ridge
t44 <- predict(m44, mat_teste2[[i]], type = "response")
rmse44[i,j] <- RMSE(t44,mat_teste2[[i]][31])
rae44[i,j] <- rae(t44,mat_teste2[[i]][31])
mae44[i,j] <- mae(t44,mat_teste2[[i]][31])
rrse44[i,j] <- rrse(t44,mat_teste2[[i]][31])
m45 = glmnet(Class ~ Time + V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + V10 + V11 + V12 + V13 + 
               V14 + V15 + V16 + V17 + V18 + V19 + V20 + V21 + V22 + V23 + V24 + V25 + V26 + V27 + 
               V28 + Amount, data = mat_treino3[[i]], alpha = 0) # Regressão Ridge
t45 <- predict(m45, mat_teste3[[i]], type = "response")
rmse45[i,j] <- RMSE(t45,mat_teste3[[i]][31])
rae45[i,j] <- rae(t45,mat_teste3[[i]][31])
mae45[i,j] <- mae(t45,mat_teste3[[i]][31])
rrse45[i,j] <- rrse(t45,mat_teste3[[i]][31])

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
t46 <- predict(m46, mat_teste4[[i]], type = "response")
rmse46[i,j] <- RMSE(t46,mat_teste4[[i]][1])
rae46[i,j] <- rae(t46,mat_teste4[[i]][1])
mae46[i,j] <- mae(t46,mat_teste4[[i]][1])
rrse46[i,j] <- rrse(t46,mat_teste4[[i]][1])
m47 = glmnet(Class ~ V4 + V9 + V11 + V13 + V15 + V19 + V24,data = mat_treino5[[i]], alpha = 0) # Regressão Ridge
t47 <- predict(m47, mat_teste5[[i]], type = "response")
rmse47[i,j] <- RMSE(t47,mat_teste5[[i]][1])
rae47[i,j] <- rae(t47,mat_teste5[[i]][1])
mae47[i,j] <- mae(t47,mat_teste5[[i]][1])
rrse47[i,j] <- rrse(t47,mat_teste5[[i]][1])
m48 = glmnet(Class ~ V4 + V9 + V11 + V13 + V15 + V19 + V24,data = mat_treino6[[i]], alpha = 0) # Regressão Ridge
t48 <- predict(m48, mat_teste6[[i]], type = "response")
rmse48[i,j] <- RMSE(t48,mat_teste6[[i]][1])
rae48[i,j] <- rae(t48,mat_teste6[[i]][1])
mae48[i,j] <- mae(t48,mat_teste6[[i]][1])
rrse48[i,j] <- rrse(t48,mat_teste6[[i]][1])


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
t49 <- predict(m49, mat_teste1[[i]], type = "response")
rmse49[i,j] <- RMSE(t49,mat_teste1[[i]][31])
rae49[i,j] <- rae(t49,mat_teste1[[i]][31])
mae49[i,j] <- mae(t49,mat_teste1[[i]][31])
rrse49[i,j] <- rrse(t49,mat_teste1[[i]][31])
m50 = glmnet(Class ~ Time + V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + V10 + V11 + V12 + V13 + 
               V14 + V15 + V16 + V17 + V18 + V19 + V20 + V21 + V22 + V23 + V24 + V25 + V26 + V27 + 
               V28 + Amount, data = mat_treino2[[i]], alpha = 1) # LASSO
t50 <- predict(m50, mat_teste2[[i]], type = "response")
rmse50[i,j] <- RMSE(t50,mat_teste2[[i]][31])
rae50[i,j] <- rae(t50,mat_teste2[[i]][31])
mae50[i,j] <- mae(t50,mat_teste2[[i]][31])
rrse50[i,j] <- rrse(t50,mat_teste2[[i]][31])
m51 = glmnet(Class ~ Time + V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + V10 + V11 + V12 + V13 + 
               V14 + V15 + V16 + V17 + V18 + V19 + V20 + V21 + V22 + V23 + V24 + V25 + V26 + V27 + 
               V28 + Amount, data = mat_treino3[[i]], alpha = 1) # LASSO
t51 <- predict(m51, mat_teste3[[i]], type = "response")
rmse51[i,j] <- RMSE(t51,mat_teste3[[i]][31])
rae51[i,j] <- rae(t51,mat_teste3[[i]][31])
mae51[i,j] <- mae(t51,mat_teste3[[i]][31])
rrse51[i,j] <- rrse(t51,mat_teste3[[i]][31])

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
t52 <- predict(m52, mat_teste4[[i]], type = "response")
rmse52[i,j] <- RMSE(t52,mat_teste4[[i]][1])
rae52[i,j] <- rae(t52,mat_teste4[[i]][1])
mae52[i,j] <- mae(t52,mat_teste4[[i]][1])
rrse52[i,j] <- rrse(t52,mat_teste4[[i]][1])
m53 = glmnet(Class ~ V4 + V9 + V11 + V13 + V15 + V19 + V24,data = mat_treino5[[i]], alpha = 1) # LASSO
t53 <- predict(m53, mat_teste5[[i]], type = "response")
rmse53[i,j] <- RMSE(t53,mat_teste5[[i]][1])
rae53[i,j] <- rae(t53,mat_teste5[[i]][1])
mae53[i,j] <- mae(t53,mat_teste5[[i]][1])
rrse53[i,j] <- rrse(t53,mat_teste5[[i]][1])
m54 = glmnet(Class ~ V4 + V9 + V11 + V13 + V15 + V19 + V24,data = mat_treino6[[i]], alpha = 1) # LASSO
t54 <- predict(m54, mat_teste6[[i]], type = "response")
rmse54[i,j] <- RMSE(t54,mat_teste6[[i]][1])
rae54[i,j] <- rae(t54,mat_teste6[[i]][1])
mae54[i,j] <- mae(t54,mat_teste6[[i]][1])
rrse54[i,j] <- rrse(t54,mat_teste6[[i]][1])

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
t55 <- predict(m55, mat_teste1[[i]], type = "response")
rmse55[i,j] <- RMSE(t55,mat_teste1[[i]][31])
rae55[i,j] <- rae(t55,mat_teste1[[i]][31])
mae55[i,j] <- mae(t55,mat_teste1[[i]][31])
rrse55[i,j] <- rrse(t55,mat_teste1[[i]][31])
m56 = glmnet(Class ~ Time + V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + V10 + V11 + V12 + V13 + 
               V14 + V15 + V16 + V17 + V18 + V19 + V20 + V21 + V22 + V23 + V24 + V25 + V26 + V27 + 
               V28 + Amount, data = mat_treino2[[i]], alpha = 0.5) # ElasticNet
t56 <- predict(m56, mat_teste2[[i]], type = "response")
rmse56[i,j] <- RMSE(t56,mat_teste2[[i]][31])
rae56[i,j] <- rae(t56,mat_teste2[[i]][31])
mae56[i,j] <- mae(t56,mat_teste2[[i]][31])
rrse56[i,j] <- rrse(t56,mat_teste2[[i]][31])
m57 = glmnet(Class ~ Time + V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + V10 + V11 + V12 + V13 + 
               V14 + V15 + V16 + V17 + V18 + V19 + V20 + V21 + V22 + V23 + V24 + V25 + V26 + V27 + 
               V28 + Amount, data = mat_treino3[[i]], alpha = 0.5) # ElasticNet
t57 <- predict(m57, mat_teste3[[i]], type = "response")
rmse57[i,j] <- RMSE(t57,mat_teste3[[i]][31])
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
rmse58[i,j] <- RMSE(t58,mat_teste4[[i]][1])
rae58[i,j] <- rae(t58,mat_teste4[[i]][1])
mae58[i,j] <- mae(t58,mat_teste4[[i]][1])
rrse58[i,j] <- rrse(t58,mat_teste4[[i]][1])
m59 = glmnet(Class ~ V4 + V9 + V11 + V13 + V15 + V19 + V24,data = mat_treino5[[i]], alpha = 0.5) # ElasticNet
t59 <- predict(m59, mat_teste5[[i]], type = "response")
rmse59[i,j] <- RMSE(t59,mat_teste5[[i]][1])
rae59[i,j] <- rae(t59,mat_teste5[[i]][1])
mae59[i,j] <- mae(t59,mat_teste5[[i]][1])
rrse59[i,j] <- rrse(t59,mat_teste5[[i]][1])
m60 = glmnet(Class ~ V4 + V9 + V11 + V13 + V15 + V19 + V24,data = mat_treino6[[i]], alpha = 0.5) # ElasticNet
t60 <- predict(m60, mat_teste6[[i]], type = "response")
rmse60[i,j] <- RMSE(t60,mat_teste6[[i]][1])
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
rmse61[i,j] <- RMSE(t61,mat_teste1[[i]][31])
rae61[i,j] <- rae(t61,mat_teste1[[i]][31])
mae61[i,j] <- mae(t61,mat_teste1[[i]][31])
rrse61[i,j] <- rrse(t61,mat_teste1[[i]][31])
m62 <- rpart( Class ~ Time + V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + V10 + V11 + V12 + V13 + 
                V14 + V15 + V16 + V17 + V18 + V19 + V20 + V21 + V22 + V23 + V24 + V25 + V26 + V27 + 
                V28 + Amount, data = mat_treino2[[i]],method="class")
t62 <- predict(m62, mat_teste2[[i]], type = "response")
rmse62[i,j] <- RMSE(t62,mat_teste2[[i]][31])
rae62[i,j] <- rae(t62,mat_teste2[[i]][31])
mae62[i,j] <- mae(t62,mat_teste2[[i]][31])
rrse62[i,j] <- rrse(t62,mat_teste2[[i]][31])
m63 <- rpart( Class ~ Time + V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + V10 + V11 + V12 + V13 + 
                V14 + V15 + V16 + V17 + V18 + V19 + V20 + V21 + V22 + V23 + V24 + V25 + V26 + V27 + 
                V28 + Amount, data = mat_treino3[[i]],method="class")
t63 <- predict(m63, mat_teste3[[i]], type = "response")
rmse63[i,j] <- RMSE(t63,mat_teste3[[i]][31])
rae63[i,j] <- rae(t63,mat_teste3[[i]][31])
mae63[i,j] <- mae(t63,mat_teste3[[i]][31])
rrse63[i,j] <- rrse(t63,mat_teste3[[i]][31])

#######################################
##      VARIAVEIS SIMETRICAS         ##
#######################################
m64 <- rpart(Class ~ V4 + V9 + V11 + V13 + V15 + V19 + V24, data = mat_treino4[[i]],method="class")
t64 <- predict(m64, mat_teste4[[i]], type = "response")
rmse64[i,j] <- RMSE(t64,mat_teste4[[i]][1])
rae64[i,j] <- rae(t64,mat_teste4[[i]][1])
mae64[i,j] <- mae(t64,mat_teste4[[i]][1])
rrse64[i,j] <- rrse(t64,mat_teste4[[i]][1])
m65 <- rpart(Class ~ V4 + V9 + V11 + V13 + V15 + V19 + V24, data = mat_treino5[[i]],method="class")
t65 <- predict(m65, mat_teste5[[i]], type = "response")
rmse65[i,j] <- RMSE(t65,mat_teste5[[i]][1])
rae65[i,j] <- rae(t65,mat_teste5[[i]][1])
mae65[i,j] <- mae(t65,mat_teste5[[i]][1])
rrse65[i,j] <- rrse(t65,mat_teste5[[i]][1])
m66 <- rpart(Class ~ V4 + V9 + V11 + V13 + V15 + V19 + V24, data = mat_treino6[[i]],method="class")
t66 <- predict(m66, mat_teste6[[i]], type = "response")
rmse66[i,j] <- RMSE(t66,mat_teste6[[i]][1])
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
rmse67[i,j] <- RMSE(t67,mat_teste1[[i]][31])
rae67[i,j] <- rae(t67,mat_teste1[[i]][31])
mae67[i,j] <- mae(t67,mat_teste1[[i]][31])
rrse67[i,j] <- rrse(t67,mat_teste1[[i]][31])
m68 <- randomForest(Class ~ Time + V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + V10 + V11 + V12 + V13 + 
                       V14 + V15 + V16 + V17 + V18 + V19 + V20 + V21 + V22 + V23 + V24 + V25 + V26 + V27 + 
                       V28 + Amount, data = mat_treino2[[i]], ntree = 500)
t68 <- predict(m68, mat_teste2[[i]], type = "response")
rmse68[i,j] <- RMSE(t68,mat_teste2[[i]][31])
rae68[i,j] <- rae(t68,mat_teste2[[i]][31])
mae68[i,j] <- mae(t68,mat_teste2[[i]][31])
rrse68[i,j] <- rrse(t68,mat_teste2[[i]][31])
m69 <- randomForest(Class ~ Time + V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + V10 + V11 + V12 + V13 + 
                       V14 + V15 + V16 + V17 + V18 + V19 + V20 + V21 + V22 + V23 + V24 + V25 + V26 + V27 + 
                       V28 + Amount, data = mat_treino3[[i]], ntree = 500)
t69 <- predict(m69, mat_teste3[[i]], type = "response")
rmse69[i,j] <- RMSE(t69,mat_teste3[[i]][31])
rae69[i,j] <- rae(t69,mat_teste3[[i]][31])
mae69[i,j] <- mae(t69,mat_teste3[[i]][31])
rrse69[i,j] <- rrse(t69,mat_teste3[[i]][31])

#######################################
##      VARIAVEIS SIMETRICAS         ##
#######################################
m70 <- randomForest(Class ~ V4 + V9 + V11 + V13 + V15 + V19 + V24, data = mat_treino4[[i]], ntree = 500)
t70 <- predict(m70, mat_teste4[[i]], type = "response")
rmse70[i,j] <- RMSE(t70,mat_teste4[[i]][1])
rae70[i,j] <- rae(t70,mat_teste4[[i]][1])
mae70[i,j] <- mae(t70,mat_teste4[[i]][1])
rrse70[i,j] <- rrse(t70,mat_teste4[[i]][1])
m71 <- randomForest(Class ~ V4 + V9 + V11 + V13 + V15 + V19 + V24, data = mat_treino5[[i]], ntree = 500)
t71 <- predict(m71, mat_teste5[[i]], type = "response")
rmse71[i,j] <- RMSE(t71,mat_teste5[[i]][1])
rae71[i,j] <- rae(t71,mat_teste5[[i]][1])
mae71[i,j] <- mae(t71,mat_teste5[[i]][1])
rrse71[i,j] <- rrse(t71,mat_teste5[[i]][1])
m72 <- randomForest(Class ~ V4 + V9 + V11 + V13 + V15 + V19 + V24, data = mat_treino6[[i]], ntree = 500)
t72 <- predict(m72, mat_teste6[[i]], type = "response")
rmse72[i,j] <- RMSE(t72,mat_teste6[[i]][1])
rae72[i,j] <- rae(t72,mat_teste6[[i]][1])
mae72[i,j] <- mae(t72,mat_teste6[[i]][1])
rrse72[i,j] <- rrse(t72,mat_teste6[[i]][1])
}
}
# Para prever todos esses valores é usar o predict.
# fited1 <- predict(m1,banco_teste1, type = "response")