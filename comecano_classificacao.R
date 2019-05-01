# Começando com a questão de classificação
install.packages("fBasics")
install.packages("MASS")
rm(list = ls())

library(MASS)
library(caret)
library(psych)
library(ggplot2)
library(VGAM)
library(ROCR)
library(pROC)
library(InformationValue)
library(glmnet)
library(e1071)
library(RSNNS)
library(fBasics)
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

# hist com todas as variáveis explicativas e procurar as mais simetricas

### Com Amostra balanceada.
# Sem seleção de variaveis com todos.
# Sem seleção de variaveis com as variáveis simétricas.
### Com amostra desbalanceada.
# Sem seleção de variaveis com todos.
# Sem seleção de variaveis com as variáveis simétricas.



################################################################################################
##                                       Olhando a simetria das variaveis                     ##
################################################################################################

par(mfrow = c(3,3))
apply(banco[1:6], 2, "hist") # "Time" "V1" "V2" "V3" "V4" "V5"
apply(banco[7:12], 2, "hist") # "V6" "V7" "V8" "V9"  "V10" "V11"
apply(banco[13:18], 2, "hist")# "V12" "V13" "V14" "V15" "V16" "V17"
apply(banco[19:24], 2, "hist") # "V18" "V19" "V20" "V21" "V22" "V23"
apply(banco[25:30], 2, "hist") # "V24" "V25" "V26" "V27" "V28" "Amount"

# As variáveis mais simetricas foram:
hist(V4)
hist(V9)
hist(V11)
hist(V13)
hist(V15) # OBS
hist(V19)
hist(V24)
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

################################################################################################
##                         Fazendo com a distribuição binomial com função de ligação logit    ##
################################################################################################

#################################################################################
## TODAS AS VARIAVEIS                                                          ##
#################################################################################

# Modelo Amostra balanceada Binomial Logit
mabbl1 <- glm(formula = Class ~ Time + V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + V10 + V11 + V12 + V13 + 
                V14 + V15 + V16 + V17 + V18 + V19 + V20 + V21 + V22 + V23 + V24 + V25 + V26 + V27 + 
                V28 + Amount,family = binomial(link = "logit"), data = banco1)
summary(mabbl1)
# Modelo Amostra Pouco balanceada Binomial Logit
mabbl2 <- glm(formula = Class ~ Time + V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + V10 + V11 + V12 + V13 + 
                V14 + V15 + V16 + V17 + V18 + V19 + V20 + V21 + V22 + V23 + V24 + V25 + V26 + V27 + 
                V28 + Amount,family = binomial(link = "logit"), data = banco2)
summary(mabbl2)
# Modelo Amostra desbalanceada Binomial Logit
mabbl3 <- glm(formula = Class ~ Time + V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + V10 + V11 + V12 + V13 + 
                V14 + V15 + V16 + V17 + V18 + V19 + V20 + V21 + V22 + V23 + V24 + V25 + V26 + V27 + 
                V28 + Amount,family = binomial(link = "logit"), data = banco3)
summary(mabbl3)


#################################################################################
## VARIAVEIS SIMETRICAS                                                        ##
#################################################################################
# Observando agora o modelo de regressão binomial com funcao de ligação logit
# sendo rodado esse modelo apenas com as variáveis mais simetricas do banco
# sendo elas 7 V4, V9, V11, V13, V5, V19, V24.
# As variáveis mais simetricas foram:
hist(V4)
hist(V9)
hist(V11)
hist(V13)
hist(V15) # OBS
hist(V19)
hist(V24)

# MODELO COMPLETO SEM SELEÇÃO DE VARIAVEIS COM AS VARIÁVEIS MAIS SIMETRICAS.
# Modelo Amostra Sem Seleção de Variáveis Simetricas Binomial Logit.


# Modelo Amostra balanceada Binomial Logit
mabbl11 <- glm(formula = Class ~ V4 + V9 + V11 + V13 + V15 + V19 + V24,family = binomial(link = "logit"), data = banco1)
summary(mabbl11)
# Modelo Amostra Pouco balanceada Binomial Logit
mabbl22 <- glm(formula = Class ~ V4 + V9 + V11 + V13 + V15 + V19 + V24,family = binomial(link = "logit"), data = banco2)
summary(mabbl22)
# Modelo Amostra desbalanceada Binomial Logit
mabbl33 <- glm(formula = Class ~ V4 + V9 + V11 + V13 + V15 + V19 + V24,family = binomial(link = "logit"), data = banco3)
summary(mabbl33)



################################################################################################
##          Fazendo com a distribuição binomial com função de ligação probit               ##
################################################################################################

#################################################################################
##                             TODAS AS VARIAVEIS                              ##
#################################################################################

# Modelo Amostra balanceada Analise de discriminante Quadrático  
mabbp1 <- glm(formula = Class ~ Time + V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + V10 + V11 + V12 + V13 + 
                V14 + V15 + V16 + V17 + V18 + V19 + V20 + V21 + V22 + V23 + V24 + V25 + V26 + V27 + 
                V28 + Amount,family = binomial(link = "probit"), data = banco1)
summary(mabbp1)
# Modelo Amostra Pouco balanceada Analise de discriminante Quadrático  
mabbp2 <- glm(formula = Class ~ Time + V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + V10 + V11 + V12 + V13 + 
                V14 + V15 + V16 + V17 + V18 + V19 + V20 + V21 + V22 + V23 + V24 + V25 + V26 + V27 + 
                V28 + Amount,family = binomial(link = "probit"), data = banco2)
summary(mabbp2)
# Modelo Amostra desbalanceada Analise de discriminante Quadrático  
mabbp3 <- glm(formula = Class ~ Time + V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + V10 + V11 + V12 + V13 + 
                V14 + V15 + V16 + V17 + V18 + V19 + V20 + V21 + V22 + V23 + V24 + V25 + V26 + V27 + 
                V28 + Amount,family = binomial(link = "probit"), data = banco3)
summary(mabbp3)


#################################################################################
##                          VARIAVEIS SIMETRICAS                               ##
#################################################################################
# Observando agora o modelo de regressão binomial com funcao de ligação probit
# sendo rodado esse modelo apenas com as variáveis mais simetricas do banco
# sendo elas 7 V4, V9, V11, V13, V5, V19, V24.
# As variáveis mais simetricas foram:
hist(V4)
hist(V9)
hist(V11)
hist(V13)
hist(V15) # OBS
hist(V19)
hist(V24)

# MODELO COMPLETO SEM SELEÇÃO DE VARIAVEIS COM AS VARIÁVEIS MAIS SIMETRICAS.
# Modelo Amostra Sem Seleção de Variáveis Simetricas Binomial Probit.


# Modelo Amostra balanceada Analise de discriminante Quadrático  
mabbp11 <- glm(formula = Class ~ V4 + V9 + V11 + V13 + V15 + V19 + V24,family = binomial(link = "probit"), data = banco1)
summary(mabbp11)
# Modelo Amostra Pouco balanceada Analise de discriminante Quadrático  
mabbp22 <- glm(formula = Class ~ V4 + V9 + V11 + V13 + V15 + V19 + V24,family = binomial(link = "probit"), data = banco2)
summary(mabbp22)
# Modelo Amostra desbalanceada Analise de discriminante Quadrático  
mabbp33 <- glm(formula = Class ~ V4 + V9 + V11 + V13 + V15 + V19 + V24,family = binomial(link = "probit"), data = banco3)
summary(mabbp33)


################################################################################################
##                         ANALISE LINEAR DE DISCRIMINANTE                                    ##
################################################################################################

#################################################################################
## TODAS AS VARIAVEIS                                                          ##
#################################################################################

library(MASS)
# Modelo Amostra balanceada Analise Linear Discriminante.
mabald1 <- lda(Class ~ Time + V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + V10 + V11 + V12 + V13 + 
                 V14 + V15 + V16 + V17 + V18 + V19 + V20 + V21 + V22 + V23 + V24 + V25 + V26 + V27 + 
                 V28 + Amount, data = banco1)
summary(mabald1)
# Modelo Amostra Pouco balanceada Analise Linear Discriminante.
mabald2 <- lda(formula = Class ~ Time + V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + V10 + V11 + V12 + V13 + 
                 V14 + V15 + V16 + V17 + V18 + V19 + V20 + V21 + V22 + V23 + V24 + V25 + V26 + V27 + 
                 V28 + Amount, data = banco2)
summary(mabald2)
# Modelo Amostra desbalanceada Analise Linear Discriminante.
mabald3 <- lda(formula = Class ~ Time + V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + V10 + V11 + V12 + V13 + 
                 V14 + V15 + V16 + V17 + V18 + V19 + V20 + V21 + V22 + V23 + V24 + V25 + V26 + V27 + 
                 V28 + Amount, data = banco3)
summary(mabald3)


#################################################################################
## VARIAVEIS SIMETRICAS                                                        ##
#################################################################################
# Observando agora o modelo de regressão binomial com funcao de ligação probit
# sendo rodado esse modelo apenas com as variáveis mais simetricas do banco
# sendo elas 7 V4, V9, V11, V13, V5, V19, V24.
# As variáveis mais simetricas foram:
hist(V4)
hist(V9)
hist(V11)
hist(V13)
hist(V15) # OBS
hist(V19)
hist(V24)

# Modelo Amostra Sem Seleção de Variáveis Simetricas Binomial Probit.


# Modelo Amostra balanceada Analise Linear Discriminante.
mabald11 <- lda(formula = Class ~ V4 + V9 + V11 + V13 + V15 + V19 + V24,data = banco1)
summary(mabald11)
# Modelo Amostra Pouco balanceada Analise Linear Discriminante.
mabald22 <- lda(formula = Class ~ V4 + V9 + V11 + V13 + V15 + V19 + V24,data = banco2)
summary(mabald22)
# Modelo Amostra desbalanceada Analise Linear Discriminante.
mabald33 <- lda(formula = Class ~ V4 + V9 + V11 + V13 + V15 + V19 + V24,data = banco3)
summary(mabald33)



################################################################################################
##                         ANALISE DE DISCRIMINANTE Quadrático                              ##
################################################################################################

#################################################################################
## TODAS AS VARIAVEIS                                                          ##
#################################################################################

library(MASS)
# Modelo Amostra balanceada Analise Linear Discriminante.
mabaqd1 <- qda(Class ~ Time + V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + V10 + V11 + V12 + V13 + 
                 V14 + V15 + V16 + V17 + V18 + V19 + V20 + V21 + V22 + V23 + V24 + V25 + V26 + V27 + 
                 V28 + Amount, data = banco1)
summary(mabaqd1)
# Modelo Amostra Pouco balanceada Analise Linear Discriminante.
mabaqd2 <- qda(formula = Class ~ Time + V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + V10 + V11 + V12 + V13 + 
                 V14 + V15 + V16 + V17 + V18 + V19 + V20 + V21 + V22 + V23 + V24 + V25 + V26 + V27 + 
                 V28 + Amount, data = banco2)
summary(mabaqd2)
# Modelo Amostra desbalanceada Analise Linear Discriminante.
mabaqd3 <- qda(formula = Class ~ Time + V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + V10 + V11 + V12 + V13 + 
                 V14 + V15 + V16 + V17 + V18 + V19 + V20 + V21 + V22 + V23 + V24 + V25 + V26 + V27 + 
                 V28 + Amount, data = banco3)
summary(mabaqd3)


#################################################################################
## VARIAVEIS SIMETRICAS                                                        ##
#################################################################################
# Observando agora o modelo de regressão binomial com funcao de ligação probit
# sendo rodado esse modelo apenas com as variáveis mais simetricas do banco
# sendo elas 7 V4, V9, V11, V13, V5, V19, V24.
# As variáveis mais simetricas foram:
hist(V4)
hist(V9)
hist(V11)
hist(V13)
hist(V15) # OBS
hist(V19)
hist(V24)

# Modelo Amostra Sem Seleção de Variáveis Simetricas Binomial Probit.


# Modelo Amostra balanceada Analise Linear Discriminante.
mabaqd11 <- qda(formula = Class ~ V4 + V9 + V11 + V13 + V15 + V19 + V24,data = banco1)
summary(mabaqd11)
# Modelo Amostra Pouco balanceada Analise Linear Discriminante.
mabaqd22 <- qda(formula = Class ~ V4 + V9 + V11 + V13 + V15 + V19 + V24,data = banco2)
summary(mabaqd22)
# Modelo Amostra desbalanceada Analise Linear Discriminante.
mabaqd33 <- qda(formula = Class ~ V4 + V9 + V11 + V13 + V15 + V19 + V24,data = banco3)
summary(mabaqd33)

# "5-NN", "3-NN", "10-NN"

