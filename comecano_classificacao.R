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

# hist com todas as variáveis explicativas e procurar as mais simetricas

# Com o banco interio
# Sem seleção de variaveis com todos.
# Selecao de variáveis com todos.
# Sem seleção de variaveis com as variáveis simétricas.
# Selecao de variáveis  com as variáveis simétricas.


# Com amostra inteira
# Sem seleção de variaveis com todos.
# Selecao de variáveis com todos.
# Sem seleção de variaveis com as variáveis simétricas.
# Selecao de variáveis  com as variáveis simétricas.


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


################################################################################################
## Sendo feita banco todo. Fazendo com a distribuição binomial com função de ligação logit    ##
################################################################################################
#####################################################################
## # Sem seleção de variáveis, com o banco inteiro
#####################################################################
# Modelo Banco Inteiro Sem Seleção de Variaveis Binomial Logit
mbissvbl <- glm(formula = Class ~ Time + V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + V10 + V11 + V12 + V13 + 
            V14 + V15 + V16 + V17 + V18 + V19 + V20 + V21 + V22 + V23 + V24 + V25 + V26 + V27 + 
            V28 + Amount,family = binomial(link = "logit"), data = banco)
summary(mbissvbl)
# Sem seleção de variáveis.
#####################################################################
## Sendo com o banco inteiro. # Selecao de variáveis com todos.
#####################################################################
# Com a seleção de variáveis, para verificar quais variáveis se mostram significativas para o 
# modelo a partir do teste t ao nível de significância de 5%. O modelo final ficou esse.
# Modelo Banco Inteiro Com Seleção de Variaveis Binomial Logit
mbicsvbl <- glm(formula = Class ~ V2 + V4 + V5 + V6 + V8 + V10 + V13 +  V14 + V16 + V20 + V21 + V22 + V23 + V27 + 
            V28 + Amount,family = binomial(link = "logit"), data = banco)
summary(m1)

################################################################################################
## Sendo feita em uma amostra. Fazendo com a distribuição binomial com função de ligação logit##
################################################################################################

#####################################################################
## Sendo feita em uma amostra. # Selecao de variáveis com todos.
#####################################################################
# Posições que assumem 1.
vec <- which(Class == 1)

# São 387 que em Class assume 0.
posicoes <- which(Class == 0)
# São 227459 que em Class assume 1.
set.seed(1992)
vec1 <- sample(x = posicoes, size = 387)
# Pegando uma quantidade de 1's porque o banco é muito desbalanceado, mas estamos balanceando 
# length(posicoes)/length(vec) = 587.7494. tem 587.7494 cerca de 99,8% de 0's
# em Class.

# As posições que serão pegas no banco.
po <- c(vec,vec1)
# Banco selecionado.
banco1 <- banco[po, ] 

#####################################################################
## # Sem Selecao de variáveis.
#####################################################################
# Modelo Amostra Sem Seleção de Variaveis Binomial Logit que é igual a com seleção de variáveis.
massvbl <- glm(formula = Class ~ Time + V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + V10 + V11 + V12 + V13 + 
                 V14 + V15 + V16 + V17 + V18 + V19 + V20 + V21 + V22 + V23 + V24 + V25 + V26 + V27 + 
                 V28 + Amount,family = binomial(link = "logit"), data = banco1)
summary(massvbl)

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
# Modelo Banco Inteiro Sem Seleção de Variáveis Simetricas Binomial Logit.
mbissvsbl <- glm(formula = Class ~ V4 + V9 + V11 + V13 + V15 + V19 + V24,family = binomial(link = "logit"), data = banco)
summary(mbissvsbl)

# MODELO COMPLETO COM SELEÇÃO DE VARIAVEIS COM AS VARIÁVEIS MAIS SIMETRICAS.
# Modelo Banco Inteiro Com Seleção de Variáveis Simetricas Binomial Logit.
mbicsvsbl <- glm(formula = Class ~ V4 + V9 + V11 + V13 + V15 + V19,family = binomial(link = "logit"), data = banco)
summary(mbicsvsbl)

# MODELO COMPLETO SEM SELEÇÃO DE VARIAVEIS COM AS VARIÁVEIS MAIS SIMETRICAS.
# Modelo Amostra Sem Seleção de Variáveis Simetricas Binomial Logit.
massvsbl <- glm(formula = Class ~ V4 + V9 + V11 + V13 + V15 + V19 + V24,family = binomial(link = "logit"), data = banco1)
summary(massvsbl)

# MODELO COMPLETO COM SELEÇÃO DE VARIAVEIS COM AS VARIÁVEIS MAIS SIMETRICAS.
# Modelo Amostra Com Seleção de Variáveis Simetricas Binomial Logit.
macsvsbl <- glm(formula = Class ~ V4 + V11 + V13,family = binomial(link = "logit"), data = banco1)
summary(macsvsbl)







#####################################################################
# MLG com dist. binomial e com função de ligação probit.          ###
#####################################################################
# Modelo Banco Inteiro Sem Seleção de Variaveis Binomail Probit.
mbissvbp <- glm(formula = Class ~ Time + V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + V10 + V11 + V12 + V13 + 
                    V14 + V15 + V16 + V17 + V18 + V19 + V20 + V21 + V22 + V23 + V24 + V25 + V26 + V27 + 
                    V28 + Amount,family = binomial(link = "probit"), data = banco)
summary(mbissvbp)
# O modelo final ficou com o intercepto mais todas as variáveis sendo significativos para o modelo,
# com distribuição binomial e função de ligação probit.

# Modelo Banco Inteiro Com Seleção de Variaveis Binomail Probit.
mbicsvbp <- glm(formula = Class ~ V2 + V4 + V5 + V6 + V8 + V10 + V13 + V14 + V16 + V20 + V21 + V22 + V27 + 
                  V28 + Amount,family = binomial(link = "probit"), data = banco)
summary(mbicsvbp)

# Modelo Amostra Sem Seleção de Variaveis Binomail Probit. é a mesma com e sem seleção de variáveis pq todas as 
# variáveis deram siginficativas para o modelo.
massvbp <- glm(formula = Class ~ Time + V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + V10 + V11 + V12 + V13 + 
                  V14 + V15 + V16 + V17 + V18 + V19 + V20 + V21 + V22 + V23 + V24 + V25 + V26 + V27 + 
                  V28 + Amount,family = binomial(link = "probit"), data = banco1)
summary(massvbp)



# MODELO COMPLETO SEM SELEÇÃO DE VARIAVEIS COM AS VARIÁVEIS MAIS SIMETRICAS.
# Modelo Banco Inteiro Sem Seleção de Variáveis Simetricas Binomial PROBIT.
mbissvsbp <- glm(formula = Class ~ V4 + V9 + V11 + V13 + V15 + V19 + V24,family = binomial(link = "probit"), data = banco)
summary(mbissvsbp)

# MODELO COMPLETO COM SELEÇÃO DE VARIAVEIS COM AS VARIÁVEIS MAIS SIMETRICAS.
# Modelo Banco Inteiro Com Seleção de Variáveis Simetricas Binomial PROBIT.
mbicsvsbp <- glm(formula = Class ~ V4 + V9 + V11 + V13 + V15 + V19,family = binomial(link = "probit"), data = banco)
summary(mbicsvsbp)

# MODELO COMPLETO SEM SELEÇÃO DE VARIAVEIS COM AS VARIÁVEIS MAIS SIMETRICAS.
# Modelo Amostra Sem Seleção de Variáveis Simetricas Binomial PROBIT.
massvsbp <- glm(formula = Class ~ V4 + V9 + V11 + V13 + V15 + V19 + V24,family = binomial(link = "probit"), data = banco1)
summary(massvsbp)

# MODELO COMPLETO COM SELEÇÃO DE VARIAVEIS COM AS VARIÁVEIS MAIS SIMETRICAS.
# Modelo Amostra Com Seleção de Variáveis Simetricas Binomial PROBIT.
massvsbp <- glm(formula = Class ~ V4 + V11 + V13 ,family = binomial(link = "probit"), data = banco1)
summary(massvsbp)

