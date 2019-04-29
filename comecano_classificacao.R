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
# Posições que assumem 1.
vec <- which(Class == 1)

# São 387 que em Class assume 0.
posicoes <- which(Class == 0)
# São 227459 que em Class assume 1.
set.seed(1992)
vec1 <- sample(x = posicoes, size = 2 * 387)
# Pegando duas vezes a quantidade de 1's porque o banco é muito desbalanceado,
# length(posicoes)/length(vec) = 587.7494. tem 587.7494 cerca de 99,8% de 0's
# em Class.

# As posições que serão pegas no banco.
po <- c(vec,vec1)
# Banco selecionado.
banco1 <- banco[po, ] 

# MLG com dist. binomial e com função de ligação logit.
m1 <- glm(formula = Class ~ Time + V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + V10 + V11 + V12 + V13 + 
                    V14 + V15 + V16 + V17 + V18 + V19 + V20 + V21 + V22 + V23 + V24 + V25 + V26 + V27 + 
                    V28 + Amount,family = binomial(link = "logit"), data = banco1)
summary(m1)

# Começando a seleção de variáveis.

# MLG com dist. binomial e com função de ligação probit.
m2 <- glm(formula = Class ~ Time + V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + V10 + V11 + V12 + V13 + 
                    V14 + V15 + V16 + V17 + V18 + V19 + V20 + V21 + V22 + V23 + V24 + V25 + V26 + V27 + 
                    V28 + Amount,family = binomial(link = "probit"), data = banco1)
