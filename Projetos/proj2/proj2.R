getwd()
#fonte de dados
#https://www.muratkoklu.com/datasets/vtdhnd07.php
#install.packages("readxl")
require("readxl")
require(zoo)
require(data.table)
require(tibble)
require(dplyr)
require(stringr)
#install.packages("corrplot")
require(corrplot)
require(caret)
require(randomForest)
#install.packages("randomForest")
dados<-read_xlsx('Acoustic_Extinguisher_Fire_Dataset.xlsx')
require(ggplot2)
require(pROC)
require(Amelia)

#resumo do dados
View(dados)
str(dados)

#convertendo variaveis string em fator
dados$FUEL <- as.factor(dados$FUEL)
dados$STATUS <- as.factor(dados$STATUS) 


#status esta balanceada
table(dados$STATUS)
#resposta sim

#sumario da varias numeros
summary(dados)

# mapa de valores nulos
missmap(dados, main = "valores nulos")
#mas nao procuramos zeros
table(dados4$AIRFLOW)
#1632 valores zero
boxplot(dados$AIRFLOW)



# variaveis numericas
dados1<-dados
dados1 <- dados1[, ! names(dados1) %in% c( "FUEL"), drop = F]
dados1$STATUS<-as.double(dados1$STATUS)
dados2<-dados
dados3<-dados[,c("AIRFLOW","DISTANCE","STATUS")]

#como nao ha outliers vou imputar a media
dados4<-dados
summary(dados$AIRFLOW)
dados4$AIRFLOW<-ifelse(dados4$AIRFLOW==0,median(dados4$AIRFLOW),dados4$AIRFLOW)
dados4$STATUS<-as.factor(dados4$STATUS)

boxplot(dados1) 

#correlacoes
M = cor(dados1)
corrplot(M)
#airflow e distance tem relevancia


#resumo estatistco
summary(dados2)
boxplot(dados2) 
str(dados2)

#modelo 1 todos os dados
#separar dados
split <- createDataPartition(y = dados2$STATUS, p = 0.7, list = FALSE)
# Criando dados de treino e de teste
dados_treino <- dados2[split,]
dados_teste <- dados2[-split,]

#modelo
glm.fit <- glm(STATUS ~.,family=binomial(link='logit'),data=dados_treino)
glm.probs <- predict(glm.fit,type = "response")
glm.pred <- ifelse(glm.probs > 0.5, 1, 0)

pred<-as.vector(glm.pred)
table(glm.pred,dados_treino$STATUS)
mean(glm.pred == dados_treino$STATUS)
#accuracy 0.8992 

#modelo 2 com dados imputados 
#separar dados
split <- createDataPartition(y = dados4$STATUS, p = 0.7, list = FALSE)
# Criando dados de treino e de teste
dados_treino <- dados4[split,]
dados_teste <- dados4[-split,]

glm.fit <- glm(STATUS ~.,family=binomial(link='logit'),data=dados_treino)
glm.probs <- predict(glm.fit,type = "response")
glm.pred <- ifelse(glm.probs > 0.5, 1, 0)

pred<-as.vector(glm.pred)
table(glm.pred,dados_treino$STATUS)
mean(glm.pred == dados_treino$STATUS)
#accuracy 0.8900

#modelo 3 
#install.packages("rpart")
require("party")
require(ModelMetrics)
require(rpart)
require(rpart.plot)

#separar dados
split <- createDataPartition(y = dados$STATUS, p = 0.7, list = FALSE)
# Criando dados de treino e de teste
dados_treino <- dados[split,]
dados_teste <- dados[-split,]

#decision-trees
tree <- rpart(formula=STATUS ~., data = dados_treino)
rpart.plot(tree)

summary(tree)


previsao <- predict(tree, dados_teste,type = 'class')
confMat<-table(previsao,dados_teste$STATUS)
accuracy <- sum(diag(confMat))/sum(confMat)
#0.8914

#curva roc
p1 <- predict(tree, dados_treino, type = 'prob')
p1 <- p1[,2]
r <- multiclass.roc(dados_treino$STATUS, p1, percent = TRUE)
roc <- r[['rocs']]
r1 <- roc[[1]]


plot.roc(r1,
         print.auc=TRUE,
         auc.polygon=TRUE,
         grid=c(0.1, 0.2),
         grid.col=c("green", "red"),
         max.auc.polygon=TRUE,
         auc.polygon.col="lightblue",
         print.thres=TRUE,
         main= 'ROC Curve')

