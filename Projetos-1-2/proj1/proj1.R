#fonte de dados
#https://data.mendeley.com/datasets/tb9yrptydn/2
getwd()
#install.packages("readxl")
require("readxl")
require(zoo)
require(data.table)
require(tibble)
require(dplyr)
require(stringr)
#install.packages("scales") 
require("scales")
#install.packages("corrplot")
require(corrplot)
require(caret)
require(randomForest)
#install.packages("randomForest")
#install.packages("Amelia")

dados<-read_xlsx('FEV-data-Excel.xlsx')
library(ggplot2)

#resumo do dados
View(dados)
str(dados)

# mapa de valores nulos
require(Amelia)
missmap(dados, main = "valores nulos")

#convertendo variaveis string em fator
dados$Make <- as.factor(dados$Make) 
levels(dados$Make)
summary(dados$Make)

#converter para factor
dadosvec<-as.vector( dados$`Car full name`)             
dados[1][] <- word(dadosvec, 1)

#colunas 1 e 2 ficam com os mesmo valores ,ou seja,tem que excluir umas das colunas
dados$`Car full name`<-NULL

# coluna excluida vamos fazer na outra coluna o corte de palavras para fatorizar depois
dadosvec<-as.vector( dados$`Model`)             
dados[2][] <- word(dadosvec, 1)

#transformar em fator
dados$Model=as.factor(as.vector( dados$`Model`))
dados$Make=as.factor(as.vector(dados$Make))


#type of brakes
#achar valores unicos no texto
teste <- dados %>% 
  group_by(dados$`Type of brakes`)%>% select('Type of brakes')%>%  distinct()

dados$`Type of brakes`<-as.factor(dados$`Type of brakes`)


teste<-factor(levels = c("front+rear", "front+drum(rear)"))
levels(teste) <- c("front+rear", "front+drum(rear)")
dados$`Type of brakes`<-teste


#`Drive type`
#achar valores unicos no texto
teste <- dados %>% 
  group_by(dados$`Drive type`)%>% select('Drive type')%>%  distinct()
teste<-as.vector(teste)

#converter
dados$`Drive type`
dadosvec<-as.vector(dados$`Drive type`)  
text<-as.factor(dadosvec)
dados$`Drive type`<-text

#strings convertidas em fatores falta lidarmos com numeros

#verificar nulos com strings

glimpse(dados)
table(is.na(dados$`Type of brakes`))
#ou
dados %>%
  distinct(dados$`Type of brakes`)

dados<-dados %>%
  filter(!is.na(dados$`Type of brakes`))

#permutando os numericos com media
dados$`Permissable gross weight [kg]`<-na.aggregate(dados$`Permissable gross weight [kg]`) 
dados$`Maximum load capacity [kg]`<-na.aggregate(dados$`Maximum load capacity [kg]`)
dados$`Acceleration 0-100 kph [s]`<-na.aggregate(dados$`Acceleration 0-100 kph [s]`)
dados$`mean - Energy consumption [kWh/100 km]`<-na.aggregate(dados$`Acceleration 0-100 kph [s]`)

# separar em treino e teste

#vetor de numeros
colsc <- c("Minimal price (gross) [PLN]", "Maximum torque [Nm]","Engine power [KM]" , "Battery capacity [kWh]",
          "Range (WLTP) [km]","Length [cm]","Wheelbase [cm]",
          "Width [cm]", "Height [cm]", "Minimal empty weight [kg]","Permissable gross weight [kg]","Maximum load capacity [kg]",
          "Number of seats","Number of doors","Tire size [in]","Maximum speed [kph]","Boot capacity (VDA) [l]",
          "Acceleration 0-100 kph [s]","Maximum DC charging power [kW]","mean - Energy consumption [kWh/100 km]")
colnames(dados)

dados1<-dados[,colsc]
colnames(dados1)<-c("Minimal price","Maximum torque","Engine power","Battery","Range","Length","Wheelbase",
                    "Width","Height","Minimal empty","Permissable","Maximum load",
                    "Number of seats",'Number of doors','Tire size','Maximum speed',"Boot capacity",
                    "Acceleration","Maximum charging","mean predictable")

#correlacoes
M = cor(dados1)
corrplot(M)

#resumo estatistco
summary(dados1)

quantile(dados1$`mean predictable`)
hist(dados1$`mean predictable`)

#alguns outliers ,mas,com o numero baixo de observacoes resolvi deixa-los
boxplot(dados1[,2:10]) 
boxplot(dados1[,11:20]) 
boxplot(dados1[,1]) 


#as variaveis
#variaveis irrelevantes doors,seats,maximun load,height
dados1 <- dados1[, ! names(dados1) %in% c( "Height","Maximum load",
                                          "Number of seats",'Number of doors'), drop = F]

#tentar uma regressao sem normalizar dados para ver se tem diferenca
#significativa depois
?cbind
#juntar com as variaveis fator
dados3<-dados[,c("Make","Model","Type of brakes","Drive type")]
dados2<-cbind(dados3,dados1)

#dados 4 sao algumas variaveis para teste(dados com correlacao forte)
dados4<-dados1[,c("Length","Minimal empty","Acceleration","mean predictable")]

#modelo 1 (com todas as variaveis)
split <- createDataPartition(y = dados2$`mean predictable`, p = 0.7, list = FALSE)
# Criando dados de treino e de teste
dados_treino <- dados2[split,]
dados_teste <- dados2[-split,]


# treinando
modelol_v1 <- train(`mean predictable` ~ ., data = dados_treino, method = "lm")
varImp(modelol_v1)
summary(modelol_v1)

#testando
?predict
predictedValues <- predict(modelol_v1, dados_teste)
table(dados_teste$`mean predictable`, predictedValues)
cor(dados_teste$`mean predictable`, predictedValues)

#grafico do modelo
ggplot( dados_teste,aes(x = `mean predictable`
                        , y = predictedValues )) +
  geom_point() +
  stat_smooth()

#modelo 2(so variaveis numericas)
split <- createDataPartition(y = dados1$`mean predictable`, p = 0.7, list = FALSE)
# Criando dados de treino e de teste
dados_treino <- dados1[split,]
dados_teste <- dados1[-split,]


# treinando
modelol_v2 <- train(`mean predictable` ~ ., data = dados_treino, method = "lm")
varImp(modelol_v2)
summary(modelol_v2)

#testando
?predict
predictedValues <- predict(modelol_v2, dados_teste)
table(dados_teste$`mean predictable`, predictedValues)
cor(dados_teste$`mean predictable`, predictedValues)

#grafico do modelo
ggplot( dados_teste,aes(x = `mean predictable`
                        , y = predictedValues )) +
  geom_point() +
  stat_smooth()

#modelo 3 com variaveis relevantes1
# Funcao do Caret para divisao dos dados
?createDataPartition
split <- createDataPartition(y = dados4$`mean predictable`, p = 0.7, list = FALSE)
# Criando dados de treino e de teste
dados_treino <- dados4[split,]
dados_teste <- dados4[-split,]


# treinando
modelol_v3 <- train(`mean predictable` ~ ., data = dados_treino, method = "lm")
varImp(modelol_v3)
summary(modelol_v3)

#testando
?predict
predictedValues <- predict(modelol_v3, dados_teste)
table(dados_teste$`mean predictable`, predictedValues)
cor(dados_teste$`mean predictable`, predictedValues)

summary(modelol_v3)

#grafico do modelo
ggplot( dados_teste,aes(x = `mean predictable`
                        , y = predictedValues )) +
  geom_point() +
  stat_smooth()

#fazendo testes de correlacao com dados 1(todas as variaveis)algumas variaveis ficam relevantes
#Length,`Minimal empty,acceleration
#colocando as variaveis fatores nenhuma fica relevante
#o modelo com dados 4 com ("Length","Minimal empty","Acceleration","mean predictable")
#apresentou 100 porcento (mesmo com underfit)

#salvando o modelo
saveRDS(modelol_v1, file = "caminho/modelol_v1.rds")
