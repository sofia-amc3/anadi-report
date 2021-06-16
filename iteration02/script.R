# Libraries
library(car)
library(rpart)
library(rpart.plot)
library(neuralnet)
library(FNN)
library(caret)

# Variáveis globais

# Funções
MAE <- function(d) {
  mean(abs(d))
}

printMAE <- function(method, d) {
  mae <- MAE(d)
  cat("(", method, ") MAE: ", mae, "\n")
}

RMSE <- function(d) {
  sqrt(mean(d^2))
}

printRMSE <- function(method, d) {
  rmse <- RMSE(d)
  cat("(", method, ") RMSE: ", rmse, "\n")
}

normalize <- function(y) {
  (y - min(y)) / (max(y) - min(y))
}

classificationsModelsEvaluation <- function(data.test, predict.model) {
  m.conf <- table(data.test, predict.model)
  
  accuracy <- (m.conf[1, 1] + m.conf[2, 2]) / sum(m.conf)
  precision <- m.conf[1, 1] / (m.conf[1, 1] + m.conf[2, 1])
  sensitivity <- m.conf[1, 1] / (m.conf[1, 1] + m.conf[1, 2])
  specificity <- m.conf[2, 2] / (m.conf[2, 2] + m.conf[2, 1])
  f1 <- (2 * precision * sensitivity) / (precision * sensitivity)
  
  print("Matriz de confusão")
  print(m.conf)
  
  cat(paste("accuracy: ", accuracy, "%\n"))
  cat(paste("sensitivity: ", sensitivity, "%\n"))
  cat(paste("specificity: ", specificity, "%\n"))
  cat(paste("F1: ", f1, "\n"))
}



################################################## REGRESSÃO ##################################################

# Exercício 1 --------------------------------------------------
# importação bruno
data <- read.csv("E:/college/mastersDegree/0thYear-preRequirements/2ndSemester/computerDataAnalysis/praticalWork/Iteration 02/data.csv")

data <- read.csv("data.csv")


str(data)
head(data)

# Dimensão
dimension <- dim(data)
numberRows <- dimension[1]
numberColumns <- dimension[2]
cat("Dimensão\nLinhas: ", dimension[1], "\t Colunas: ", dimension[2])

#Sumário
summary(data)

# Normalização os dados
data.normal <- as.data.frame(lapply(data[, 4:numberColumns], normalize))
head(data.normal)



# Exercício 2 --------------------------------------------------
# Variáveis




# Exercício 3 --------------------------------------------------
# Divisão dos dados em dois subconjuntos - treino e teste - segundo o critério holdout (70% treino / 30% teste)
index <- sample(1:numberRows, as.integer(0.7 * numberRows))
data.train <- data[index, ]
data.test <- data[-index, ]


# Alínea a)
slr.model <- lm(total_deaths ~ new_cases, data = data.train)
slr.model
summary(slr.model)


# Alínea b)
plot(data.train$new_cases, data.train$total_deaths, pch = 20, 
     xlab = "Novos Casos", 
     ylab = "Total de Mortes", 
     main = "Diagrama de Dispersão e Reta de Regressão")
abline(slr.model$coefficients[1], slr.model$coefficients[2], col = "red")


# Alínea c)
slr.pred <- predict(slr.model, data.test)
d <- data.test$total_deaths - slr.pred

# Erro Médio Absoluto
printMAE("Regressão Linear Simples", d)

# Raiz Quadrada do Erro Médio
printRMSE("Regressão Linear Simples", d)



# Exercício 4 --------------------------------------------------
# Divisão dos dados em dois subconjuntos - treino e teste - segundo o critério holdout (70% treino / 30% teste)
set.seed(123)
index <- sample(1:numberRows, as.integer(0.7 * numberRows))
data.train <- data[index, 4:numberColumns]
data.test <- data[-index, 4:numberColumns]


# Alínea a)
mlr.model <- lm(life_expectancy ~ ., data = data.train)
mlr.model

summary(mlr.model)
summary(mlr.model)$coefficient

# Previsão (avaliação do método)
mlr.predict <- predict(mlr.model, data.test)
mlr.predict
mlr.d <- data.test$life_expectancy - mlr.predict

# Erro Médio Absoluto
printMAE("Regressão Linear Múltipla", mlr.d);

# Raiz Quadrada do Erro Médio
printRMSE("Regressão Linear Múltipla", mlr.d);


# Alínea b)
# Obtenção da árvore de Regressão
rpart.model <- rpart(life_expectancy ~ ., method = "anova", data = data.train)
rpart.model

# Visualização da árvore de Regressão
rpart.plot(rpart.model, digits = 3)
rpart.plot(rpart.model, digits = 4, fallen.leaves = TRUE, type = 3, extra = 101)

# Previsão (avaliação do método)
rpart.predict <- predict(rpart.model, data.test)
rpart.d <- rpart.predict - data.test$life_expectancy

# Erro Médio Absoluto
printMAE("Árvore de Regressão", rpart.d);

# Raiz Quadrada do Erro Médio
printRMSE("Árvore de Regressão", rpart.d);


# Alínea c)
# Divisão dos dados normalizados através do critério holdout (70% treino, 30% teste)
data.train <- data.normal[index, ]
data.test <- data.normal[-index, ]
columnIndex <- which(colnames(data.test) == "life_expectancy")

# Função que cria uma rede neural e apresenta o MAE e RMSE
neuralNetwork <- function(hidden) {
  neural.model <- neuralnet(life_expectancy ~ ., data = data.train, hidden = hidden)
  plot(neural.model)
  
  model.results <- compute(neural.model, data.test[, -columnIndex])
  head(model.results$net.result)
  neural.predict <- model.results$net.result
  head(neural.predict)
  
  correlation <- cor(neural.predict, data.test$life_expectancy)
  print(correlation)
  
  # Cálculo do MAE e RMSE
  d <- neural.predict - data.test$life_expectancy
  
  # Erro Médio Absoluto
  printMAE("Árvore de Regressão", d);
  
  # Raiz Quadrada do Erro Médio
  printRMSE("Árvore de Regressão", d);
  
  # Retorna os erros das previsões
  return(d)
}

neural.d <- neuralNetwork(1) # Rede neuronal com 1 nó interno
neuralNetwork(4) # Rede neuronal com 4 nós internos
neuralNetwork(c(3, 5)) # Rede neuronal com 2 níveis internos com 3 e 5 nós

# Comparação dos resultados obtidos pelos modelos
# Transformação dos erros para valores absolutos (positivos)
mlr.d <- mlr.d^2
rpart.d <- rpart.d^2
neural.d <- neural.d^2

errorsData <- data.frame(
  RPart = rpart.d,
  Neural = neural.d
)
SerrorsData <- stack(errorsData)

# Testes à normalidade
shapiro.test(rpart.d - neural.d)
lillie.test(rpart.d - neural.d)

# Teste à igualdade das variâncias
leveneTest(SerrorsData[, 1] ~ SerrorsData[, 2])

# Teste comparador das duas médias
t.test(rpart.d, neural.d, var.equal = FALSE, alternative = "two.sided")



################################################## CLASSIFICAÇÃO ##################################################

# Exercício 5 --------------------------------------------------
# Obtenção da média do Rt
rt.average <- mean(data$reproduction_rate)

# Separação do Rt em 0 e 1 (com a média como valor de corte)
split <- function (x) {
  if (x > rt.average)  "high"
  else  "low"
}
data$NiveldeRisco <- simplify2array(lapply(data$reproduction_rate, split))
numberColumns <- numberColumns + 1

# Verificação da frequência das duas classes
table(data$NiveldeRisco)



# Exercício 6 --------------------------------------------------
# Variáveis
#data$NiveldeRisco <- as.numeric(as.factor(data$NiveldeRisco))


# Alínea a)



# Alínea b)



# Alínea c)



# Exercício 7 --------------------------------------------------
# Remove a variável criada no exercício 5 e utilizada no exercício 6
data <- subset(data, select = -numberColumns)
numberColumns <- numberColumns - 1

# Separação do Rt e Incidência em verde, amarelo e vermelho (com base na Matriz de Risco)
ClassedeRisco <- c()

for (i in 1:numberRows) {
  incidence <- data[i, "incidence"]
  rt <- data[i, "reproduction_rate"]
  
  if (incidence < 120) {
    if (rt < 1)  value = "Verde"
    else  value = "Amarelo"
  } else {
    if (rt < 1)  value = "Amarelo"
    else  value = "Vermelho"
  }
  
  ClassedeRisco <- c(ClassedeRisco, value)
}

data$ClassedeRisco <- as.factor(ClassedeRisco)
numberColumns <- numberColumns + 1

# Verificação da frequência das classes
table(data$ClassedeRisco)



# Exercício 8 --------------------------------------------------
set.seed(456)
index <- sample(1:numberRows, as.integer(0.7 * numberRows))
data.train <- data[index, 4:numberColumns]
data.test <- data[-index, 4:numberColumns]


# Alínea a)
# Árvore de Regressão
rpart.model <- rpart(ClassedeRisco ~ ., method = "class", data = data.train)
par(xpd = TRUE)
plot(rpart.model, compress = TRUE)
text(rpart.model, use.n = TRUE)
rpart.plot(rpart.model)

# Previsão
rpart.predict <- predict(rpart.model, data.test, type = "class")
head(rpart.predict)

# Obtenção dos critérios de avaliação (Accuracy, Sensitivity, Specificity e F1)
confusionMatrix(data.test$ClassedeRisco, rpart.predict)


# Alínea b)
# Adição da ClassedeRisco aos dados normalizados
data.normal$ClassedeRisco <- as.factor(ClassedeRisco)
# Criação de colunas que diferenciam as classes de risco em valores de true/false
data.normal$amarelo <- ClassedeRisco == "Amarelo"
data.normal$verde <- ClassedeRisco == "Verde"
data.normal$vermelho <- ClassedeRisco == "Vermelho"

# Obtenção de novos dados de treino e teste através dos dados normalizados
data.train <- data.normal[index, ]
data.test <- data.normal[-index, ]

neural.model <- neuralnet(ClassedeRisco ~ ., data = data.train, hidden = 3)
plot(neural.model)

columnIndex <- 23
model.results <- compute(neural.model, data.test)
head(model.results$net.result)
neural.predict <- model.results$net.result
head(neural.predict)

idx <- apply(neural.predict, 1, which.max)
predicted <- as.factor(c('Amarelo', 'Verde', 'Vermelho')[idx])

confusionMatrix(predicted, data.test$ClassedeRisco)


# Alínea c)
