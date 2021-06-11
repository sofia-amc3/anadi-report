# Libraries
library(rpart)
library(rpart.plot)
library(neuralnet)

# Variáveis globais

# Funções
MAE <- function(method, d) {
  mae <- mean(abs(d))
  cat("(", method, ") MAE: ", mae)
}

RMSE <- function(method, d) {
  rmse <- sqrt(mean(d^2))
  cat("(", method, ") RMSE: ", rmse)
}

normalize <- function(y) {
  (y - min(y)) / (max(y) - min(y))
}



################################################## REGRESSÃO ##################################################

# Exercício 1 --------------------------------------------------
data <- read.csv("")
# importação bruno
data <- read_excel("E:/college/mastersDegree/0thYear-preRequirements/2ndSemester/computerDataAnalysis/praticalWork/Iteration 02/countryagregatedata.xlsx")


str(data)
head(data)
dimension <- dim(data)
cat("Dimensão\nLinhas: ", dimension[1], "\t Colunas: ", dimension[2])
summary(data)

numberRows <- nrow(data)
numberColumns <- ncol(data)



# Exercício 2 --------------------------------------------------
# Variáveis




# Exercício 3 --------------------------------------------------
# Variáveis

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
MAE("Regressão Linear Simples", d);

# Raiz Quadrada do Erro Médio
RMSE("Regressão Linear Simples", d);



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
d <- data.test$life_expectancy - mlr.predict

# Erro Médio Absoluto
MAE("Regressão Linear Múltipla", d);

# Raiz Quadrada do Erro Médio
RMSE("Regressão Linear Múltipla", d);


# Alínea b)
# Obtenção da árvore de Regressão
rpart.model <- rpart(life_expectancy ~ ., method = "anova", data = data.train)
rpart.model

# Visualização da árvore de Regressão
rpart.plot(rpart.model, digits = 3)
rpart.plot(rpart.model, digits = 4, fallen.leaves = TRUE, type = 3, extra = 101)

# Previsão (avaliação do método)
rpart.predict <- predict(rpart.model, data.test)
d <- rpart.predict - data.test$life_expectancy

# Erro Médio Absoluto
MAE("Árvore de Regressão", d);

# Raiz Quadrada do Erro Médio
RMSE("Árvore de Regressão", d);


# Alínea c)
# Normaliza os dados
data.normal <- as.data.frame(lapply(data[, 4:numberColumns], normalize))
summary(data.normal$life_expectancy)

# Divisão dos dados normalizados através do critério holdout (70% treino, 30% teste)
data.train <- data.normal[index, ]
data.test <- data.normal[-index, ]

neural.model <- neuralnet(life_expectancy ~ ., data = data.train)
plot(neural.model)

columnIndex <- which(colnames(data.test) == "life_expectancy")

model.results <- compute(neural.model, data.test[, -columnIndex])
head(model.results$net.result)
neural.predict <- model.results$net.result
head(neural.predict)

cor(neural.predict, data.test$life_expectancy)

# Cálculo do MAE e RMSE
d <- neural.predict - data.test$life_expectancy

# Erro Médio Absoluto
MAE("Árvore de Regressão", d);

# Raiz Quadrada do Erro Médio
RMSE("Árvore de Regressão", d);


# Comparação dos resultados obtidos pelos modelos



################################################## CLASSIFICAÇÃO ##################################################

# Exercício 5 --------------------------------------------------
# Variáveis
rt.average <- mean(data$reproduction_rate)

# Separação do Rt em 0 e 1 (com a média como valor de corte)
split <- function (x) {
  if (x > rt.average)  "high"
  else  "low"
}
data$NiveldeRisco <- simplify2array(lapply(data$reproduction_rate, split))
table(data$NiveldeRisco)



# Exercício 6 --------------------------------------------------
# Variáveis
#data$NiveldeRisco <- as.numeric(as.factor(data$NiveldeRisco))

# Alínea a)


# Alínea b)


# Alínea c)



# Exercício 7 --------------------------------------------------
# Variáveis


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

table(data$ClassedeRisco)



# Exercício 8 --------------------------------------------------
# Conversão da Classe de Risco para classes númericas
# 1 - amarelo
# 2 - verde
# 3 - vermelho
data$ClassedeRisco <- as.numeric(as.factor(ClassedeRisco))


# Alínea a)


# Alínea b)


# Alínea c)
