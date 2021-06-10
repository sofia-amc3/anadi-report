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


################################################## REGRESSÃO ##################################################

# Exercício 1 --------------------------------------------------
data <- read.csv("")

str(data)
head(data)
dimension <- dim(data)
cat("Dimensão\nLinhas: ", dimension[1], "\t Colunas: ", dimension[2])
summary(data)

numberRows <- nrow(data)



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
# Variáveis
set.seed(123)
index <- sample(1:numberRows, as.integer(0.7 * numberRows))
data.train <- data[index, ]
data.test <- data[-index, ]


# Alínea a)
mlr.model <- lm(sales ~ facebook + youtube + newspaper, data = treino)
mlr.model

summary(mlr.model)
summary(mlr.model)$coefficient

# Previsão (avaliação do método)
mlr.predict <- predict(mlr.model, data.test)
mlr.predict
d <- data.test$life_expectancy - mlr.predict

# Erro Médio Absoluto
MAE("Regressão Linear", d);

# Raiz Quadrada do Erro Médio
RMSE("Regressão Linear", d);


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
neural.model <- neuralnet(life_expectancy ~ ., data = data.train)
plot(neural.model)

model.results <- compute(concrete_model, data.test[1:8])
head(model.results$net.result)
neural.predict <- model.results$net.result
head(neural.predict)

cor(neural.predict, data.test$life_expectancy)



################################################## CLASSIFICAÇÃO ##################################################

# Exercício 5 --------------------------------------------------
# Variáveis
rt.average <- mean(data$reproduction_rate)

# Separação do Rt em 0 e 1 (com a média como valor de corte)
split <- function (x) { x > rt.average }
data$NiveldeRisco <- simplify2array(lapply(data$reproduction_rate, split))
data$NiveldeRisco <- as.numeric(data$NiveldeRisco)
table(data$NiveldeRisco)



# Exercício 6 --------------------------------------------------
# Variáveis


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

data$ClassedeRisco <- as.numeric(as.factor(ClassedeRisco))
table(data$ClassedeRisco)



# Exercício 8 --------------------------------------------------
# Variáveis


# Alínea a)


# Alínea b)


# Alínea c)
