# Libraries
library(rpart)
library(rpart.plot)
library(neuralnet)

# Variáveis globais

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


# 




# Exercício 4 --------------------------------------------------
# Variáveis


# Alínea a)


# Alínea b)


# Alínea c)




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


# 



# Exercício 6 --------------------------------------------------
# Variáveis


# Alínea a)


# Alínea b)


# Alínea c)



# Exercício 7 --------------------------------------------------
# Variáveis


# 



# Exercício 8 --------------------------------------------------
# Variáveis


# Alínea a)


# Alínea b)


# Alínea c)
