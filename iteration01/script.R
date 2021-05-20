# Libraries
library(nortest)
library(car)

# Variáveis globais
# Converte os dados da coluna 'date' para o tipo Date
data$date <- as.Date(as.character(data$date))

lineWidth <- 2

continentsNames <- c(northAmerica = "North America", southAmerica = "South America", europe = "Europe",
                africa = "Africa", asia = "Asia", oceania = "Oceania")
continentsCodes <- c(northAmerica = "OWID_NAM", southAmerica = "OWID_SAM", europe = "OWID_EUR",
                     africa = "OWID_AFR", asia = "OWID_ASI", oceania = "OWID_OCE")
continentsColors <- c(northAmerica = "firebrick2", southAmerica = "goldenrod1", europe = "dodgerblue3",
                      africa = "coral", asia = "mediumpurple1", oceania = "chartreuse4")

countriesCodes <- c(albania = "ALB", denmark = "DNK", germany = "DEU", russia = "RUS",
                    portugal = "PRT", spain = "ESP", italy = "ITA", uk = "GBR",
                    france = "FRA")
countriesColors <- c(albania = "", denmark = "firebrick1", germany = "gray22", russia = "gray98",
                     portugal = "red1", spain = "gold", italy = "chartreuse3", uk = "dodgerblue1",
                     france = "dodgerblue2")



################################################## ANÁLISE EXPLORATÓRIA DE DADOS ##################################################

# Alínea a) --------------------------------------------------
# Variáveis
columns <- c("date", "total_cases")
months <- seq(as.Date("2020-01-01"), by = "month", length.out = 16)
millions <- seq(0, 4 * 10^7, 1 * 10^7)

# Obtém os dados de cada continente
northAmerica <- subset(data, data$iso_code == continentsCodes["northAmerica"], columns)
southAmerica <- subset(data, data$iso_code == continentsCodes["southAmerica"], columns)
europe <- subset(data, data$iso_code == continentsCodes["europe"], columns)
africa <- subset(data, data$iso_code == continentsCodes["africa"], columns)
asia <- subset(data, data$iso_code == continentsCodes["asia"], columns)
oceania <- subset(data, data$iso_code == continentsCodes["oceania"], columns)

# Gráfico de Linhas
plot(northAmerica$date, northAmerica$total_cases, type = "l",
     main = "Total de infetados pelo COVID-19 por continente",
     xlab = "Data",
     ylab = "Total de infetados (em milhões)",
     ylim = c(0, millions[5]),
     col = continentsColors["northAmerica"], lwd = lineWidth, xaxt = "n", yaxt = "n")
lines(southAmerica$date, southAmerica$total_cases, type = "l", col = continentsColors["southAmerica"], lwd = lineWidth)
lines(europe$date, europe$total_cases, type = "l", col = continentsColors["europe"], lwd = lineWidth)
lines(africa$date, africa$total_cases, type = "l", col = continentsColors["africa"], lwd = lineWidth)
lines(asia$date, asia$total_cases, type = "l", col = continentsColors["asia"], lwd = lineWidth)
lines(oceania$date, oceania$total_cases, type = "l", col = continentsColors["oceania"], lwd = lineWidth)

# Eixo do x (Meses)
axis(1, months, format(months, "%b %y"))
axis(2, millions, format(round(millions / 10^6, 1), trim = TRUE))

# Legenda do Gráfico
legend("topleft", legend = continentsNames, pch = 15, col = continentsColors)


# Alínea b) --------------------------------------------------
# Variáveis
columns <- c("date", "total_cases_per_million")
months <- seq(as.Date("2020-01-01"), by = "month", length.out = 16)

# Obtém os dados de cada continente
northAmerica <- subset(data, data$iso_code == continentsCodes["northAmerica"], columns)
southAmerica <- subset(data, data$iso_code == continentsCodes["southAmerica"], columns)
europe <- subset(data, data$iso_code == continentsCodes["europe"], columns)
africa <- subset(data, data$iso_code == continentsCodes["africa"], columns)
asia <- subset(data, data$iso_code == continentsCodes["asia"], columns)
oceania <- subset(data, data$iso_code == continentsCodes["oceania"], columns)

# Gráfico de Linhas
plot(northAmerica$date, northAmerica$total_cases_per_million, type = "l", 
     main = "Total de infetados pelo COVID-19 por milhão de habitantes",
     xlab = "Data",
     ylab = "Total de infetados por milhão de habitantes", 
     col = continentsColors[1], lwd = lineWidth, xaxt = "n") # Nota: xaxt = desaparecer 2021
lines(southAmerica$date, southAmerica$total_cases_per_million, type = "l", col = continentsColors[2], lwd = lineWidth)
lines(europe$date, europe$total_cases_per_million, type = "l", col = continentsColors[3], lwd = lineWidth)
lines(africa$date, africa$total_cases_per_million, type = "l", col = continentsColors[4], lwd = lineWidth)
lines(asia$date, asia$total_cases_per_million, type = "l", col = continentsColors[5], lwd = lineWidth)
lines(oceania$date, oceania$total_cases_per_million, type = "l", col = continentsColors[6], lwd = lineWidth)

# Eixo do x (Meses)
axis(1, months, format(months, "%b %y"))

# Legenda do Gráfico
legend("topleft", legend = continentsNames, pch = 15, col = continentsColors)


# Alínea c) --------------------------------------------------
# Variáveis
column = c("total_deaths_per_million")
colors <- c(countriesColors["portugal"], countriesColors["spain"], countriesColors["italy"], countriesColors["uk"])

# Obtém o numero total de mortos por milhão de habitante em cada país
portugal <- (subset(data, data$iso_code == countriesCodes["portugal"], column))$total_deaths_per_million
spain <- (subset(data, data$iso_code == countriesCodes["spain"], column))$total_deaths_per_million
italy <- (subset(data, data$iso_code == countriesCodes["italy"], column))$total_deaths_per_million
uk <- (subset(data, data$iso_code == countriesCodes["uk"], column))$total_deaths_per_million

# Coloca todos os vetores com o mesmo numero de entradas
n <- max(length(portugal), length(spain), length(italy), length(uk))
length(portugal) <- n
length(spain) <- n
length(italy) <- n
length(uk) <- n

# Gráfico de caixas
countries <- cbind(portugal, spain, italy, uk)
boxplot(countries, names = c("Portugal", "Espanha", "Itália", "Reino Unido"),
        main = "Número de mortos diários pelo COVID-19 por milhão de habitantes",
        xlab = "Continentes",
        ylab = "Número de mortos diários por milhão de habitantes", 
        col = colors,
        range = 1.5) # range -> [Q1 - (1.5 * IQR), Q3 + (1.5 * IQR)]


# Alínea d) --------------------------------------------------
# Variáveis
columns <- c("total_deaths_per_million", "total_tests_per_thousand")
colors <- c("red1", "dodgerblue1")

# Esta função serve para obter os valores finais do número de mortos e testes
getMaxValues <- function(df) {
  maxDeathsPerMillion <- max(df$total_deaths_per_million, na.rm = TRUE)
  maxTests <- max(df$total_tests_per_thousand, na.rm = TRUE)
  return(c(maxDeathsPerMillion, maxTests))
}

# Obtém o numero total de mortos por milhão de habitante em cada pais
albania <- subset(data, data$iso_code == countriesCodes["albania"], columns)
denmark <- subset(data, data$iso_code == countriesCodes["denmark"], columns)
germany <- subset(data, data$iso_code == countriesCodes["germany"], columns)
russia <- subset(data, data$iso_code == countriesCodes["russia"], columns)

table <- as.matrix(data.frame("Albânia" = getMaxValues(albania),
                              "Dinamarca" = getMaxValues(denmark),
                              "Alemanha" = getMaxValues(germany),
                              "Rússia" = getMaxValues(russia)))

# Gráfico de barras
barplot(table,
        main = "Número total de mortos por milhão de habitante e\nnúmero total testes diários por milhar de habitante",
        ylab = "Número de habitantes",
        ylim = c(0, 3000),
        col = colors,
        beside = TRUE)

# Legenda do Gráfico
legend("topright",
       legend = c("Número total de mortos", "Número total de testes diários"),
       fill = colors)


# Alínea e) --------------------------------------------------
europeanCountries <- subset(data, data$continent == "Europe")
row <- which.max(europeanCountries$new_cases_per_million)
maxTotalCases <- europeanCountries[row, "new_cases_per_million"]
country <- europeanCountries[row, "location"]
date <- europeanCountries[row, "date"]
cat("País europeu que teve o maior número de infetados, por milhão de habitantes, num só dia: ",
    "\n\t", country, "\t", "Dia: ", format(date, "%d %b %Y"),
    "\t Casos: ", maxTotalCases)


# Alínea f) --------------------------------------------------
row <- which.max(data$reproduction_rate)
maxTransmissibilityRate <- data[row, "reproduction_rate"]
date <- data[row, "date"]
country <- data[row, "location"]
cat("Dia e País onde se registou a maior taxa de transmissibilidade do vírus:",
    "\n\t", country, "\t", "Dia: ", format(date, "%d %b %Y"),
    "\t Índice de transmissibilidade: ", maxTransmissibilityRate)


# Alínea g) --------------------------------------------------
# Variáveis
column = "total_deaths_per_million"

# Obtém o número total de mortos por milhão de habitante em cada continente
northAmerica <- (subset(data, data$iso_code == continentsCodes["northAmerica"], column))$total_deaths_per_million
southAmerica <- (subset(data, data$iso_code == continentsCodes["southAmerica"], column))$total_deaths_per_million
europe <- (subset(data, data$iso_code == continentsCodes["europe"], column))$total_deaths_per_million
africa <- (subset(data, data$iso_code == continentsCodes["africa"], column))$total_deaths_per_million
asia <- (subset(data, data$iso_code == continentsCodes["asia"], column))$total_deaths_per_million
oceania <- (subset(data, data$iso_code == continentsCodes["oceania"], column))$total_deaths_per_million

# Coloca todos os vetores com o mesmo numero de entradas
n <- max(length(northAmerica), length(southAmerica), length(europe), length(africa), length(asia), length(oceania))
length(northAmerica) <- n
length(southAmerica) <- n
length(europe) <- n
length(africa) <- n
length(asia) <- n
length(oceania) <- n

# Gráfico de caixas
continents <- cbind(northAmerica, southAmerica, europe, africa, asia, oceania)
boxplot(continents, names = continentsNames,
        main = "Número de mortos diários pelo COVID-19 por milhão de habitantes",
        xlab = "Continentes",
        ylab = "Número de mortos diários por milhão de habitantes", 
        col = continentsColors,
        range = 1.5) # range -> [Q1 - (1.5 * IQR), Q3 + (1.5 * IQR)]



################################################## ANÁLISE INFERENCIAL ##################################################

# Variáveis
firstDay <- as.Date("2020-04-01")
lastDay <- as.Date("2021-02-27")
days <- seq(firstDay, lastDay, "day")
conditions <- data$date >= firstDay &
              data$date <= lastDay
dataSample <- subset(data, conditions)


# Alínea a) --------------------------------------------------
set.seed(118)

# Variáveis
nrDays <- 30
columns <- c("date", "reproduction_rate")
selectedDays <- sample(days, nrDays)
selectedDays <- as.Date(as.character(selectedDays))

# Gets the data for each country for in selected days
uk <- subset(dataSample, dataSample$iso_code == countriesCodes["uk"], columns)
uk <- uk[is.element(uk$date, selectedDays), ]$reproduction_rate
portugal <- subset(dataSample, dataSample$iso_code == countriesCodes["portugal"], columns)
portugal <- portugal[is.element(portugal$date, selectedDays), ]$reproduction_rate

# Teste à normalidade -  Método #1
shapiro.test(uk - portugal)
# Teste à normalidade -  Método #2
lillie.test(uk - portugal)
# Teste à normalidade -  Método #3
qqnorm(uk - portugal, main = "Teste à normalidade das populações do Reino Unido e Portugal")
qqline(uk - portugal)

# T-Test
t.test(uk, portugal, paired = TRUE, alternative = "greater")


# Alínea b) --------------------------------------------------
set.seed(115)

# Variáveis
nrDays <- 15
columns <- c("date", "new_deaths_per_million")
selectedDays <- sample(days, nrDays)
selectedDays <- as.Date(as.character(selectedDays))

# Gets the data for each country in the selected days
spain <- subset(dataSample, dataSample$iso_code == countriesCodes["spain"], columns)
spain <- spain[is.element(spain$date, selectedDays), ]$new_deaths_per_million
france <- subset(dataSample, dataSample$iso_code == countriesCodes["france"], columns)
france <- france[is.element(france$date, selectedDays), ]$new_deaths_per_million
portugal <- subset(dataSample, dataSample$iso_code == countriesCodes["portugal"], columns)
portugal <- portugal[is.element(portugal$date, selectedDays), ]$new_deaths_per_million
italy <- subset(dataSample, dataSample$iso_code == countriesCodes["italy"], columns)
italy <- italy[is.element(italy$date, selectedDays), ]$new_deaths_per_million

countriesData <- c(spain, france, portugal, italy)
matrix <- matrix(countriesData, nrow = nrDays, ncol = 4)
friedman.test(matrix)


# Alínea c) --------------------------------------------------
# Variáveis
nrDays <- 30
columns <- c("date", "new_deaths_per_million")

# Gets the data for each continent in the selected days
set.seed(100);
selectedDays <- as.Date(as.character(sample(days, nrDays)))
africa <- subset(dataSample, dataSample$iso_code == continentsCodes["africa"], columns)
africa <- africa[is.element(africa$date, selectedDays), ]$new_deaths_per_million
set.seed(101);
selectedDays <- as.Date(as.character(sample(days, nrDays)))
asia <- subset(dataSample, dataSample$iso_code == continentsCodes["asia"], columns)
asia <- asia[is.element(asia$date, selectedDays), ]$new_deaths_per_million
set.seed(102);
selectedDays <- as.Date(as.character(sample(days, nrDays)))
europe <- subset(dataSample, dataSample$iso_code == continentsCodes["europe"], columns)
europe <- europe[is.element(europe$date, selectedDays), ]$new_deaths_per_million
set.seed(103);
selectedDays <- as.Date(as.character(sample(days, nrDays)))
northAmerica <- subset(dataSample, dataSample$iso_code == continentsCodes["northAmerica"], columns)
northAmerica <- northAmerica[is.element(northAmerica$date, selectedDays), ]$new_deaths_per_million
set.seed(104);
selectedDays <- as.Date(as.character(sample(days, nrDays)))
southAmerica <- subset(dataSample, dataSample$iso_code == continentsCodes["southAmerica"], columns)
southAmerica <- southAmerica[is.element(southAmerica$date, selectedDays), ]$new_deaths_per_million

continentsData <- data.frame(cbind(africa, asia, europe, northAmerica, southAmerica))
ScontinentsData <- stack(continentsData)

# Teste à normalidade -  Método #1
shapiro.test(continentsData[, 1])
shapiro.test(continentsData[, 2])
shapiro.test(continentsData[, 3])
shapiro.test(continentsData[, 4])
shapiro.test(continentsData[, 5])
# Verifica se as variâncias são iguais
leveneTest(ScontinentsData[, 1] ~ ScontinentsData[, 2])

# Teste comparador das médias (one-way ANOVA)
continents <- c(africa, asia, europe, northAmerica, southAmerica)
groups <- factor(c(rep("Africa", length(africa)),
                   rep("Asia", length(asia)),
                   rep("Europe", length(europe)),
                   rep("North America", length(northAmerica)),
                   rep("South America", length(southAmerica))
                 ))
anovaTest <- aov(continents ~ groups, data = continentsData)
summary(anovaTest)

# Análise post-hoc
post_hoc <- TukeyHSD(anovaTest, conf.level = 0.95)
plot(post_hoc, las = 2)
legend("bottomright",
       legend = c("As - Ásia", "Af - África", "Eu - Europa", "NA - América do Norte", "SA - América do Sul"))



################################################## ANÁLISE DE CORRELAÇÃO ##################################################

# Variáveis
conditions <- data$continent == "Europe" &
              data$population > 10^7 &
              data$date >= as.Date("2021-01-01")

# Obtém os países europeus com mais de 10 000 000 de habitantes
# e os seus dados do ano 2021
europeanCountriesData <- subset(data, conditions)
europeanCountries <- unique(europeanCountriesData$location)


# Alínea a) --------------------------------------------------
# Variáveis
columns <- c("date", "location", "reproduction_rate", "population_density")
x <- c()
y <- c()

# Guarda o número total de mortes e maior percentagem de populção com idade > 65 anos
#dos países europeus selecionados
for (country in europeanCountries) {
  countryData <- subset(europeanCountriesData, europeanCountriesData$location == country)
  maxReproductionRate <- max(countryData[,columns[3]], na.rm = TRUE)
  maxPopulationDensity <- max(countryData[,columns[4]], na.rm = TRUE) # na.rm -> remove os NA's
  
  x <- append(x, maxReproductionRate)
  y <- append(y, maxPopulationDensity)
}

reg <- lm(y ~ x)

# Teste aos outliers
variables <- cbind(x, y)
boxplot(variables,
        main = "Teste aos Outliers de X e Y")

# Teste à linearidade
plot(x, y,
     main = "Gráfico de dispersão entre o índice de transmissibilidade\ne densidade de população",
     xlab = "Índice de transmissibilidade",
     ylab = "Densidade de população",
     pch = 19,
     col = "black")
abline(reg)

# Teste à normalidade: Método #1
shapiro.test(residuals(reg))

# Teste à normalidade: Método #2
qqnorm(residuals(reg), ylab = "Resíduos", main = "Teste à normalidade dos resíduos")
qqline(residuals(reg))

# Teste às variâncias (condição de homocedasticidade)
plot(fitted(reg), residuals(reg), xlab = "Valores Ajustados", ylab = "Resíduos", main = "Teste à condição de homocedasticidade")
abline(h = 0) # coloca uma linha no valor 0

mx = median(x) # divisão dos dados ao meio
var.test(residuals(reg) [x > mx], residuals(reg) [x < mx])

# Teste de correlação
cor.test(x, y, alternative = "two.sided", method = "pearson")


# Alínea b) --------------------------------------------------
# Variáveis
columns <- c("date","location", "total_deaths_per_million", "aged_65_older")
x <- c()
y <- c()

# Guarda o número total de mortes e maior percentagem de populção com idade > 65 anos
#dos países europeus selecionados
for (country in europeanCountries) {
  countryData <- subset(europeanCountriesData, europeanCountriesData$location == country)
  maxDeaths <- max(countryData$total_deaths_per_million, na.rm = TRUE);
  maxAged <- max(countryData$aged_65_older, na.rm = TRUE);
  
  x <- append(x, maxDeaths)
  y <- append(y, maxAged)
}

reg <- lm(y ~ x)

# Teste aos outliers
variables <- cbind(x, y)
boxplot(variables,
        main = "Teste aos Outliers de X e Y")

# Teste à linearidade
plot(x, y,
     main = "Gráfico de dispersão entre o número de mortos\ne população com idade > 65 anos",
     xlab = "Número de mortos",
     ylab = "População com idade > 65 anos",
     pch = 19,
     col = "black")
abline(reg)

# Teste à normalidade: Método #1
shapiro.test(residuals(reg))

# Teste à normalidade: Método #2
qqnorm(residuals(reg), ylab = "Resíduos", main = "Teste à normalidade dos resíduos")
qqline(residuals(reg))

# Teste às variâncias (condição de homocedasticidade)
plot(fitted(reg), residuals(reg), xlab = "Valores Ajustados", ylab = "Resíduos", main = "Teste à condição de homocedasticidade")
abline(h = 0) # coloca uma linha no valor 0

mx = median(x) # divisão dos dados ao meio
var.test(residuals(reg) [x > mx], residuals(reg) [x < mx])

# Teste de correlação
cor.test(x, y, alternative = "two.sided", method = "pearson")



################################################## ANÁLISE DE REGRESSÃO ##################################################

# Variáveis
conditions <- data$iso_code == countriesCodes["portugal"] &
              data$date > as.Date("2020-04-01") &
              data$date < as.Date("2021-02-27")
columns <- c("new_deaths_per_million", "new_cases_per_million", "reproduction_rate", "stringency_index")

# Obtém os dados de Portugal e remove os NA's
portugal <- subset(data, conditions, columns);
portugal <- na.omit(portugal)

dm <- portugal$new_deaths_per_million
cm <- portugal$new_cases_per_million
rm <- portugal$reproduction_rate
ir <- portugal$stringency_index


# Alínea a) --------------------------------------------------
mod <- lm(ir ~ dm + cm + rm)
summary(mod)


# Alínea b) --------------------------------------------------
x <- cbind(dm, cm, rm)

# Homocedasticidade
qqnorm(residuals(mod))
qqline(residuals(mod))

shapiro.test(residuals(mod)) 

t.test(residuals(mod)) 

plot(fitted(mod), residuals(mod), xlab = "Valores Ajustados", ylab = "Resíduos", main = "Teste à condição de homocedasticidade")
abline(h = 0) # coloca uma linha no valor 0

mx = median(x) # divisão dos dados ao meio
var.test(residuals(mod) [x > mx], residuals(mod) [x < mx]) 

# Autocorrelação Nula
# H0: Os resíduos são independentes
# H1: Os resíduos não são independentes
durbinWatsonTest(mod) 

# Multicolinearidade
vif(mod)


# Alínea c) --------------------------------------------------
values <- data.frame(dm = 10, cm = 460, rm = 1.1)
predict(mod, values, interval = "confidence", level = 0.95)
