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
countriesColors <- c(albania = "", denmark = "", germany = "", russia = "",
                     portugal = "red1", spain = "gold", italy = "chartreuse3", uk = "dodgerblue1",
                     france = "")



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
maxToalCases <- europeanCountries[row, "new_cases_per_million"]$new_cases_per_million
country <- europeanCountries[row, "location"]$location
date <- europeanCountries[row, "date"]$date
cat("País europeu que teve o maior número de infetados, por milhão de habitantes, num só dia: ",
    "\n\t", country, "\t", "Dia: ", format(date, "%d %b %Y"),
    "\t Casos: ", maxToalCases)


# Alínea f) --------------------------------------------------
row <- which.max(data$reproduction_rate)
maxTransmissibilityRate <- data[row, "reproduction_rate"]$reproduction_rate
date <- data[row, "date"]$date
country <- data[row, "location"]$location
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

# Alínea a) --------------------------------------------------



# Alínea b) --------------------------------------------------



# Alínea c) --------------------------------------------------




################################################## ANÁLISE DE CORRELAÇÃO ##################################################

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

# Gráfico de dispersão
plot(x, y,
     main = "Gráfico de dispersão entre o índice de transmissibilidade\ne densidade de população",
     xlab = "Índice de transmissibilidade",
     ylab = "Densidade de população",
     pch = 19,
     col = "black")

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

# Gráfico de dispersão
plot(x, y,
     main = "Gráfico de dispersão entre o número total de mortes, por milhão de habitantes,\ne percentagem de população com mais de 65 anos",
     xlab = "Número total de mortes / milhão de habitantes",
     ylab = "% de população > 65 anos",
     pch = 19,
     col = "black")

# Teste de correlação
cor.test(x, y, alternative = "two.sided", method = "pearson")



################################################## ANÁLISE DE REGRESSÃO ##################################################

# Variáveis
columns <- c("new_deaths_per_million", "new_cases_per_million", "reproduction_rate", "stringency_index")
portugal <- subset(data,
                   data$iso_code == "PRT" & data$date > 2020-04-01 & data$date < 2021-02-27,
                   columns);

dm <- portugal$new_deaths_per_million
cm <- portugal$new_cases_per_million
rm <- portugal$reproduction_rate
ir <- portugal$stringency_index


# Alínea a) --------------------------------------------------



# Alínea b) --------------------------------------------------



# Alínea c) --------------------------------------------------
