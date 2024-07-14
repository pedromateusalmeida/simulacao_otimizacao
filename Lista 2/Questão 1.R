# Exercício 1
# Enunciado: Implemente o método da função inversa para gerar uma variável aleatória contínua com a seguinte densidade:
# f(x) = x/4, se 0 < x <= 2; 1 - x/4, se 2 < x < 4; 0, caso contrário.
# Faça uma estimativa numérica por simulação de sua média, variância, assimetria e curtose, e compare com os valores teóricos exatos.

library(e1071)  # Carregar a biblioteca para calcular skewness e kurtosis

# Função Inversa da CDF
inverse_cdf <- function(u) {
  ifelse(u <= 0.5, 2 * sqrt(u), 4 - 2 * sqrt(1 - u))
}

# Simulação de 100000 amostras
set.seed(123)  # Fixa a semente para reprodutibilidade
n <- 100000  # Número de amostras
samples <- sapply(runif(n), inverse_cdf)  # Gera amostras usando a função inversa

# Cálculos de estatísticas básicas
mean_sample <- mean(samples)  # Média das amostras
variance_sample <- var(samples)  # Variância das amostras
skewness_sample <- skewness(samples)  # Assimetria das amostras
kurtosis_sample <- kurtosis(samples) - 3  # Curtose das amostras (ajustada para excedente)

# Imprimir os resultados estimados
cat("Média Estimada: ", mean_sample, "\n")
cat("Variância Estimada: ", variance_sample, "\n")
cat("Assimetria Estimada: ", skewness_sample, "\n")
cat("Curtose Estimada: ", kurtosis_sample, "\n")

# Integração para a média teórica
mean_theoretical <- integrate(function(x) x * (x/4), 0, 2)$value + integrate(function(x) x * ((4-x)/4), 2, 4)$value

# Integração para E[X^2] (esperança do quadrado de X)
mean_square_theoretical <- integrate(function(x) x^2 * (x/4), 0, 2)$value + integrate(function(x) x^2 * ((4-x)/4), 2, 4)$value

# Cálculo da variância teórica
variance_theoretical <- mean_square_theoretical - mean_theoretical^2

# Imprimir os valores teóricos corrigidos
cat("Média Teórica: ", mean_theoretical, "\n")
cat("Variância Teórica: ", variance_theoretical, "\n")
