# Função para gerar variáveis aleatórias Weibull usando o método da função inversa
weibull_inverse <- function(u, a, b) {
  (-log(1 - u) / a)^(1 / b)
}

# Parâmetros da distribuição Weibull
a <- 1  # Coeficiente de escala
b <- 1.5  # Coeficiente de forma

# Gerando amostras
set.seed(123)  # Para reprodutibilidade
n_samples <- 10000  # Número de amostras
uniform_samples <- runif(n_samples)  # Gerando amostras uniformes
weibull_samples <- sapply(uniform_samples, weibull_inverse, a = a, b = b)

# Cálculos de estatísticas básicas
mean_weibull <- mean(weibull_samples)
sd_weibull <- sd(weibull_samples)

# Para calcular assimetria e curtose, utilizamos o pacote 'e1071'
library(e1071)
skewness_weibull <- skewness(weibull_samples)
kurtosis_weibull <- kurtosis(weibull_samples) - 3  # Ajuste para fazer excedente de kurtose

# Imprimir estatísticas
cat("Média: ", mean_weibull, "\n")
cat("Desvio Padrão: ", sd_weibull, "\n")
cat("Assimetria: ", skewness_weibull, "\n")
cat("Curtose: ", kurtosis_weibull, "\n")

# Histograma das amostras geradas
hist(weibull_samples, breaks = 50, main = "Histograma de Amostras Weibull", xlab = "Valores", col = "blue")
