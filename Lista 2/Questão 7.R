# Implementação do método polar para gerar uma variável aleatória normal N(m, s^2)
gerar_normal_polar <- function(n, mean, sd) {
  resultados <- numeric(n)
  i <- 1
  while (i <= n) {
    u1 <- runif(1)
    u2 <- runif(1)
    v1 <- 2 * u1 - 1
    v2 <- 2 * u2 - 1
    w <- v1^2 + v2^2
    if (w <= 1) {
      z1 <- v1 * sqrt((-2 * log(w)) / w)
      z2 <- v2 * sqrt((-2 * log(w)) / w)
      resultados[i] <- mean + sd * z1
      i <- i + 1
      if (i <= n) {
        resultados[i] <- mean + sd * z2
        i <- i + 1
      }
    }
  }
  return(resultados)
}

# Parâmetros
n <- 10000  # Número de simulações
media_verdadeira <- 10  # Média verdadeira da distribuição normal
desvio_verdadeiro <- 2  # Desvio padrão verdadeiro da distribuição normal

# Gerar variáveis aleatórias normais usando o método polar
set.seed(123)  # Definindo a semente para reprodução dos resultados
amostra <- gerar_normal_polar(n, media_verdadeira, desvio_verdadeiro)

# Estimativa numérica dos momentos
media_estimada <- mean(amostra)
variancia_estimada <- var(amostra)
library(e1071)  # Carregar pacote para skewness e kurtosis
assimetria_estimada <- skewness(amostra)
curtose_estimada <- kurtosis(amostra)

# Output dos resultados
cat("Estimativa Numérica da Média:", media_estimada, "\n")
cat("Estimativa Numérica da Variância:", variancia_estimada, "\n")
cat("Estimativa Numérica da Assimetria:", assimetria_estimada, "\n")
cat("Estimativa Numérica da Curtose:", curtose_estimada - 3, "\n")  # Curtose excessiva ajustada
