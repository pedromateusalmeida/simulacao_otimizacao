set.seed(123)  # Para consistência nos resultados
n <- 10000  # Número de pontos simulados Normal
z <- numeric(n)  # Vetor para armazenar os resultados

j <- 1
while(j <= n){
  u1 <- runif(1)  # Gerar U1 uniformemente distribuído
  u2 <- runif(1)  # Gerar U2 uniformemente distribuído
  v1 <- 2 * u1 - 1  # V1 de -1 a 1
  v2 <- 2 * u2 - 1  # V2 de -1 a 1
  s <- v1^2 + v2^2  # Soma dos quadrados
  
  # Checar se o ponto está dentro do círculo unitário
  if(s < 1 && s > 0){
    # Transformação Box-Muller
    factor <- sqrt(-2 * log(s) / s)
    z1 <- v1 * factor
    z2 <- v2 * factor
    
    # Salvar os resultados
    z[j] <- z1
    if(j + 1 <= n){
      z[j + 1] <- z2
      j <- j + 2
    } else {
      j <- j + 1
    }
  }
}

# Histograma dos resultados
hist(z, breaks = 50, main = "Histograma de Simulação da Distribuição Normal", xlab = "Valores", col = "lightblue")

# Calcular e imprimir média e desvio padrão
media <- mean(z)
desvio_padrao <- sd(z)
cat("Média Estimada:", media, "\n")
cat("Desvio Padrão Estimado:", desvio_padrao, "\n")
