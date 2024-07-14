# (6) Simule a variável aleatória discreta binomial negativa.
set.seed(123)  # Para reprodutibilidade dos resultados
p = 0.3
r = 3  # Número de sucessos desejados
nsim = 10000
z = rep(0, nsim)

for (j in 1:nsim) {
  nSuss = 0
  x = 0  # Contador de tentativas totais
  
  while (nSuss < r) {
    if (runif(1) <= p) {
      nSuss = nSuss + 1  # Conta um sucesso
    }
    x = x + 1  # Conta uma tentativa
  }
  
  z[j] = x  # Armazena o número total de tentativas até r sucessos
}

# Histograma dos resultados
hist(z, breaks = 50, main = "Histograma de Tentativas até R Sucessos", xlab = "Número de Tentativas", col = "lightblue")

# Cálculo da média de tentativas
mean_tentativas <- mean(z)
print(paste("Média de Tentativas: ", mean_tentativas))
