# Parâmetros para a simulação
nRet <- 20  # Número de tentativas em cada experimento
probs <- c(0.05, 0.10, 0.15, 0.20, 0.20, 0.30)  # Probabilidades para cada uma das 6 categorias
nsim <- 10000  # Número de simulações

# Gerar amostras multinomiais
multinomial_samples <- rmultinom(nsim, nRet, prob = probs)

# Configuração para plotagem de múltiplos histogramas
par(mfrow = c(2, 3), mar = c(4, 4, 2, 1))

# Gerar histogramas para cada categoria
for (i in 1:6) {
  hist(multinomial_samples[i, ], breaks = 20, main = paste("Histograma para categoria", i),
       xlab = "Número de ocorrências", col = "lightblue")
}

# Resetar layout de plotagem para padrão
par(mfrow = c(1, 1), mar = c(5, 4, 4, 2))
