# Função para gerar amostras da variável X usando a função inversa
gerar_variavel_X <- function(n, size) {
  u <- runif(size)  # Gerar números aleatórios uniformemente distribuídos
  X <- u^(1/n)  # Aplicar a inversa da função acumulada
  return(X)
}

# Função de densidade de probabilidade para a variável aleatória X
pdf_X <- function(x, n) {
  ifelse(x >= 0 & x <= 1, n * x^(n-1), 0)  # Considerar PDF válida apenas para 0 <= x <= 1
}

# Configuração do ambiente de plotagem
par(mfrow = c(3, 1), mar = c(4, 4, 2, 1))  # Configuração para múltiplos plots e ajuste de margens

# Número de amostras
n_samples <- 10000

# Valores de n para gerar variáveis aleatórias
n_values <- c(1, 2, 3)

# Loop para gerar e plotar amostras e PDF para cada n
for (n in n_values) {
  # Gerar amostras da variável X
  X <- gerar_variavel_X(n, n_samples)
  
  # Histograma das amostras geradas
  hist(X, freq = FALSE, breaks = 40, main = paste("Histograma para n =", n), xlab = "X", ylim = c(0, 2 * n), col = "lightblue")
  
  # Adicionar a função de densidade de probabilidade ao gráfico
  curve(pdf_X(x, n), from = 0, to = 1, add = TRUE, col = "red", lwd = 2)
}

# Resetar layout de plotagem para padrão
par(mfrow = c(1, 1), mar = c(5, 4, 4, 2))
