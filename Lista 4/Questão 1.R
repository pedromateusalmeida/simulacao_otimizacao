# Carregar pacotes necessários
if (!require(moments)) install.packages("moments", dependencies=TRUE)
library(moments)

# Função para rodar o algoritmo Metropolis-Hastings
run_metropolis_hastings <- function(a, n) {
  X <- numeric(n)
  X[1] <- 0
  for (t in 1:(n-1)) {
    x <- X[t]
    y <- x + (2 * runif(1) - 1) * a
    if (runif(1) < exp((x^2 - y^2) / 2)) {
      X[t + 1] <- y
    } else {
      X[t + 1] <- x
    }
  }
  return(X)
}

# Função para amostra menor para o teste Shapiro-Wilk
shapiro_test_sample <- function(X) {
  sample_size <- min(length(X), 5000)
  sample_indices <- sample(length(X), sample_size)
  shapiro.test(X[sample_indices])
}

# Parâmetros
n <- 100000
a_values <- c(0.1, 2, 10)  # Exemplos de diferentes valores de 'a'

# Rodar simulações para diferentes valores de 'a'
results <- list()
for (a in a_values) {
  X <- run_metropolis_hastings(a, n)
  
  # Plotar histograma
  hist(X, breaks = 50, main = paste("Histograma para a =", a), xlab = "X", ylab = "Frequência")
  
  # Calcular estatísticas
  exe1 <- data.frame(
    Parametro = c("Média", "Desvio Padrão", "Assimetria", "Curtose"),
    Valor = c(mean(X), sd(X), skewness(X), kurtosis(X))
  )
  print(paste("Resultados para a =", a))
  print(exe1)
  
  # Testes de normalidade
  shapiro_test <- shapiro_test_sample(X)
  ks_test <- ks.test(X, y = "pnorm")
  
  print(paste("Teste de Shapiro-Wilk para a =", a))
  print(shapiro_test)
  
  print(paste("Teste de Kolmogorov-Smirnov para a =", a))
  print(ks_test)
  
  results[[paste("a =", a)]] <- list(
    hist_data = X,
    summary_stats = exe1,
    shapiro_test = shapiro_test,
    ks_test = ks_test
  )
}

# Resultados armazenados em 'results'
print(results)