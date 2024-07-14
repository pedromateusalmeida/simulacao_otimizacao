# Gerando a variável aleatória X usando um número aleatório
set.seed(123) # Definindo a semente para reprodução dos resultados
n_aleatorio <- runif(1) # Gerando um número aleatório entre 0 e 1

# Supondo que a variável aleatória X siga uma distribuição normal para fins de exemplo
n <- c(1, 2, 3) # Valores de n

# Função de densidade de probabilidade da distribuição normal
pdf_normal <- function(x) dnorm(x, mean = 0, sd = 1)

# Gerando dados para os histogramas
dados <- list()
for (i in 1:length(n)) {
  dados[[i]] <- rnorm(10000, mean = 0, sd = 1) ^ n[i]
}

# Plotando histogramas e função de densidade de probabilidade
par(mfrow = c(1, length(n)))
for (i in 1:length(n)) {
  hist(dados[[i]], freq = FALSE, main = paste("n =", n[i]), xlab = "X", ylab = "Densidade")
  curve(pdf_normal, col = "blue", add = TRUE)
}


# Questão 4
# Simulando a variável aleatória discreta multinomial
set.seed(123) # Definindo a semente para reprodução dos resultados
n <- 100 # Número de observações
prob <- c(0.2, 0.3, 0.5) # Probabilidades para cada categoria
resultado <- rmultinom(1, n, prob)
resultado

# Questão 5
# Simulando a variável aleatória discreta hipergeométrica
set.seed(123) # Definindo a semente para reprodução dos resultados
N <- 100 # Tamanho da população
M <- 30 # Número de sucessos na população
n <- 10 # Tamanho da amostra
resultado <- rhyper(1, M, N - M, n)
resultado

# Questão 6
# Simulando a variável aleatória discreta binomial negativa
set.seed(123) # Definindo a semente para reprodução dos resultados
size <- 5 # Número de falhas até o experimento ser interrompido
prob <- 0.3 # Probabilidade de sucesso em cada tentativa
resultado <- rnbinom(1, size, prob)
resultado