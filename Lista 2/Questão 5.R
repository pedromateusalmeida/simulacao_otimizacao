#Exercicio 5
#Enunciado - Simule a variável aleatória discreta hipergeométrica.
# Configurações iniciais
set.seed(123)  # Para reprodução dos resultados
N <- 100  # Tamanho total da população
M <- 30   # Número total de sucessos na população
n <- 10   # Tamanho da amostra
nsim <- 10000  # Número de simulações

# Realizar múltiplas simulações
resultados <- rhyper(nsim, M, N - M, n)

# Visualizar os resultados através de um histograma
hist(resultados, breaks = 30, main = "Histograma de Resultados Hipergeométricos", xlab = "Número de Sucessos", col = "lightblue")
