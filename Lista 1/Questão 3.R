set.seed(123)  # Para reprodutibilidade

# Função para simular a retirada das bolas e encontrar a última bola azul
simulate_last_blue_position <- function() {
  # Criando o vetor de bolas
  balls <- c(rep("blue", 3), rep("white", 4), rep("gray", 5))
  
  # Embaralhando o vetor de bolas
  shuffled_balls <- sample(balls)
  
  # Encontrando a posição da última bola azul
  last_blue_position <- max(which(shuffled_balls == "blue"))
  
  return(last_blue_position)
}

# Realizando a simulação múltiplas vezes
num_simulations <- 10000
results <- replicate(num_simulations, simulate_last_blue_position())

# Contando a frequência de cada posição para a última bola azul
positions_count <- table(results)
positions_count

# Dados da simulação
positions <- c(3:12)
frequencies <- c(45, 156, 263, 458, 711, 964, 1224, 1662, 2063, 2454)

# Criando um gráfico de barras
barplot(frequencies, names.arg = positions, col = "blue",
        main = "Frequência da Última Bola Azul por Posição",
        xlab = "Posição da Última Bola Azul",
        ylab = "Frequência",
        ylim = c(0, max(frequencies) + 100))
