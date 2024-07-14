set.seed(123)  # Para reprodutibilidade

# Função para simular o problema das entrevistas
simulate_secretary_problem <- function(n, k) {
  candidates <- sample(1:n)  # Embaralha os candidatos
  best_so_far <- max(candidates[1:k])  # Melhor dos primeiros k descartados
  
  for (i in (k+1):n) {
    if (candidates[i] > best_so_far) {
      return(candidates[i] == n)  # Retorna TRUE se escolher o melhor, FALSE caso contrário
    }
  }
  return(FALSE)  # Se nunca escolher alguém melhor que os k primeiros
}

# Testando diferentes valores de n e k
n_values <- c(10, 20, 30, 40)
results <- data.frame()

for (n in n_values) {
  k_values <- 1:(n-1)
  probabilities <- sapply(k_values, function(k) mean(replicate(1000, simulate_secretary_problem(n, k))))
  optimal_k <- k_values[which.max(probabilities)]
  results <- rbind(results, data.frame(n = n, Optimal_k = optimal_k, Max_Probability = max(probabilities)))
}

print(results)
