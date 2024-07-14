# Parâmetros
lambda <- 1.5
n <- 1000
censoring_point <- 1  # Valor de censura

# Gerar dados exponenciais
set.seed(123)
u <- runif(n)
z <- -(log(1 - u)) / lambda

# Aplicar censura
z_censored <- pmin(z, censoring_point)
censored <- z > censoring_point

# Plotar histograma dos dados censurados
hist(z_censored, breaks = 50, main = "Dados Exponenciais Censurados", xlab = "Valor", ylab = "Frequência")


# Função de Verossimilhança Esperada (E-step)
e_step <- function(data, lambda, censoring_point, censored) {
  expected_values <- ifelse(censored, censoring_point + (1 / lambda), data)
  return(expected_values)
}

# Função de Maximização (M-step)
m_step <- function(data) {
  new_lambda <- 1 / mean(data)
  return(new_lambda)
}

# Algoritmo EM
em_algorithm <- function(data, censoring_point, censored, tol = 1e-6, max_iter = 1000) {
  lambda <- 1 / mean(data)  # Inicialização
  for (i in 1:max_iter) {
    # E-step
    expected_data <- e_step(data, lambda, censoring_point, censored)
    
    # M-step
    new_lambda <- m_step(expected_data)
    
    # Convergência
    if (abs(new_lambda - lambda) < tol) {
      break
    }
    
    lambda <- new_lambda
  }
  return(lambda)
}

# Aplicar o algoritmo EM
lambda_est <- em_algorithm(z_censored, censoring_point, censored)

# Exibir resultado
cat("Estimativa de lambda pelo algoritmo EM:", lambda_est, "\n")
