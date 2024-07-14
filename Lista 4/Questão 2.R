# Carregar pacotes necessários
if (!require(moments)) install.packages("moments", dependencies=TRUE)
library(moments)

# Função para calcular a distribuição binomial 
binomial_dist_proposta <- function(n, p) {
  result <- sum(runif(n) < p)
  return(result)
}

# Algoritmo Metropolis-Hastings para gerar amostras da distribuição binomial
metropolis_hastings_binom <- function(n, p, N) {
  samples <- numeric(N)
  cur_sample <- binomial_dist_proposta(n, p)
  
  for (i in 1:N) {
    prop_sample <- binomial_dist_proposta(n, p)
    
    accept_prob_num <- choose(n, prop_sample) * p^prop_sample * (1-p)^(n-prop_sample)
    accept_prob_denom <- choose(n, cur_sample) * p^cur_sample * (1-p)^(n-cur_sample)
    acceptance_prob <- min(1, accept_prob_num / accept_prob_denom)
    
    if (runif(1) < acceptance_prob) {
      cur_sample <- prop_sample
    }
    
    samples[i] <- cur_sample
  }
  
  return(samples)
}

# Parâmetros
n <- 20    # Número de tentativas
p <- 0.5   # Probabilidade de sucesso
N <- 1000  # Número de amostras

# Gerar amostras
binonTeste <- metropolis_hastings_binom(n, p, N)

# Plotar histograma
hist(binonTeste, breaks = 50, main = "Histograma das Amostras da Distribuição Binomial", xlab = "Valor", ylab = "Frequência")

# Calcular estatísticas descritivas
exe2 <- data.frame(Parametro=c("Média","Desvio Padrão","Assimetria","Curtose"),
                   Valor=c(mean(binonTeste),sd(binonTeste),skewness(binonTeste),kurtosis(binonTeste)))

print(exe2)
