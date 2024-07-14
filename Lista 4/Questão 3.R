# Carregar pacotes necessários
if (!require(moments)) install.packages("moments", dependencies=TRUE)
library(moments)

# Parâmetros verdadeiros
trueA <- 5
trueB <- 0
trueSd <- 10
sampleSize <- 31

# Criar valores independentes x
x <- (-(sampleSize-1)/2):((sampleSize-1)/2)
# Criar valores dependentes de acordo com ax + b + N(0,sd)
y <- trueA * x + trueB + rnorm(n = sampleSize, mean = 0, sd = trueSd)

# Plotar os dados
plot(x, y, main = "Dados de Teste")

# Função de verossimilhança
likelihood <- function(param) {
  a <- param[1]
  b <- param[2]
  sd <- param[3]
  
  pred <- a * x + b
  singlelikelihoods <- dnorm(y, mean = pred, sd = sd, log = TRUE)
  sumll <- sum(singlelikelihoods)
  return(sumll)
}

# Plotar o perfil de verossimilhança do parâmetro de inclinação a
slopevalues <- function(x) {
  return(likelihood(c(x, trueB, trueSd)))
}

slopelikelihoods <- sapply(seq(3, 7, by = 0.05), slopevalues)
plot(seq(3, 7, by = 0.05), slopelikelihoods, type = "l", 
     xlab = "Valores do parâmetro de inclinação a", ylab = "Log-verossimilhança")

# Função de distribuição a priori
prior <- function(param) {
  a <- param[1]
  b <- param[2]
  sd <- param[3]
  aprior <- dunif(a, min = 0, max = 10, log = TRUE)
  bprior <- dnorm(b, sd = 10, log = TRUE)
  sdprior <- dunif(sd, min = 0, max = 30, log = TRUE)
  return(aprior + bprior + sdprior)
}

# Função de posteriori
posterior <- function(param) {
  return(likelihood(param) + prior(param))
}

# Função de proposta
proposalfunction <- function(param) {
  return(rnorm(3, mean = param, sd = c(0.1, 0.5, 0.3)))
}

# Algoritmo Metropolis-Hastings
run_metropolis_MCMC <- function(startvalue, iterations) {
  chain <- matrix(NA, nrow = iterations + 1, ncol = 3)
  chain[1, ] <- startvalue
  for (i in 1:iterations) {
    proposal <- proposalfunction(chain[i, ])
    
    probab <- exp(posterior(proposal) - posterior(chain[i, ]))
    if (runif(1) < probab) {
      chain[i + 1, ] <- proposal
    } else {
      chain[i + 1, ] <- chain[i, ]
    }
  }
  return(chain)
}

# Parâmetros iniciais
startvalue <- c(4, 0, 10)
iterations <- 50000
burnIn <- 5000

# Executar o algoritmo
chain <- run_metropolis_MCMC(startvalue, iterations)
acceptance <- 1 - mean(duplicated(chain[-(1:burnIn), ]))

# Resumo dos resultados
par(mfrow = c(2, 3))
hist(chain[-(1:burnIn), 1], nclass = 30, main = "Posterior de a", xlab = "Valor Verdadeiro = linha vermelha")
abline(v = mean(chain[-(1:burnIn), 1]))
abline(v = trueA, col = "red")

hist(chain[-(1:burnIn), 2], nclass = 30, main = "Posterior de b", xlab = "Valor Verdadeiro = linha vermelha")
abline(v = mean(chain[-(1:burnIn), 2]))
abline(v = trueB, col = "red")

hist(chain[-(1:burnIn), 3], nclass = 30, main = "Posterior de sd", xlab = "Valor Verdadeiro = linha vermelha")
abline(v = mean(chain[-(1:burnIn), 3]))
abline(v = trueSd, col = "red")

plot(chain[-(1:burnIn), 1], type = "l", main = "Valores da cadeia de a")
abline(h = trueA, col = "red")

plot(chain[-(1:burnIn), 2], type = "l", main = "Valores da cadeia de b")
abline(h = trueB, col = "red")

plot(chain[-(1:burnIn), 3], type = "l", main = "Valores da cadeia de sd")
abline(h = trueSd, col = "red")

# Estatísticas descritivas
exe3a <- data.frame(Parametro = c("Média", "Desvio Padrão", "Assimetria", "Curtose"),
                    Valor = c(mean(chain[-(1:burnIn), 1]), sd(chain[-(1:burnIn), 1]), 
                              skewness(chain[-(1:burnIn), 1]), kurtosis(chain[-(1:burnIn), 1])))

exe3b <- data.frame(Parametro = c("Média", "Desvio Padrão", "Assimetria", "Curtose"),
                    Valor = c(mean(chain[-(1:burnIn), 2]), sd(chain[-(1:burnIn), 2]), 
                              skewness(chain[-(1:burnIn), 2]), kurtosis(chain[-(1:burnIn), 2])))

exe3sd <- data.frame(Parametro = c("Média", "Desvio Padrão", "Assimetria", "Curtose"),
                     Valor = c(mean(chain[-(1:burnIn), 3]), sd(chain[-(1:burnIn), 3]), 
                               skewness(chain[-(1:burnIn), 3]), kurtosis(chain[-(1:burnIn), 3])))

# Comparação com a regressão linear tradicional
modelo_lm <- lm(y ~ x)
summary(modelo_lm)

# Impressão das estatísticas descritivas
print(exe3a)
print(exe3b)
print(exe3sd)
