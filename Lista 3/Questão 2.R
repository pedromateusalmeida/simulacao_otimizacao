# Função de verossimilhança logarítmica
logfAlfa <- function(alfa0, x) {
  valor = 0
  for(i in 1:length(x))
    valor = valor + log(1 + alfa0 * x[i])
  valor - length(x) * log(2)
}

# Primeira derivada da função de verossimilhança logarítmica
fllogAlfa <- function(alfa0, x) {
  valor = 0
  for(i in 1:length(x))
    valor = valor + (x[i] / (1 + alfa0 * x[i]))
  valor
}

# Segunda derivada da função de verossimilhança logarítmica
flllogAlfa <- function(alfa0, x) {
  valor = 0
  for(i in 1:length(x))
    valor = valor + (-(x[i] * x[i]) / ((1 + alfa0 * x[i]) * (1 + alfa0 * x[i])))
  valor
}

# Vetor de dados do exemplo 1 do texto
x = c(0.41040018, 0.91061564, -0.61106896, 0.39736684, 0.37997637, 0.34565436, 0.01906680, -0.28765977, -0.33169289, 0.99989810, -0.35203164, 0.10360470,
      0.30573300, 0.75283842, -0.33736278, -0.91455101, -0.76222116, 0.27150040, -0.01257456, 0.68492778, -0.72343908, 0.45530570, 0.86249107, 0.52578673,
      0.14145264, 0.76645754, -0.65536275, 0.12497668, 0.74971197, 0.53839119)

# Parâmetros iniciais
v0 = 0.7
itMax = 1000

# Implementação do Método de Newton-Raphson
alfa0 = v0
for (i in 1:itMax) { 
  alfa = alfa0 - (fllogAlfa(alfa0, x) / flllogAlfa(alfa0, x))
  alfa0 = alfa
}

cat("Estimativa de alfa pelo método de Newton-Raphson:", format(alfa0, digits = 20), "\n")

# Verificar os valores da função de verossimilhança, primeira e segunda derivadas em um intervalo de alfa
teste2 = runif(200, min = -1, max = 1)
teste2 = sort(teste2)
testelog = sapply(teste2, logfAlfa, x = x)
testeder1 = sapply(teste2, fllogAlfa, x = x)
testeder2 = sapply(teste2, flllogAlfa, x = x)

# Plotar os gráficos
par(mfrow = c(2, 2))
plot(teste2, testelog, type = "l", main = "Log-Verossimilhança", xlab = "alfa", ylab = "logfAlfa")
plot(teste2, testeder1, type = "l", main = "Primeira Derivada", xlab = "alfa", ylab = "fllogAlfa")
plot(teste2, testeder2, type = "l", main = "Segunda Derivada", xlab = "alfa", ylab = "flllogAlfa")
