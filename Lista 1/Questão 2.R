# Função para simular o preenchimento de um álbum de figurinhas
simulate_album_filling <- function(n) {
  album <- rep(0, n)
  count <- 0
  
  while (min(album) == 0) {
    sticker <- sample(1:n, 1)
    album[sticker] <- album[sticker] + 1
    count <- count + 1
  }
  
  return(count)
}

# Função para calcular a média de M(n) para diferentes tamanhos de álbuns
calculate_average_Mn <- function(max_n, num_simulations = 1000) {
  averages <- numeric(max_n)
  for (n in 1:max_n) {
    results <- replicate(num_simulations, simulate_album_filling(n))
    averages[n] <- mean(results)
  }
  return(averages)
}

# Realizando simulações e calculando a média de M(n)
max_n <- 50  # Testar álbuns de tamanhos variando de 1 a 350 figurinhas
average_Mn <- calculate_average_Mn(max_n)

# Imprimindo os resultados
print(average_Mn)


# Função para calcular o n-ésimo número harmônico
harmonic_number <- function(n) {
  return(sum(1/(1:n)))
}

# Calculando o valor teórico de M(n)
theoretical_Mn <- sapply(1:max_n, function(n) n * harmonic_number(n))

# Comparando com os resultados empíricos
comparison <- data.frame(
  n = 1:max_n,
  Empirical = average_Mn,
  Theoretical = theoretical_Mn
)

print(harmonic_number)

print(theoretical_Mn)

print(comparison)

# Carregando pacote para gráficos
library(ggplot2)

# Preparando dados para plotagem
comparison_df <- data.frame(
  n = 1:max_n,
  Empirical = average_Mn,
  Theoretical = theoretical_Mn
)

# Criando gráfico de comparação
ggplot(comparison_df, aes(x = n)) +
  geom_line(aes(y = Empirical, colour = "Empirical"), size = 1) +
  geom_line(aes(y = Theoretical, colour = "Theoretical"), size = 1) +
  labs(title = "Comparison of Empirical and Theoretical Mean M(n)",
       x = "Number of Stickers in Album (n)",
       y = "Average Number of Stickers Bought (M(n))") +
  scale_colour_manual("", 
                      breaks = c("Empirical", "Theoretical"),
                      values = c("blue", "red"))
