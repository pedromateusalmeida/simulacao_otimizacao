# Definir a função f(x) e sua derivada f'(x)
fx <- function(x) {
  x^3 - 2*x^2 - x + 2
}

flx <- function(x) {
  3*x^2 - 4*x - 1
}

# Definir os parâmetros
raizes <- list()
qtdRaizes <- 3
v0 <- 1.5
itMax <- 1000
controle <- 0
x <- 0
x0 <- v0

# Implementação do Método de Newton
while (TRUE) {
  for (i in 1:itMax) {
    x <- x0 - (fx(x0) / flx(x0))
    x0 <- x
  }
  
  if (x0 != 1 && x0 != -1 && x0 != 2) {
    cat('não convergiu \n')
    cat(x0)
  }
  
  if (length(raizes) > 0) {
    for (k in 1:length(raizes)) {
      if (round(raizes[[k]], 8) == round(x0, 8)) {
        controle <- controle + 1
      }
    }
  }
  
  if (controle == 0) {
    raizes <- append(raizes, x0)
  }
  
  if (length(raizes) == qtdRaizes) {
    break
  }
  
  x0 <- runif(1, min = -5, max = 5)
  controle <- 0
}

format(raizes, digits = 20)
