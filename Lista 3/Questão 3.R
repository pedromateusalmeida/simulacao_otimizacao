# Nova abordagem

# Parâmetros do problema
comprimento_barra_estoque <- 10
comprimentos_necessarios <- c(3, 5, 9, 1, 2, 1)
controle <- rep(0, length(comprimentos_necessarios))
desperdicio_atual <- comprimento_barra_estoque
quantidade_barras_usadas <- 0
desperdicio_total <- 0
ponteiro <- 1
combinacao_atual <- 0

# Iterar sobre os comprimentos necessários
for(i in 1:length(comprimentos_necessarios)) {
  ponteiro = i
  combinacao_atual = comprimentos_necessarios[i]
  cat('Iniciando com comprimento', combinacao_atual, '\n')
  if(controle[i] == 0){
    while(combinacao_atual < comprimento_barra_estoque && ponteiro <= length(comprimentos_necessarios)) {
      controle[i] = 1
      ponteiro = ponteiro + 1
      if(ponteiro <= length(comprimentos_necessarios)){
        if(combinacao_atual + comprimentos_necessarios[ponteiro] <= comprimento_barra_estoque && controle[ponteiro] == 0){
          combinacao_atual = combinacao_atual + comprimentos_necessarios[ponteiro] 
          cat('Adicionou comprimento', comprimentos_necessarios[ponteiro], 'valor atual', combinacao_atual, '\n')
          if(comprimento_barra_estoque - combinacao_atual < desperdicio_atual){
            desperdicio_atual = comprimento_barra_estoque - combinacao_atual
            controle[ponteiro] = 1
          }
        }
      } else {
        desperdicio_atual = comprimento_barra_estoque - combinacao_atual
      }
    }
  }
  desperdicio_total = desperdicio_atual + desperdicio_total
}

# Imprimir o desperdício total
print(paste('Desperdício total:', desperdicio_total))
print('Controle:', controle)