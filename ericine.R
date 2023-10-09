
frascos_de_soro <- c(
  480, 485, 485, 490, 490, 
  495, 495, 497, 498, 498, 
  500, 500, 503, 505, 506, 
  508, 510, 510, 515, 519
)

amplitute = max(frascos_de_soro) - min(frascos_de_soro) # a amplitude

classes = 5 # o número de classes

intervalo = 8 # tecnicamente, isto é igual a amplitude / classes

l1 = min(frascos_de_soro) + 8 # o limite inferior + o intervalo = limite superior
l2 = l1 + 9 # o limite superior = limite inferior + 1 + intervalo
l3 = l2 + 9 
l4 = l3 + 9 
l5 = l4 + 9 

tabela = data.frame(
  classe = c('480 a 488', '489 a 497', '498 a 506', '507 a 515', '516 a 524'),
  frequencia = c(3, 5, 7, 4, 1)
)

hist(frascos_de_soro, breaks = 5)

ggplot2::ggplot(tabela)+
  ggplot2::geom_col(ggplot2::aes(x = classe, y = frequencia), fill = 'dodgerblue3')+
  ggplot2::labs(
    title = 'Distribuição de Frequência dos Frascos de Soro (em mililitros)',
    x = 'Classes', y = 'Frequência')+
  ggplot2::theme_minimal()
