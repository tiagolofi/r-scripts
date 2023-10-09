
# install.packages("tidyverse")

x <- c("A", "B", "C", "D")

y <- 1:4

df <- data.frame("letras" = x, "numeros" = y)

#### Operações com Dplyr ####

require(dplyr)

df |> filter(
  numeros > 3
) # linhas em que a coluna "numeros" é maior do que "3"

df |> filter(
  letras != "A"
) # linhas em que a coluna "letras" é diferente de "A"

df |> mutate(
  numeros = numeros ** 2
) # elevando a coluna "numeros" ao quadrado

df |> select(letras) # selecionando apenas a coluna letras

df |> 
  mutate(
    letras = paste0(letras, '0'),
    numeros = numeros ** 4
  ) |>
  rename(
    "Letras com zero ao lado" = "letras",
    "Números elevados à quarta potência" = "numeros"
  ) # um exemplo de como NÃO nomear variáveis!
