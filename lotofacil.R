
chance_max = function(jogo){
  x = list()
  for (i in 1:100000){
    x[[i]] = sample(1:25, 15, replace=FALSE)
  }
  l = lapply(x, sum) |> unlist()
  probs = dnorm(sum(jogo), mean(l), sd(l))
  return(paste0(round(probs, 4)*100, '%'))
}

primos = c(2, 3, 5, 7, 11, 13, 17, 19, 23)

n_primos = setdiff(1:25, primos)

sequence = c(primos, sample(n_primos, 6)) |> sort()

chance_max(jogo=sequence)
