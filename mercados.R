
require(quantmod)
require(jsonlite)
s <- jsonlite::toJSON(x, dataframe = 'rows')

x = data.frame(getSymbols("ITUB4.SA", from = "2022-01-01", to = "2022-12-30", auto.assign = FALSE))

colnames(x) <- c('open', 'high', 'low', 'close', 'volume', 'close_adj')




x$date <- rownames(x) |> as.Date()

write.csv2(x, file = 'teste.csv')

classifier_trend <- function(y){
  
  x = c()
  
  for (i in 1:length(y)) {
    ifelse(
      y[i] < y[i + 1],
      x[i + 1] <- 'Alta', # alta
      x[i + 1] <- 'Baixa' # baixa
    )
  }
  
  x[1] = x[2]
  
  return(x)
  
}

x$trend <- factor(classifier_trend(y = x$close), levels = c('Baixa', 'Alta'))

x$close_l1 <- Lag(x$close, 1) |> as.numeric()

x$close_l7 <- Lag(x$close, 7) |> as.numeric()

x$close_l10<- Lag(x$close, 10) |> as.numeric()

x$close_l15 <- Lag(x$close, 15) |> as.numeric()

x$close_l20 <- Lag(x$close, 20) |> as.numeric()

x$close_l30 <- Lag(x$close, 30) |> as.numeric()

x$close_l60 <- Lag(x$close, 60) |> as.numeric()

x$diff_vol <- c(0, diff(x$volume, 1))/1000 |> as.numeric()

model_base <- glm(trend ~ diff_vol + close_l10, data = x, family = binomial(link = 'logit'))

summary(model_base)

get_result <- function(x1, x2, probs){

  df_test = data.frame(
    diff_vol = x1, 
    close_l10 = x2
  )

  r <- as.numeric(predict(model_base, newdata = df_test, type="response"))

  ifelse(
    r > probs,
    return('Alta'),
    return('Baixa')
  )
}

get_result(
  x1 = -12691.111,
  x2 = 14.88969,
  probs = 0.50
)

x$test <- c()

x$verify <- c()

for (i in 1:length(x$open)) {
  
  x$test[i] <- get_result(x$diff_vol[i], x$close_l10[i], probs = 0.7)
  
  x$verify[i] <- x$trend[i] == x$test[i]
  
}

df <- x |>
  dplyr::group_by(mes = paste0(lubridate::year(rownames(x)), '/', lubridate::month(rownames(x))), trend) |>
  dplyr::summarise(
    qtd = dplyr::n()
  )

ggplot(x) +
  aes(x = date, y = close) +
  geom_line(colour = "#112446") +
  theme_minimal()


data <- readxl::read_excel('dados-treino.xlsx')

data$decisao <- factor(data$decisao, levels = c('compra', 'vende', 'segura', 'espera'))

data$diff_vol <- c(0, diff(data$volume, 1))/1000 |> as.numeric()

data$close_l15 <- Lag(data$close, 15) |> as.numeric()

require(VGAM)

model_base <- VGAM::vglm(decisao ~ diff_vol + close_l15, data = data, family = multinomial)













































