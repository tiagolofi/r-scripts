
# install.packages('magrittr', 'readxl', 'dplyr', 'stargazer')

require(magrittr)

dados <- readxl::read_excel('~/Dados/dados1.xlsx')

dplyr::glimpse(dados)

cor.test(dados$horas_dorme, (dados$tempo_aula_online/dados$tempo_tela))

dados %<>%
  dplyr::mutate(
    proporcao_aula_tela_total = tempo_aula_online/tempo_tela*100
  )

model <- lm(horas_dorme ~ proporcao_aula_tela_total,
            data = subset(dados, proporcao_aula_tela_total <= 100))

stargazer::stargazer(model, type='text')

dados %>%
  dplyr::filter(
    proporcao_aula_tela_total <= 100
  ) %>% 
  ggplot2::ggplot()+
  ggplot2::aes(x = proporcao_aula_tela_total, y = log(horas_dorme))+
  ggplot2::geom_point(ggplot2::aes(col=dispositivo_aula, shape=sexo, size=quant_disciplinas))+
  ggplot2::geom_smooth(se=F, method='lm', col='red2')+
  ggplot2::labs(
    title = 'Relação entre tempo destinado a aula online e horas de sono',
    subtitle = 'Alunos que dedicam mais tempo de tela a aula online\nreduzem as horas de sono em 0.01h a cada aumento percentual no tempo de tela.',
    caption = 'Fonte: Pesquisa aplicada via Google Forms com Estudantes da UFMA.',
    x = 'Proporção do Tempo de Tela Total destinado à Aula Online',
    y = 'Horas de sono',
    col = 'Tipo do Dispositivo',
    size = 'Quantidade de Disciplinas'
  )+
  ggplot2::theme_minimal()
