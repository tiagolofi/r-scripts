
dados = readxl::read_excel('Dados/dados_edne.xlsx')

mapa = geobr::read_municipality(21, 2018)

setdiff(dados$IRM, mapa$name_muni) # o tamanho do problema

dados$IRM <- dados$IRM |>
  stringr::str_remove_all('[:punct:]+') |>
  abjutils::rm_accent() |>
  stringr::str_to_title()

mapa$name_muni <- mapa$name_muni |>
  stringr::str_remove_all('[:punct:]+') |>
  abjutils::rm_accent() |>
  stringr::str_to_title()

dados$IRM <- dados$IRM |> stringr::str_replace_all('Paulo  Ramos', 'Paulo Ramos')
dados$IRM <- dados$IRM |> stringr::str_replace_all('Santa Filomena Do Ma', 'Santa Filomena Do Maranhao')

setdiff(dados$IRM, mapa$name_muni) # problema resolvido

dados_mapa <- mapa |>
  dplyr::left_join(
    dados, by = c('name_muni' = 'IRM')
  )

dados_mapa$variacao = dados$IRM2018 - dados$IRM2012

showtextdb::font_install(showtextdb::google_fonts('Montserrat'))
showtext::showtext_auto()

dados_mapa <- dados_mapa |> 
  dplyr::mutate(
    `Geração de Renda 2012` = factor(
      dplyr::case_when(
        IRM2012 < 1 ~ "Menor que a participação da pop.", 
        IRM2012 >= 1 ~ "Maior que a participação da pop.",
        is.na(IRM2012) ~ "Sem info",
      ),
      levels = c(
        "Sem info",
        "Menor que a participação da pop.",
        "Maior que a participação da pop."
      )
    ),
    `Geração de Renda 2018` = factor(
      dplyr::case_when(
        IRM2018 < 1 ~ "Menor que a participação da pop.", 
        IRM2018 >= 1 ~ "Maior que a participação da pop.",
        is.na(IRM2018) ~ "Sem info",
      ),
      levels = c(
        "Sem info",
        "Menor que a participação da pop.",
        "Maior que a participação da pop."
      )
    )
  )

gg1 = dados_mapa |> 
  ggplot2::ggplot()+
  ggplot2::geom_sf(ggplot2::aes(fill=`Geração de Renda 2012`), col='grey30')+
  ggplot2::scale_fill_manual(
    values=c("coral1", "cyan4")
  )+
  ggplot2::labs(
    fill="Capacidade de Geração de Renda"
  )+
  ggplot2::theme_minimal()+
  ggplot2::theme(
    text = ggplot2::element_text(family='Montserrat', size = 90, face='bold'),
    axis.text = ggplot2::element_blank(),
    legend.spacing.y = ggplot2::unit(0.5, 'cm'),
    legend.spacing.x = ggplot2::unit(0.25, 'cm'),
    panel.border = ggplot2::element_blank(),
    panel.grid.minor = ggplot2::element_blank()
  )

ggplot2::ggsave(gg1, filename='edne_mapa2012.png', dpi=600, height = 10, width = 8, limitsize = FALSE)

gg2 = dados_mapa |> 
  ggplot2::ggplot()+
  ggplot2::geom_sf(ggplot2::aes(fill=`Geração de Renda 2018`), col='grey30')+
  ggplot2::scale_fill_manual(
    values=c("coral1", "cyan4")
  )+
  ggplot2::labs(
    fill="Capacidade de Geração de Renda"
  )+
  ggplot2::theme_minimal()+
  ggplot2::theme(
    text = ggplot2::element_text(family='Montserrat', size = 90, face='bold'),
    axis.text = ggplot2::element_blank(),
    legend.spacing.y = ggplot2::unit(0.5, 'cm'),
    legend.spacing.x = ggplot2::unit(0.25, 'cm'),
    panel.border = ggplot2::element_blank(),
    panel.grid.minor = ggplot2::element_blank()
  )

ggplot2::ggsave(gg2, filename='edne_mapa2018.png', dpi=600, height = 10, width = 8, limitsize = FALSE)

########################################################################

quantile(dados_mapa$IRM2012, seq(0, 1, 0.2), na.rm = T)
quantile(dados_mapa$IRM2018, seq(0, 1, 0.2), na.rm = T)

dados_mapa <- dados_mapa |> 
  dplyr::mutate(
    `INT Geração de Renda 2012` = factor(
      dplyr::case_when(
        dplyr::between(IRM2012, 0.0004, 0.114899999) ~ '0.0004 a 0.1148', 
        dplyr::between(IRM2012, 0.1149, 0.205699999) ~ '0.1148 a 0.2056', 
        dplyr::between(IRM2012, 0.2057, 0.270999999) ~ '0.2057 a 0.2709', 
        dplyr::between(IRM2012, 0.2710, 0.365099999) ~ '0.2710 a 0.3650', 
        dplyr::between(IRM2012, 0.3651, 4.175999999) ~ '0.3651 a 4.1759', 
        is.na(IRM2012) ~ "Sem info"
      ),
      levels = c(
        "Sem info",
        '0.0004 a 0.1148',
        '0.1148 a 0.2056',
        '0.2057 a 0.2709',
        '0.2710 a 0.3650',
        '0.3651 a 4.1759'
      )
    ),
    `INT Geração de Renda 2018` = factor(
      dplyr::case_when(
        dplyr::between(IRM2018, 0.0723, 0.230699999) ~ '0.0723 a 0.2306', 
        dplyr::between(IRM2018, 0.2307, 0.289899999) ~ '0.2307 a 0.2898', 
        dplyr::between(IRM2018, 0.2899, 0.355899999) ~ '0.2899 a 0.3558', 
        dplyr::between(IRM2018, 0.3559, 0.456299999) ~ '0.3559 a 0.4562', 
        dplyr::between(IRM2018, 0.4563, 3.8227) ~ '0.4563 a 3.8226', 
        is.na(IRM2018) ~ "Sem info"
      ),
      levels = c(
        "Sem info",
        '0.0723 a 0.2306',
        '0.2307 a 0.2898',
        '0.2899 a 0.3558',
        '0.3559 a 0.4562',
        '0.4563 a 3.8226'
      )
    )
  )

gg3 = dados_mapa |> 
  ggplot2::ggplot()+
  ggplot2::geom_sf(ggplot2::aes(fill=`INT Geração de Renda 2012`), col='grey30')+
  ggplot2::scale_fill_brewer(
    palette = 'Blues', direction = 1
  )+
  ggplot2::labs(
    fill = 'Capacidade de Geração de Renda'
  )+
  ggplot2::theme_minimal()+
  ggplot2::theme(
    text = ggplot2::element_text(family='Montserrat', size = 90, face='bold'),
    axis.text = ggplot2::element_blank(),
    legend.spacing.y = ggplot2::unit(0.5, 'cm'),
    legend.spacing.x = ggplot2::unit(0.25, 'cm'),
    panel.border = ggplot2::element_blank(),
    panel.grid.minor = ggplot2::element_blank()
  )

ggplot2::ggsave(gg3, filename='edne_mapa2012cont.png', dpi=600, height = 10, width = 8, limitsize = FALSE)

gg4 = dados_mapa |> 
  ggplot2::ggplot()+
  ggplot2::geom_sf(ggplot2::aes(fill=`INT Geração de Renda 2018`), col='grey30')+
  ggplot2::scale_fill_brewer(
    palette = 'Blues', direction = 1
  )+
  ggplot2::labs(
    fill = 'Capacidade de Geração de Renda'
  )+
  ggplot2::theme_minimal()+
  ggplot2::theme(
    text = ggplot2::element_text(family='Montserrat', size = 90, face='bold'),
    axis.text = ggplot2::element_blank(),
    legend.spacing.y = ggplot2::unit(0.5, 'cm'),
    legend.spacing.x = ggplot2::unit(0.25, 'cm'),
    panel.border = ggplot2::element_blank(),
    panel.grid.minor = ggplot2::element_blank()
  )

ggplot2::ggsave(gg4, filename='edne_mapa2018cont.png', dpi=600, height = 10, width = 8, limitsize = FALSE)
