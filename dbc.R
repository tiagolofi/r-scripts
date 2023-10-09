
if("read.dbc" %in% installed.packages() == FALSE){
  install.packages("read.dbc")
}

require(read.dbc)
require(geobr)
require(sf)
require(tidyverse)

# importação de dados

read_all_dbc <- function(path){
  
  l <- list()
  
  for (i in list.files(path)) {
    l[[i]] = read.dbc(
      paste0(path, i)
    )
    
    print(i)
    
  }
  
  return(
    do.call('rbind', l)
  )
  
}

data <- read_all_dbc(path = "dbc/")

row.names(data) <- 1:nrow(data)

# tratamento de dados

testa = function(x, y){
  return(
    ifelse(x == y, "Sim", "Não")
  )
}

data$DESLOC <- mapply(testa, data$SP_M_HOSP, data$SP_M_PAC)

df2 <- data |>
  filter(strtrim(SP_M_PAC, 2) == "21") |>
  group_by(
    SP_M_HOSP, SP_M_PAC, DESLOC
  ) |>
  summarise(
    ATD = n()
  ) |>
  pivot_wider(
    names_from = DESLOC, values_from = ATD
  )

df3 <- df2 |> 
  select(-Sim) |>
  mutate_all(~replace_na(., 0))

# mapas

ma <- read_municipality(code = 21)

ma$lat_lon = st_centroid(ma$geom)

ext_lon <- function(x){
  return(x[[1]])
}

ext_lat <- function(x){
  return(x[[2]])
}

ma$lon <- sapply(ma$lat_lon, ext_lon)
ma$lat <- sapply(ma$lat_lon, ext_lat)

ma$code_muni6 <- sapply(ma$code_muni, function(x){return(strtrim(x, 6))})

# juntando tudo

df4 <- df3 |>
  left_join(
    select(data.frame(ma), code_muni6, lon, lat, name_muni),
    by = c("SP_M_HOSP" = "code_muni6")
  ) |>
  left_join(
    select(data.frame(ma), code_muni6, lon, lat, name_muni),
    by = c("SP_M_PAC" = "code_muni6")
  )
  
st_agr(ma) <- "constant"

area_viana <- ma |>
  st_crop(c(xmin = -4, xmax = -2, ymin = -46, ymax = -44))

df4 |> 
  filter(name_muni %in% c("Viana")) |>
  ggplot()+
  geom_sf(fill = "grey90", color = "white", data = ma)+
  geom_label(aes(x = lon.x, y = lat.x, label = name_muni.y))+
  geom_segment(aes(x = lon.x, y = lat.x, xend = lon.y, yend = lat.y, color = `Não`))+
  scale_color_distiller(palette = "Blues", direction = 1)+
  theme_minimal()

