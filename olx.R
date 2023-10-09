
require(tidyverse)
require(rvest)
require(magrittr)
require(hereR)

set_key("dDhf8dzZH6Ctk4tvWiKJqf0ojPjs4xRFNQJtq0dUidQ")

df <- list()

pb <- txtProgressBar(0, 100, style = 3)

for (i in 1:100) {
  
  olx <- read_html(str_c("https://ma.olx.com.br/regiao-de-sao-luis/imoveis?o=", i))
  
  boxes <- olx %>% html_nodes(".gIEtsI") %>% html_text()
  
  prices <- parse_number(
    str_remove_all(
      str_sub(
        boxes, 
        start = str_locate(boxes, fixed("R$"))[,1]+3, 
        end = str_locate(boxes, fixed("R$"))[,2]+8
      ), "[[:punct:]]+"
    )
  )
  
  address <- str_to_title(
    str_sub(
      boxes, 
      start = str_locate(boxes, fixed("São Luís, "))[,1]+10, 
      end = str_locate(boxes, fixed("Profissional"))[,1]-1
    )
  )
  
  types <- ifelse(
    str_detect(boxes, "Venda|venda") == T, 
    "Venda",
    ifelse(
      str_detect(boxes, "Aluguel|aluguel") == T,
      "Aluguel", 
      "Outro")
    )
    
  df[[i]] <- tibble(address, prices, types)
  
  setTxtProgressBar(pb, i)
  
}

tabela <- data.table::rbindlist(df)

tabela %<>% 
  filter(types == "Venda", 
         prices > 0, 
         str_length(address) < 30) %>% 
  remove_missing() %>% 
  group_by(address) %>% 
  summarise(builds = n(),
            price = mean(prices, na.rm = T),
            sd_price = sd(prices, na.rm = T))

geoaddress <- geocode(
  address = paste(
    tabela$address, "São Luís", "Maranhão", "Brasil", 
    sep = ", ")
  )

tabela$id <- 1:length(geoaddress)

tabela %<>% 
  left_join(geoaddress, by = "id")

tabela$lng <- st_coordinates(tabela$geometry)[,1]
tabela$lat <- st_coordinates(tabela$geometry)[,2]

require(sf)
require(leaflet)

tabela %>% 
  filter(sd_price > 0) %>% 
  leaflet() %>% 
  addProviderTiles(providers$CartoDB.Positron) %>% 
  addCircleMarkers(
    lng = ~lng,
    lat = ~lat, 
    label = ~paste("R$", format(round(price, 2), 
                                big.mark = ".", 
                                decimal.mark = ","), 
                   "-", address.x),
    popup = ~paste("R$", format(round(sd_price, 2), 
                                big.mark = ".", 
                                decimal.mark = ",")),
    radius = ~sqrt(price/1000)+5, 
    stroke = F, 
    fillOpacity = 1,
    fillColor = ~colorQuantile("YlOrRd", price)(price)
  ) %>% 
  addMeasure(
    position = "bottomleft", 
    primaryLengthUnit = "kilometers", 
    primaryAreaUnit = "hectares", 
    activeColor = "dodgerblue4", 
    completedColor = "dodgerblue"
  )