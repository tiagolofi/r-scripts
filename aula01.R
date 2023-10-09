
require(readxl)

#### carregando a base de dados ####

df <- read_excel('base_exemplo01.xlsx')

# df_csv <- read.csv('csv_exemplo.csv') # ","
# df_csv <- read.csv2('csv_exemplo.csv') # ";"

require(dplyr)
require(ggplot2)

#### detecção de outliers utilizando boxplot ####

df |>
  mutate(Outlier = ifelse(Mort_Infantil > 35, Municipio, NA)) |> # definindo a região de corte
  ggplot() +
  aes(y = Mort_Infantil) + # estética
  geom_boxplot(fill = 'dodgerblue', color = 'blue2') + # fazendo a arte
  labs(title = 'Detectando Outliers - Taxa de Mortalidade Infantil') + # titulos
  geom_text(aes(label = Outlier, x = 0.1), size = 3, na.rm = TRUE) # legendas

df |>
  mutate(Outlier = ifelse(Taxa_Analfabetismo < 10, Municipio, NA)) |> 
  ggplot() +
  aes(y = Taxa_Analfabetismo) + 
  geom_boxplot(fill = 'gold3', color = 'blue2') + 
  labs(title = 'Detectando Outliers - Taxa de Analfabetismo') +
  geom_text(aes(label = Outlier, x = 0.1), size = 3, na.rm = TRUE)

#### criando grupos de análise ####

df <- df |> 
  mutate( 
    Grupos = case_when( # aplicando regras
      Taxa_Analfabetismo < 15.5 ~ "10 melhores",
      Taxa_Analfabetismo > 36.2 ~ "10 piores",
      TRUE ~ "197 restantes"
    )
  )

#### teste de hipótese ####

# teste T para duas amostras independetes = comparar individuos diferentes sobre a mesma variável

morte_piores <- df |>
  filter(Grupos == "10 piores") |>
  select(Mort_Infantil)

morte_melhores <- df |>
  filter(Grupos == "10 melhores") |>
  select(Mort_Infantil)

t.test(morte_piores, morte_melhores)

#### análise de uma regressão linear usando duas variáveis ####

outliers <- c(
  'Bernardo do Mearim', 'Bequimão', 'Alcântara', 
  'São José de Ribamar', 'Paço do Lumiar', 'São Luís'
)

df_modelo <- df |>
  filter(!Municipio %in% outliers)

modelo <- lm(Mort_Infantil ~ Taxa_Analfabetismo, data = df_modelo) # y = a + bx + e

summary(modelo)

#### visualizando a relação ####

require(ggrepel)

df_modelo |>
  ggplot() + 
  aes(x = Taxa_Analfabetismo, y = Mort_Infantil, color = Grupos) + 
  geom_point(size = 2) + 
  geom_text_repel(aes(label = Municipio), size = 2, data = subset(df_modelo, Grupos != '197 restantes')) + 
  labs(
    title = 'Relação entre a taxa de analfabetismo e a mortalidade infantil',
    subtitle = 'Maranhão - 2010',
    x = 'Taxa de Analfabetismo (%)',
    y = 'Taxa de Mortalidade Infantil (óbitos/mil nascidos vivos)',
    caption = 'Fonte: DataIMESC, elaborado pelo autor.'
  )+
  theme_minimal()
