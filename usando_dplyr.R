#? Comentários com "?" são comentários normais
#! Comentários com "!" são códigos errados
#* Comentários com "*" são para correções
# Apenas "#" são códigos comentados (ignorados)
#TODO é algo para fazermos juntos

#? Vamos adicionar a biblioteca dplyr 
library(dplyr)

#? E outras bibliotecas que serão úteis
library(lubridate)
library(stringr)

#? Vamos começar com os dados de pokemon
#? https://www.kaggle.com/datasets/igorcoelho24/pokemon-all-generations/versions/1?resource=download
dados <- read.csv("Dados/Pokemon_full.csv")
head(dados) #? vê as primeiras linhas de dados


#? A biblioteca dplyr possui o operador "pipe"
#? dado por  %>%
#? Ele "pega" tudo que está à esquerda dele e coloca como primeiro elemento
#? da função à direita.
#? Também é possível usar o operador "."
#? para especificar onde ele deve substituir.


#? Exemplo: contar o número de linhas de dados

nrow(dados)
dados %>% nrow()
dados %>% nrow(.)

#? Algumas funções da biblioteca dplyr

#? A função filter seleciona linhas com base em um teste
filter(dados, type == "grass")

#? podemos usar o seguinte comando também
(df_grass <- dados %>% filter(type == "grass"))

#TODO Vamos filtrar todos os pokemons do tipo fogo ou água
(df_fire_water <- dados %>% filter(type == "fire" | type == "water"))

#TODO Vamos filtrar todos os pokemons que tem  "fly"
(df_fly <- dados %>% filter(grepl("fly", name)))

################### Lição 13/04 ########################
#TODO Vamos filtrar todos os pokemons que tem  "bee" ou "saur"
dados %>% filter(grepl("bee", name)|grepl("saur", name))
#######################################################

#? A função pull devolve um vetor
pull(dados,name)
dados %>% pull(name)

#AULA cascata de funções (vantagems: bem visual e melhor de escrever)
dados %>% 
  filter(type=="fire") %>% #? filtrou apenas fogo
  pull(secundary.type) %>% #? extraiu os tipos secundarios
  unique                   #? apenas valores unicos

#? A função select seleciona colunas
dados %>% select(c(1, 2, 3)) #? pelo número da coluna
dados %>% select(name, type, height) #? pelo nome da coluna

dados %>% select(type) %>% unique #? tipos unicos


#TODO achar todas as combinações existentes de type e secondary.type
dados %>% select(type,secundary.type) %>% unique

#? Outras possibilidades
dados %>% names
dados %>%
    select(starts_with("h")) %>% head #? starts_with, ends_with, contains

dados %>% select(-name) %>% head #? negativo exclui as colunas

#? A função mutate modifica ou cria uma coluna com base em outras
mutate(dados,height2=2*height)

dados %>% 
    mutate(
        height2 = 2*height, #? nome diferente add no final
        speed = 2*speed, #? nome igual substitui coluna
        bee = grepl("bee",name)
    ) %>% head

#? A função arrange organiza o data frame com base em colunas

dados %>%
    arrange(name) %>% #? ascendente (A-Z)
        head() #? começo

dados %>%
    arrange(name) %>%
        tail() #? final

dados %>%
    arrange(desc(name)) %>% #? descendente (Z-A)
        tail()

dados %>%
  arrange(type,height) %>%
  head() 

#? Vamos fazer algumas contas!!

dados %>%
    summarise(
        media_altura = mean(height),
        media_peso = mean(weight)
    )

#? Podemos fazer isso por grupos
dados %>%
    group_by(type) %>%
        summarise(
            media_altura = mean(height),
            media_peso = mean(weight),
            N = n()
        ) %>%
            arrange(media_altura)


#TODO Filtrar os pokemons que tem o peso acima da média da altura do seu type
dados %>%
  group_by(type) %>%                  #? agrupa por tipo
  mutate(
    media_altura = mean(height),      #? calcula a media do tipo
    media_peso = mean(weight)
  ) %>% 
  filter(height > media_altura, weight > media_peso) %>%   #? filtra maiores que a media
  select(-media_altura)              #? apaga coluna de media
  
################### Lição 20/04 ########################
#TODO criar uma coluna com a transformação Z-score para altura POR type utilizando TODAS
dados %>%
  group_by(type) %>% 
  mutate(
    zscore_height = (height -mean(height))/sd(height)
  ) %>% 
  arrange(type)
#######################################################
