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
library(ggplot2)


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

# 20/04

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
df <- dados %>%
  group_by(type) %>% 
  mutate(
    zscore_height = (height - mean(height))/sd(height),
    zscore_weight = (weight - mean(weight))/sd(weight)
  ) 

library(ggplot2)

dados %>% pull(type) %>% unique

ggplot(df)+
  geom_density(aes(x=zscore_height, color= type))
#######################################################

# 12/05

#? Renomear colunas
dados %>%
  group_by(type) %>%
  summarise(
    media_altura = mean(height),
    media_peso = mean(weight),
    N = n()
  ) %>%
  arrange(media_altura) %>%
  rename("Número de pokemons" = N)

#? Movê-las
dados %>%
  group_by(type) %>%
  summarise(
    media_altura = mean(height),
    media_peso = mean(weight),
    N = n()
  ) %>%
  arrange(media_altura) %>%
  rename("Número de pokemons" = N) %>%
  relocate("Número de pokemons")
dados %>%
  group_by(type) %>%
  summarise(
    media_altura = mean(height),
    media_peso = mean(weight),
    N = n()
  ) %>%
  arrange(media_altura) %>%
  rename("Número de pokemons" = N) %>%
  relocate("Número de pokemons", .after = type)


#? rowwise

#? A função mutate e outras do pacote dplyr trabalham diretamente com as colunas
#? como se fossem operações de vetor

dados %>%
  mutate(
    name2 = paste(name, " - NOVO")
  ) %>% head

#? Na prática, o dplyr faz isso:
paste(dados$name, "- NOVO")

#? o que significa que a função PRECISA aceitar um vetor

#? Imagine que você queira criar uma função que testa se o valor de uma coluna
#? na observação i é maior ou menor que um dado valor e executa uma certa ação.

f <- function(x){
  if(x <= 15){ #? no caso, o valor é 300
    return("Executei essa ação")
  }else{
    
    return("Executei Aquela ação")
  }
}

x1 <- c(30, 16, 20, 3)
f(x1)

#! O código abaixo não funciona
dados %>%
  mutate(
    nova_var = f(height)
  ) %>%
  select(height, nova_var) %>% head(30)

#* O código abaixo funciona
#TODO
dados %>%
  rowwise() %>%
  mutate(
    nova_var = f(height)
  ) %>%
  select(height, nova_var) %>% head(30)


#? ifelse e case_when

dados %>%
  mutate(
    tamanho = ifelse(
      height < 15,
      "baixinho",
      "altão"
    )
  ) %>% head

ff <- function(y){
  resposta <- c()
  
  for(i in 1:length(y)){
    if(y[i] <= 15){ #? no caso, o valor é 300
      resposta[i] <- "baixinho"
    }else{
      
      resposta[i] <- "altão"
    }
  }
  
  return(resposta)
}

ff(x1)
x1

dados %>%
  mutate(
    tamanho = ff(height)
  ) %>% head

# 
# ifelse(
#   height < 5,
#   "baixinho",
#   ifelse(
#     height < 10, 
#     ...
#   )
# )

dados %>%
  mutate(
    tamanho = case_when(
      height < 5 ~ "baixinho",
      height < 10 ~ "pequeno",
      height < 15 ~ "médio",
      TRUE ~ "altão"
    )
  ) %>% head


# ERRADO!!!
dados %>%
  mutate(
    tamanho = case_when(
      height < 5 ~ "baixinho",
      height < 10 ~ "pequeno",
      height < 15 ~ "médio"
    ) # se não colocar o TRUE, retorna <NA>
  ) %>% head(15)

#? Alguns perrengues da vida

#! O código abaixo não funciona
dados %>%
  mutate(
    tamanho = case_when(
      height < 5 ~ "baixinho",
      height < 10 ~ "pequeno",
      height < 15 ~ NA,
      TRUE ~ "altão"
    )
  ) %>% head

#* O código abaixo conserta isso

dados %>%
  mutate(
    tamanho = case_when(
      height < 5 ~ "baixinho",
      height < 10 ~ "pequeno",
      height < 15 ~ NA_character_,
      TRUE ~ "altão"
    )
  ) %>% head

# Juntar dados

#bind

#rbind - linhas na mesma coluna (mesmo nome)

df_A = data.frame(A = c(1,2,3,4), B = c(5, 6, 3, 2))
df_B = data.frame(A = c(12,22,32,42), B = c(7, 5, 3, 2))

rbind(df_A, df_B)

#cbind - colunas na mesma linha (mesmo nome)

df_A = data.frame(A = c(1,2,3,4))
df_B = data.frame(B = c(12,22,32,42))

cbind(df_A, df_B)


# outras funçoes

# bind_rows (nomes diferentes, completa com <NA>)
df_A = data.frame(A = c(1,2,3,4), B = c(5, 6, 3, 2))
df_B = data.frame(A = c(12,22,32,42), C = c(7, 5, 3, 2))

bind_rows(df_A, df_B)

df_A = data.frame(A = c(1,2,3,4))
df_B = data.frame(B = c(12,22,32))

bind_cols(df_A, df_B) # checar comando!!


#? Vamos falar de JOIN

df_means <- dados %>%
  group_by(type) %>%
  summarise(
    media_h = mean(height),
    media_w = mean(weight)
  )
df_means

#? vamos excluir os grupos que começam com "g"

df_means <- df_means %>% 
  filter(!grepl("^g", type))

df_means

# outras opções (REGEX - expressões regulares)
df_means %>% 
  filter(grepl("^g", type)) #'g' no começo

df_means %>% 
  filter(grepl("g$", type)) #'g' no final

df_means %>% 
  filter(grepl(".+g.+", type)) #'g' no meio (.+ = pelo menos um)

df_means %>% 
  filter(grepl(".+g.*", type)) #'g' no meio (.* = zero ou mais)


#? vamos adicionar um grupo que não existe

novo_grupo <- data.frame(
  type = "Vozes da minha cabeça",
  media_h = 1000,
  media_w = 400.82
)

#TODO adicionar o grupo
df_means <- rbind(df_means, novo_grupo)

### BOA Prática - eliminar espaços nas extremidades das strings

dados <- dados %>% 
  mutate_if(is.character, function(x) trimws(x,"both"))

#? full_join -> manter tudo de todos (o que não corresponde, completa com <NA>)
#TODO

df <- full_join(dados, df_means, by = "type")
View(df)

#? inner_join -> manter tudo que corresponder nos dois
#TODO
df <- inner_join(dados, df_means, by = "type")
View(df)

#? left_join -> manter tudo do df da esquerda
#TODO
df <- left_join(dados, df_means, by = "type")
View(df)

#? right_join -> manter tudo do df da direita
#TODO
df <- right_join(dados, df_means, by = "type")
View(df)

dados %>% 
  left_join(df_means, by = "type") %>% 
  left_join(df_means, by = "type") %>% View


# SINTAXE
names(dados)

df_means <- dados %>%
  group_by(type, secundary.type) %>%
  summarise(
    media_h = mean(height),
    media_w = mean(weight)
  )

df_means


df <- right_join(dados, df_means, by = c("type", "secundary.type"))
View(df)

df_means <- df_means %>% rename(height = media_h)

df <- right_join(dados, df_means, by = c("type" = "type2", "secundary.type" = "secundary.type"))
View(df)

# 18/05

#? vamos adicionar um grupo que JÁ existe

novo_grupo <- data.frame(
  type = "bug",
  media_h = 10,
  media_w = 800
)

#TODO adicionar o grupo
df_means <- rbind(df_means, novo_grupo)

#? left_join
#TODO
df <- left_join(dados, df_means, by = "type")
View(df)
df %>% 
  filter(type == "bug") %>%  head(15)
df %>% tail()
# CUIDADO: mais de uma correspondencia, faz todas as combinações possíveis

#?#####################################################################
#? TIDYR
#?#####################################################################

library(tidyr)

#? baixado de https://livro.curso-r.com/

dados <- readr::read_rds("Dados/imdb.rds")
View(dados)
head(dados)
names(dados)

df <- dados %>% 
  select(titulo, orcamento, receita, receita_eua)
df

#TODO um gráfico de barras com 10 primeiros filmes
# cada barra vem de uma coluna e aparece com uma cor diferente


#TODO checar se cada filme tem apenas um genero associado

#? Pivoteamento

#? pivot_longer - agrupa colunas

df_long <- df %>%
  slice(1:10) %>% # 10 primeiros
  tidyr::pivot_longer(2:4, values_to = "Valor", names_to = "Tipo de Valor")

View(df_long)

ggplot()+
  geom_col(data = df_long, aes(x = titulo, y = Valor, fill = `Tipo de Valor`),
           position = position_dodge2()
           )+
  theme_bw()+
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1.0)
  )


## Pivot wider - separa coluna e várias
df_long %>% 
  tidyr::pivot_wider(names_from = `Tipo de Valor`, values_from = Valor)

# correlação cor()
# Calcular correlação do conjunto inteiro entre as variáveis



#? Emissões de ar
#? https://www.kaggle.com/datasets/ashishraut64/global-methane-emissions


#? Netflix
#? https://www.kaggle.com/datasets/victorsoeiro/netflix-tv-shows-and-movies
