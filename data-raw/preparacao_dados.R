# Curso-R
# R para Ciência de Dados I - Turma março de 2022
# Professores: Beatriz Milz, Amanda Amorim e Tereza Lacerda
# Aluno: Marcio Vakassugui

#------------------------------------------------------------------------------------------------------------>
#                                 PARTICIPAÇÃO DO BRASIL NAS OLIMPÍADAS                                      #
#                           Leitura dos dados e operações de pré-processamento                               #
#------------------------------------------------------------------------------------------------------------>

# leitura dos pacotes ----------------------------------------------------------------------------------------
library(dplyr)
library(ggplot2)
library(stringr)
library(tidyr)
library(janitor)

# ATIVIDADES DE PREPARAÇÃO DA BASE DE DADOS-------------------------------------------------------------------

# 1) leitura dos datasets "athlete_events.csv" e "noc_regions.csv"--------------------------------------------
# https://www.kaggle.com/datasets/mysarahmadbhat/120-years-of-olympic-history
base_dados_1_1 <- readr::read_csv("dados/base_dados1/athlete_events.csv")  # dados olimpiadas de 1896 a 2016
base_dados_1_2 <- readr::read_csv("dados/base_dados1/noc_regions.csv")     # dados regioes dos países

# 2) Uso do janitor - clean_names-----------------------------------------------------------------------------
# pacote e função apresentada pelo curso para padronização dos nomes - pacote janitor
base_dados_1_1 <- base_dados_1_1 |> 
  clean_names()

base_dados_1_2 <-  base_dados_1_2 |> 
  clean_names()

# Visualização da base_dados_1_1

# ----------> 271116 linhas e 15 colunas
head(base_dados_1_2)

# ----------> 230 linhas e 3 colunas

# 3) join dos datasets "base_dados_1_1" com "base_dados_1_2"--------------------------------------------------
# cruzamento por meio da coluna "NOC"
dados_agrupados_1 <- left_join(base_dados_1_1, base_dados_1_2, by = "noc")
dados_agrupados_1

# 4) Excluir campos da base "dados_agrupados_1"---------------------------------------------------------------
dados_agrupados_1$id <- NULL
dados_agrupados_1$games <- NULL
dados_agrupados_1$region <- NULL


# ----------> 271116 linhas e 14 colunas (acrescentadas as colunas "region" e "notes")

# 5) Alterar a coluna "Event"---------------------------------------------------------------------------------
# A coluna "event" será utilizada para a sumarização de dados exigindo distinguir os atletas uns dos outros,
# por exemplo, Diego Hipolito e Arthur Nori, medalhistas de prata e bronze em 2016. Neste caso, a coluna 
# event possui a mesma informação ("Gymnastics Men's Floor Exercise") para ambos. Será alterado o valor desta
# coluna para "Gymnastics Men's Floor Exercise prata" e "Gymnastics Men's Floor Exercise bronze".Assim uma
# sumarização usando distinct(event) retornará distintamente os resultados destes atletas.

dados_agrupados_1 <- dados_agrupados_1 |> 
  unite(event_new, c("event","medal"), sep = " ", remove = FALSE, na.rm= FALSE) |>  # cria a coluna event_new
  mutate(event  = NULL) |>                                                          # excluir a coluna event
  rename("event"="event_new")                                                       # renomeia a coluna

# 6) Leitura dos datasets  "athletes.csv" e "medals.csv"------------------------------------------------------
# fonte: https://www.kaggle.com/datasets/piterfm/tokyo-2020-olympics
base_dados_2_1 <- readr::read_csv("dados/base_dados2/athletes.csv")  # dados da olimpíada de 2020
base_dados_2_2 <- readr::read_csv("dados/base_dados2/medals.csv")    # dados dos atletas medalhistas

# 7) Uso do janitor - clean_names-----------------------------------------------------------------------------
  # pacote e função apresentada pelo curso para padronização dos nomes
  base_dados_2_1 <- base_dados_2_1 |> 
    clean_names()
  
  base_dados_2_2 <-  base_dados_2_2 |> 
    clean_names()

# 7) Visualização dos datasets--------------------------------------------------------------------------------
head(base_dados_2_1)
# ----------> 11656 linhas e 14 colunas
head(base_dados_2_2)
# ----------> 2401 linhas e 12 colunas

# 8) Analisando as variáveis dos datasets "dados_agrupados_1", "base_dados_2_1" e "base_dados_2_2"------------
names(base_dados_2_1)
names(base_dados_2_2)
names(dados_agrupados_1)

#------------------------------------------------------------------------------------------------------------#
# base_dados_2_1      base_dados_2_2        dados_agrupados1     obs
# ---                 ---                   id                   não incluir na base final
# name                athlete_name          name                 padronizar o nome para "Name" nas bases
# birth_date          ---                   age                  calcular age na 1º base e padronizar "Age"
# short_name          athlete_short_name    ---                  não incluir na base final
# gender              athlete_sex           sex                  padronizar o nome para para "Sex" nas bases
# birth_place         ---                   ---                  não incluir na base final
# birth_country       ---                   ---                  não incluir na base final
# country             country               region               padronizarn o nome para "region" nas bases
# country_code        country_code          noc                  padronizar o nome para "NOC" nas bases
# discipline          discipline            sport                padronizar o nome para "modalidade" nas bases
# discipline_code     discipline_code       ---                  não incluir na base
# residence_place     ---                   ---                  não incluir na base
# residence_country   ---                   ---                  não incluir na base
# height_m/ft         ---                   height               na 1º base é informado a altura dos atletas
#                                                                em duas unidades (metros e pés) separadas por
#                                                                contra-barras. Será selecionada apenas os 
#                                                                valores em metros.
#                                                                Para isto, deve-se separar estes valores em
#                                                                duas colunas chamadas de Height e Height_ft.
# ---                 ---                   weight               padronizar o nome para "peso" nas bases
# url                 athlete_link          ---                  excluir da base
# ---                 ---                   team                 excluir da base
# ---                 ---                   games                excluir da base
# ---                 ---                   year                 na 1º base será inserido o valor 2021 na
#                                                                coluna, pois esta contém os dados das 
#                                                                Olimpíadas de Tóquio em 2021. Padronizar o 
#                                                                nome para "Year" nas bases
# ---                 ---                   season               excluir da base
# ---                 ---                   city                 na 1º base será inserido o valor "Toquio" na
#                                                                coluna, pois esta contém os dados das
#                                                                Olimpíadas de Tóquio em 2021. Padronizar o 
#                                                                nome para "City" nas bases
# ---                 ---                   event                excluir da base
# ---                 medal_type            medal                padronizar o nome para "Medal" nas bases
# ---                 ---                   medal_Code           excluir da base
# ---                 ---                   notes                excluir da base
#------------------------------------------------------------------------------------------------------------#

# 9) Unir as colunas "event", "discipline" e "medal" em uma nova coluna "event_new"---------------------------
# Pelo mesmo motivo explicado no item 4.
base_dados_2_2 <- base_dados_2_2 |> 
  tidyr::unite("event_new", c("event", "discipline", "medal_type"), sep = " ", na.rm = FALSE, remove = FALSE)
  
base_dados_2_2$event <- NULL

# 10) Operações preparatórias para o join dos datasets base_dados_2_1 e base_dados_2_2-------------------------
# 11) renomear e selecionar as variáveis "athlete_name", "medal_type" e "event" de base_dados_2_2--------------
base_dados_2_2 <- base_dados_2_2 |>
  rename("name" = "athlete_name", "medal" = "medal_type", "event" = "event_new") |>
  select(name, event, medal, country_code)

# 12) Join dos datasets "base_dados_2_1" com "base_dados_2_2"--------------------------------------------------
dados_agrupados_2 <- left_join(base_dados_2_1, base_dados_2_2, by = c("name", "country_code"))

# PESQUISA DA LINGUAGEM R - COMO ANEXAR AS LINHAS DE UM DATASET EM OUTRO - EMPILHAR
# Pela documentação do rbind, pode-se juntar datasets pelas linhas, entretanto estes datasets precisam ter o
# mesmo número de colunas.

# 13) Operações preparatórias para a inclusão das linhas de "dados_agrupados_2 em dados_agrupados_1"----------

# 15) separar dados da coluna "height_m/ft" em duas colunas "Height" e "Height_ft" e eliminar "Height_ft"-----
dados_agrupados_2 <- dados_agrupados_2 |>
  tidyr::separate(height_m_ft, c("height", "height_ft"), sep = "\\/")

# 16) Eliminar a coluna "Height_ft"---------------------------------------------------------------------------
dados_agrupados_2$height_ft <- NULL

# 17) Alterar a altura de dados_agrupados_2 de metros para centímetros (escalas diferentes)-------------------
dados_agrupados_2$height <- as.numeric(dados_agrupados_2$height) * 100 

# 18) Criar a coluna cidade com Toquio na base dados_agrupados_2----------------------------------------------
dados_agrupados_2 <- dados_agrupados_2 |>
  mutate(city = "Toquio")

# 19) Criar a coluna ano com o dado 2021 na base dados_agrupados_2--------------------------------------------
dados_agrupados_2 <- dados_agrupados_2 |>
  mutate(year = 2020)

# 20) Calcular a idade dos atletas a partir de sua data de nascimento e a data de abertura dos jogos----------
data_olimpiada_toquio <- as.Date("2021-07-23")

dados_agrupados_2 <- dados_agrupados_2 |>
  mutate(age = floor(as.numeric((data_olimpiada_toquio - dados_agrupados_2$birth_date) / 365.25)))

# 21) Renomear para padronizar os nomes entre as bases de dados e criar a coluna "Weight" com NA's----------
dados_agrupados_2 <- dados_agrupados_2 |>
  select(
    "name", "gender", "age", "height", "country", "country_code", "city", "discipline", "event",
    "medal", "year"
  ) |>
  rename(
   "sex" = "gender", "team" = "country", "noc" = "country_code", "sport" = "discipline"
  ) |>
  mutate(weight = NA)
# 21) Filtra os dados da base_agrupada_1 apenas com os dados das olimpíadas de verão
dados_agrupados_1 <- dados_agrupados_1 |> 
  filter(season == "Summer")

# 22) Padronizar o nome dos atletas - Na base dados_agrupados_1 o padrão é nome e sobrenome (ex. Pedro Silva)
# e na base dados_agrupados_2 o padrão é sobrenome (maiúsculas) e nome (ex. SILVA Pedro)

new_name <- c()  # vetor para armazenamento dos nomes no padrão

for (i in seq_along(1:nrow(dados_agrupados_2))){
  sobrenome <- dados_agrupados_2$name[i] |> 
    stringr::str_extract("[d[e,i,o,s] A-Z\u00C0-\u00DD ]+ ") |>  # extrai o sobrenome considerando "de", "di"
    stringr::str_squish()                                        # "do", "dos" + nomes com maiúsculas.
  
    nome <- dados_agrupados_2$name[i] |>                         # do nome completo extrai o sobrenome acima
    stringr::str_remove(sobrenome) |>                            # obtendo-se apenas o nome
    stringr::str_squish()                                        # squish() elimina espaços vazios
  
  nome_completo <- glue::glue("{nome} {sobrenome}")              # obtemos o nome completo
  
  if(nome_completo == "NA NA"){                                  # trata o seguinte erro: no ex. nome completo
    nome_completo <- dados_agrupados_2$name[i] |>                # = PEDRO, sem sobrenome o regex do laço for
      str_to_title()                                             # retorna sobrenome NA e o nome NA
  }else{                                                         # esta condição anula as operações acima e 
    nome_completo <- nome_completo |>                            # retorna o nome original PEDRO
      str_to_title()
  }
  
  new_name <- append(new_name, nome_completo)                    # adiciona o nome obtido no vetor new-name
}

dados_agrupados_2$name <- new_name                               # atualiza o campo Name com os novos nomes

# 23) Padronizar o nome dos atletas de dados_agrupados_2 com os nomes de dados_agrupados_1

new_name_1 <- c()                                               # vetor para armazenamento dos nomes alterados

for (j in seq_along(1:nrow(dados_agrupados_1))){
  nome_completo_1 <- dados_agrupados_1$name[j] |> 
    str_to_title()
  new_name_1 <- append(new_name_1, nome_completo_1)
}

dados_agrupados_1$name <- dados_agrupados_1$name <- new_name_1

# 24) Excluir as colunas season e notes de dados_agrupados_1
dados_agrupados_1$season <- NULL
dados_agrupados_1$notes <- NULL


# 24) Anexar à base "dados_agrupados_1" os dados da base "dados_agrupados_2"----------------------------------
# aqui, utilizamos a função rbind
dados_agrupados_3 <- rbind(dados_agrupados_1, dados_agrupados_2)

# FINALIZAÇÃO DA LIMPEZA E PREPARAÇÃO DA BASE-----------------------------------------------------------------
# 1) Verificar os tipos das variáveis-------------------------------------------------------------------------
glimpse(dados_agrupados_3)
# ----------> 283,001 linhas e 12 colunas

# 2) Altera o tipo da variável--------------------------------------------------------------------------------
dados_agrupados_3$sex <- as.factor(dados_agrupados_3$sex)

# 3) Análise de valores missings------------------------------------------------------------------------------
skimr::skim(dados_agrupados_3)

#----------------------------------------------->
# Existência de NA's em:
# -----------> age     (96,0% completos)
# -----------> height  (74,8% completos)
# -----------> Weight  (72,0% completos)
# -----------> sport   (99,9% completos)
# -----------> event   (96,0% completos)
# -----------> medal   (15,6% completos)  - trata-se do percentual de medalhistas
#----------------------------------------------->

# 4) Renomear as colunas--------------------------------------------------------------------------------------
names(dados_agrupados_3)

dados_agrupados_3 <- rename(dados_agrupados_3,
  "nome" = "name", "sexo" = "sex", "idade" = "age",
  "altura" = "height", "peso" = "weight", "time" = "team", "ano" = "year",
  "cidade" = "city", "modalidade" = "sport", "evento" = "event",
  "medalha" = "medal"
)

# 5) Informações sobre as variáveis --------------------------------------------------------------------------
#-------------------------------------------------------------->
# sobre as variáveis
# Name      = nome do atleta (str)
# Sex       = sexo (factor)
# Age       = idade (int)
# Height    = altura (em centímetros)
# Weight    = peso (em kilogramas)
# Team      = equipe (chr)
# NOC       = comitê olímpico nacional (código de três letras)
# Year      = ano de realização dos jogos (int)
# City      = cidade sede dos jogos (str)
# Sport     = modalidade do esporte (str)
# Event     = evento (str)
# Medal     = ouro, prota, bronze ou NA (str)
#---------------------------------------------------------------->

# Campo "Medals" com divergência nos valores (Gold medal e Gold, Silver medal e Silver, Bronze medal e Bronze)
# Padronizar os valores para Gold, Silver e Bronze
# usando uma função e um laço for para padronização do campo medalhas
troca_valores <- function(valor_antigo, valor_novo) {
  if (valor_antigo %in% valor_novo) {
    return(valor_novo[-1])
  } else {
    return(valor_antigo)
  }
}

alterar_valor_de_para <- c("Gold Medal", "Gold")
for (i in seq_along(1:nrow(dados_agrupados_3))) {
  dados_agrupados_3$medalha[i] <- troca_valores(dados_agrupados_3$medalha[i], alterar_valor_de_para)
}

alterar_valor_de_para <- c("Silver Medal", "Silver")
for (i in seq_along(1:nrow(dados_agrupados_3))) {
  dados_agrupados_3$medalha[i] <- troca_valores(dados_agrupados_3$medalha[i], alterar_valor_de_para)
}

alterar_valor_de_para <- c("Bronze Medal", "Bronze")
for (i in seq_along(1:nrow(dados_agrupados_3))) {
  dados_agrupados_3$medalha[i] <- troca_valores(dados_agrupados_3$medalha[i], alterar_valor_de_para)
}

# usando a função e um laço for para padronização do campo sexo
alterar_valor_de_para <- c("M", "Male")
for (i in seq_along(1:nrow(dados_agrupados_3))) {
  dados_agrupados_3$sexo[i] <- troca_valores(dados_agrupados_3$sexo[i], alterar_valor_de_para)
}

alterar_valor_de_para <- c("F", "Female")
for (i in seq_along(1:nrow(dados_agrupados_3))) {
  dados_agrupados_3$sexo[i] <- troca_valores(dados_agrupados_3$sexo[i], alterar_valor_de_para)
}

# Salvar base de dados limpa----------------------------------------------------------------------------------
dados_agrupados_3 <- readr::write_rds(dados_agrupados_3, "dados/base_dados_definitiva/olimpiadas_base_limpa.rds")

