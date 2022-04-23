# Curso-R
# R para Ciência de Dados I
# Professores: Beatriz Milz, Amanda Amorim e Tereza Lacerda
# Aluno: Marcio Vakassugui

#------------------------------------------------------------------------------------------------------------>
#                                 ESTATÍSTICAS DO BRASIL NAS OLIMPÍADAS                                      #
#                                     Análise Exploratório dos dados                                         #
#------------------------------------------------------------------------------------------------------------>

# Pacotes
library(dplyr)
library(readr)
library(ggplot2)
library(ggrepel)
library(kableExtra)

# Leitura da base de dados limpa------------------------------------------------------------------------------
dados_agrupados <- readr::read_rds("dados/base_dados_definitiva/olimpiadas_base_limpa.rds")

# -----------> 234437 linhas e 12 colunas

# Selecionar os dados dos atletas do Brasil-------------------------------------------------------------------
dados_agrupados_brasil <- dados_agrupados |>
  filter(noc == "BRA")

# -----------> 4104 linhas e 12 colunas

head(dados_agrupados_brasil)

# Frequências absolutas e relativas dos atletas por sexo -----------------------------------------------------
# excluir os valores NA's
dados_agrupados_brasil_no_na <- dados_agrupados_brasil |>
  filter(!is.na(sexo))

f_abs_sexo <- plyr::count(dados_agrupados_brasil_no_na$sexo)

# -----------> 12977 sexo feminino e 2825 sexo masculino

f_rel_sexf <- f_abs_sexo[1, 2] / sum(f_abs_sexo[2]) * 100

# -----------> 31,13% sexo feminino

f_rel_sexm <- f_abs_sexo[2, 2] / sum(f_abs_sexo[2]) * 100

# -----------> 68,87% sexo masculino

dados_agrupados_brasil

# Sumário dos dados ------------------------------------------------------------------------------------------
summary(dados_agrupados_brasil)

# -----------> menor idade = 13 anos
# -----------> maior idade = 56 anos
# -----------> média de idades = 25,75 anos

# -----------> menor altura = 1,33 metros
# -----------> maior altura = 2,17 metros
# -----------> média de altura = 1,777 metros

# -----------> menor peso = 30 Kg
# -----------> maior peso = 160 Kg
# -----------> média de pesos = 72,53 Kg  obs: dados da Olimpíada de 2021 não possuem valores de pesos 

# -----------> primeira participação em 1900  ** vefificar com informação de primeira em 1920

# Localizar os atletas acima descritos------------------------------------------------------------------------
# o atleta mais jovem, usando a função slice_min() vista no curso
mais_jovem <- dados_agrupados_brasil |>
  slice_min(order_by = idade) |> 
  arrange(idade)

# -----------> Talita de Alencar Rodrigues - jogos de 1948 - em Londres - esporte natação
# -----------> modalidade Swimming Women's 4 x 100 metres Freestyle Relay
# -----------> Raissa Leal - jogos de 2021 - em Toquio - esporte skateboarding
# -----------> evento women's street

# o atleta mais velho, usando a função slice_max() vista no curso
mais_velho <- dados_agrupados_brasil |>
  slice_max(order_by = idade) |> 
  arrange(desc(idade))

# -----------> Nelson Pesso Filho - jogos de 1992 - em Barcelona - esporte Equestrianism
# -----------> modalidade Equestrianism Mixed Jumping, Individual e Equestrianism Mixed Jumping, Team

# os atletas com menor estatura. Vamos visualizar os atletas com menores estaturas
mais_baixo <- dados_agrupados_brasil |>
  group_by(altura) |>
  filter(altura <= 145) |>
  arrange(altura)

# -----------> Flavia Lopes Saraiva - 16 anos - 31 kg - jogos de 2016 - no Rio de Janeiro - esporte Gymnastics
# -----------> Daiane Garcia dos Santos - 21 anos - 45 kg - jogos de 2004 em Atenas - esporte Gymnastics

# Medalhistas de Ouro, Prata e Bronze-------------------------------------------------------------------------
medalhistas_brasil <- dados_agrupados_brasil |>
  filter((!is.null(medalha)) & (!is.na(medalha))) # filtrar as linhas com campo medalha não NULL e não NA

# -----------> total de medalhistas = 530

# Total de medalhas de ouro-----------------------------------------------------------------------------------
medalhistas_por_tipo <- medalhistas_brasil |>
  group_by(medalha) |>
  summarise(quantidade = n())

# -----------> 138 atletas conquistarm ouro
# -----------> 192 atletas conquistaram prata
# -----------> 200 atletas conquistaram bronze

# Distribuição de idades entre os medalhistas de ouro---------------------------------------------------------
# Sendo as idades variando de 13 a 59 anos, será construída a tabela de frequências de medalhas de ouro por
# idades dos atletas

intervalo_classes_idade <- round(seq(0, 70, 10), 1)  # usaremos 6 intervalos
medalhistas_ouro <- medalhistas_brasil |>
  filter(medalha == "Gold")

total_ouro_idade <- plyr::count(cut(medalhistas_ouro$idade,
  breaks = intervalo_classes_idade,
  right = FALSE
))

# (10 - 20]  = 6  atletas
# (20 - 30]  = 98 atletas
# (30 - 40]  = 32 atletas
# (40 - 50]  = 2  atletas
# (50 - 60]  = 0  atletas
# (60 - 70]  = 0  atletas

# Total de medalhas de prata----------------------------------------------------------------------------------
medalhistas_prata <- medalhistas_brasil |>
  filter(medalha == "Silver")

# -----------> 192 medalhas de prata

# Distribuição de idades entre os medalhistas de prata--------------------------------------------------------
total_prata_idade <- plyr::count(cut(medalhistas_prata$idade,
  breaks = intervalo_classes_idade,
  right = FALSE
))

# (10 - 20]  = 16   atletas
# (20 - 30]  = 133  atletas
# (30 - 40]  = 43   atletas
# (40 - 50]  = 0    atletas
# (50 - 60]  = 0    atletas
# (60 - 70]  = 0    atletas

# Total de medalhas de prata----------------------------------------------------------------------------------
medalhistas_bronze <- medalhistas_brasil |>
  filter(medalha == "Bronze" & !is.na(medalha))

# -----------> 200 medalhas de bronze

# Distribuição de idades entre os medalhistas de bronze-------------------------------------------------------
total_bronze_idade <- plyr::count(cut(medalhistas_bronze$idade,
  breaks = intervalo_classes_idade,
  right = FALSE
))

# (10 - 20]  = 8    atletas
# (20 - 30]  = 153  atletas
# (30 - 40]  = 32   atletas
# (40 - 50]  = 5    atletas
# (50 - 60]  = 1    atletas
# NA         = 1    atletas    *vide observação abaixo

# NA devido a falta de informação sobre Fernando Soledade.
# Encontrar informações sobre o atirador Fernando Soledade tem sido um desafio tanto para os profissionais da
# Confederação Brasileira de Tiro Esportivo quanto para historiadores que se dedicam a estudar o desempenho
# dos brasileiros nos Jogos Olímpicos da Antuérpia-1920. A data de nascimento de Fernando Soledade permanece
# um mistério. Mas sabe-se que ele era do Rio de Janeiro, onde atuou como médico. Sua marca registrada durante
# as provas na Antuérpia foi um enorme chapéu de abas, que ele usava para protegê-lo do sol. Sua atuação nos
# Jogos de 1920 ajudou o Brasil a conquistar a medalha de bronze por equipe e, ao retornar da competição na
# Bélgica, Fernando Soledade, junto com os demais atletas que defenderam o Brasil na competição, foi saudado 
# pelo Presidente da República Epitácio Pessoa, pelo esportista e poeta Coelho Neto e pelo Presidente do
# Fluminense Arnaldo Guinle em uma solenidade realizada no Salão Nobre do Fluminense. Pelo desempenho da 
# equipe de tiro na Antuérpia (o país conquistou um ouro, uma prata e um bronze), cada um dos atiradores 
# recebeu uma placa de prata das mãos do presidente.
# fonte: http://rededoesporte.gov.br/pt-br/megaeventos/olimpiadas/medalhistas/fernando-soledade


# Join dos dataframes-----------------------------------------------------------------------------------------
# consolidar os dados em uma tabela com quantidade de medalhas por intervalo de idades
total_medalhas_idade <- left_join(total_ouro_idade, total_prata_idade, by = "x")

total_medalhas_idade_completo <- left_join(total_medalhas_idade, total_bronze_idade, by = "x")

total_medalhas_idade_completo <- total_medalhas_idade_completo |>
  rename("intervalo_idades" = "x", "Ouro" = "freq.x", "Prata" = "freq", "Bronze" = "freq.y")

# Gráfico de Barras Medalhas x idade--------------------------------------------------------------------------
# obs. o total de medalhas corresponde ao total de atletas, seja em equipes coletivas ou individuais, que 
# receberam medalhas. Então, conta-se todos os atletas de seleções de volei, futebol, etc como também os 
# atletas de modalidades individuais como ginástica, judô, etc.
grafico_medalhas_tipo <- medalhistas_brasil |>
  group_by(medalha) |>
  summarise(quantidade = n()) |>
  ggplot(aes(x = medalha, y = quantidade, label = quantidade, fill = medalha)) +
  geom_col() +
  geom_label(size = 2.8, label.size = 0.5, alpha = 0.5, nudge_x = 0.0, nudge_y = 12, show.legend = FALSE) +
  scale_fill_manual(values = c("#C8D3D5", "#6E8387", "#D0C6D2")) +
  labs(title = "Medalhas de Atletas do Brasil nos Jogos Olímpicos",
       subtitle = "Quantidades x tipos de medalhas") +
  xlab("Medalhas") +
  ylab("Quantidades") +
  theme_classic()

grafico_medalhas_tipo

# Grafico de Barras Medalhas de ouro x versus idade-----------------------------------------------------------
# usaremos este gráfico no relatório por  meio de uma consulta do leitor por tipo de medalha, usando o pacote
# shiny e suas funções.
intervalo_classes_idade <- round(seq(0, 70, 10), 1)            # vetor para o intervalo de classes de idade
medalhistas_ouro <- medalhistas_brasil |>
  filter(medalha == "Gold")                                    # apenas atletas brasileiros com ouro

total_ouro_idade <- plyr::count(cut(medalhistas_ouro$idade,    # intervalos de classes de 0 a 70 anos p/ ouro
                                    breaks = intervalo_classes_idade,
                                    right = FALSE              # inclui o limite inferior e exclui o superior
))

grafico_medalha_x_idades <- total_ouro_idade |>
  ggplot(aes(x = x, y = freq, label = freq, fill = x)) +
  geom_col(na.rm = TRUE, alpha = 0.7) +
  geom_label(size = 4, alpha = 0.5, nudge_y = 4, show.legend = FALSE) +
  scale_fill_manual(values = c("#00A5DB", "#1B70E3", "#2334CC", "#8567E6")) +
  labs(title = "Atletas com Medalhas de Ouro do Brasil por Intervalo de Idades", subtitle = "Medalhas de Ouro ",
       fill = "idades") +
  xlab("Intervalos de Idade") +
  ylab("Quantidades de Medalhas") +
  theme_classic()+
  theme(legend.position = "none",
        axis.text.x=element_text(size=11),
        axis.text.y=element_text(size=11))
grafico_medalha_x_idades 

# Grafico de Barras Medalhas de Prata x versus idade----------------------------------------------------------
# usaremos este gráfico no relatório por  meio de uma consulta do leitor por tipo de medalha, usando o pacote
# shiny e suas funções.
medalhistas_prata <- medalhistas_brasil |>                     # para os medalhistas de prata
  filter(medalha == "Silver")

total_prata_idade <- plyr::count(cut(medalhistas_prata$idade,  
                                     breaks = intervalo_classes_idade,
                                     right = FALSE
))

grafico_medalha_x_idades <- total_prata_idade |>
  ggplot(aes(x = x, y = freq, label = freq, fill = x))+
  geom_col(na.rm = TRUE, alpha = 0.7) +
  geom_label(size = 4, alpha = 0.5, nudge_y = 4, show.legend = FALSE) +
  scale_fill_manual(values = c("#00A5DB", "#1B70E3", "#2334CC", "#8567E6")) +
  labs(
    title = "Atletas com Medalhas de Prata do Brasil por Intervalo de Idades", subtitle = "Medalhas de Prata ",
    fill = "idades"
  ) +
  xlab("Intervalos de Idade") +
  ylab("Quantidades de Medalhas") +
  theme_classic() +
  theme(legend.position = "none",
        axis.text.x=element_text(size=11),
        axis.text.y=element_text(size=11))
grafico_medalha_x_idades  

# Grafico de Barras Medalhas de Bronze x versus idade---------------------------------------------------------
# usaremos este gráfico no relatório por  meio de uma consulta do leitor por tipo de medalha, usando o pacote
# shiny e suas funções.
medalhistas_bronze <- medalhistas_brasil |>                    # para os medalhistas de bronze
  filter(medalha == "Bronze" & !is.na(medalha))

total_bronze_idade <- plyr::count(cut(medalhistas_bronze$idade,
                                      breaks = intervalo_classes_idade,
                                      right = FALSE
))

grafico_medalha_x_idades <- total_bronze_idade |>
  ggplot(aes(x = x, y = freq, label = freq, fill = x)) +
  geom_col(na.rm = TRUE, alpha = 0.7) +
  geom_label(size = 4, alpha = 0.5, nudge_y = 4, show.legend = FALSE) +
  scale_fill_manual(values = c("#00A5DB", "#1B70E3", "#2334CC", "#8567E6", "#AA4586", "#59FFA0")) +
  labs(
    title = "Atletas com Medalhas de Bronze do Brasil por Intervalo de Idades", subtitle = "Medalhas de Bronze ",
    fill = "idades"
  ) +
  xlab("Intervalos de Idade") +
  ylab("Quantidades de Medalhas") +
  theme_classic()+
  theme(legend.position = "none",
        axis.text.x=element_text(size=11),
        axis.text.y=element_text(size=11))
grafico_medalha_x_idades                                   # gera o gráfico para bronze

# Como descobrir a primeira mulher brasileira a participar de uma olimpíada-----------------------------------
# o resultado será usado no relatório, juntamente com dados adicionais obtidos da internet sobre o atleta
dados_agrupados_brasil |>
  group_by(ano, sexo) |>
  filter(sexo == "Female") |>
  arrange(ano) |>
  head()

# Resposta: Maria Emma Hulda Lenk Zigler em 1932 com 17 anos de idade em Los Angeles, atleta da natação.

# Como descobrir a mulher mais jovem a conquistar uma medalha olímpica----------------------------------------
# o resultado será usado no relatório, juntamente com dados adicionais obtidos da internet sobre o atleta
dados_agrupados_brasil |>
  group_by(ano, sexo, medalha) |>
  filter(sexo == "Female" & !is.na(medalha)) |>
  arrange(ano, idade) |>
  head(40)

# Resposta: Três atletas da seleção brasileira de basquete, todas com 21 anos de idade, consquistaram a
# medalha de prata em 1996, na cidade de Atlanta.
# Cintia Silva dos Santos.
# Leila de Souza Sobral Freitas
# Silvia Andrea Santos Luz (Silvinha)

# Como descobrir o atleta brasileiro com o maior número de medalhas-------------------------------------------
# o resultado será usado no relatório, juntamente com dados adicionais obtidos da internet sobre o atleta
maiores_medalhistas <- medalhistas_brasil |>
  group_by(nome) |>
  summarise(quantidade = n()) |>
  arrange(desc(quantidade)) |>
  head(5) |>
  View()

torben <- medalhistas_brasil |>
  group_by(nome, ano, medalha, evento) |>
  filter(nome == "Torben Schmidt Grael") |>
  summarize(nome,medalha) |> 
  rename("NOME"="nome", "ANO" = "ano", "MEDALHA" = "medalha", "EVENTO" = "evento")

scheidt <- medalhistas_brasil |>
  group_by(nome, ano, medalha, evento) |>
  filter(nome == "Robert Scheidt") |>
  summarize(nome, medalha) |> 
  rename("NOME"="nome", "ANO" = "ano", "MEDALHA" = "medalha", "EVENTO" = "evento")

# Os maiores medalhistas brasileiros são:
# Robert Scheidt e Torben Schimdt Grael com cinco medalhas cada.
#   1996 Gold    Sailing Mixed One Person Dinghy
#   2000 Silver  Sailing Mixed One Person Dinghy
#   2004 Gold    Sailing Mixed One Person Dinghy
#   2008 Silver  Sailing Men's Two Person Keelboat
#   2012 Bronze  Sailing Men's Two Person Keelboat

# Torben Schimidt Grael
#  1984 Silver  Sailing Mixed Three Person Keelboat
#  1988 Bronze  Sailing Mixed Two Person Keelboat
#  1996 Gold    Sailing Mixed Two Person Keelboat
#  2000 Bronze  Sailing Mixed Two Person Keelboat
#  2004 Gold    Sailing Men's Two Person Keelboat

# Consolidade os resultados em uma tabela para o relatório----------------------------------------------------
maiores_medalhistas_brasileiros <- bind_rows(torben, scheidt, gustavo_borges, isaquias, escadinha)

# visualizar os dados
View(maiores_medalhistas_brasileiros)

# Relação peso, altura por modalidade esportiva para mulheres

options(ggrepel.max.overlaps = Inf)

peso_altura_modalidade_mulheres <- dados_agrupados_brasil |> 
  filter(ano == 2016, sexo == "Female") |> 
  group_by(modalidade) |> 
  mutate(media_altura = round(mean(altura, na.rm = TRUE),2),
         media_peso = round(mean(peso, na.rm = TRUE),2)) |> 
  distinct(modalidade, media_altura, media_peso) |> 
  na.exclude() |> 
  ggplot()+
  geom_point()+
  aes(x=media_peso, y = media_altura)+
  geom_text_repel(aes(label = modalidade, color = modalidade))+
  labs(x= "Peso Médio (Kg)", y = "Altura Média (m)",
       title ="Relação de Peso Médio e Altura Média por Modalidade Esportiva",
       subtitle = "Dados para Atletas Femininas dos Jogos Olimpicos de 2016") +
  scale_x_continuous(breaks = seq(40,130,5)) +
  scale_y_continuous(breaks = seq(130,220,5)) +
  theme_light()+
  theme(legend.position = "none",
        axis.text.x=element_text(size=12),
        axis.text.y=element_text(size=12))
peso_altura_modalidade_mulheres       


# Relação peso, altura por modalidade esportiva para homens

peso_altura_modalidade_homens <- dados_agrupados_brasil |> 
  filter(ano == 2016, sexo == "Male") |> 
  group_by(modalidade) |> 
  mutate(media_altura = round(mean(altura, na.rm = TRUE),2),
         media_peso = round(mean(peso, na.rm = TRUE),2)) |> 
  distinct(modalidade, media_altura, media_peso) |> 
  na.exclude() |> 
  ggplot()+
  geom_point()+
  aes(x=media_peso, y = media_altura)+
  geom_text_repel(aes(label = modalidade, color = modalidade))+
  labs(x= "Peso Médio (Kg)", y = "Altura Média (m)",
       title ="Relação de Peso Médio e Altura Média por Modalidade Esportiva",
       subtitle = "Dados para Atletas Masculinos dos Jogos Olimpicos de 2016") +
  scale_x_continuous(breaks = seq(40,130,5)) +
  scale_y_continuous(breaks = seq(130,240,5)) +
  theme_light()+
  theme(legend.position = "none",
        axis.text.x=element_text(size=12),
        axis.text.y=element_text(size=12))
peso_altura_modalidade_homens    


# Relacao imc, idade e modalidade esportiva
# o indice de massa corporea e obtido 

# calcular o imc dos altetas
# dados_agrupados_brasil <- dados_agrupados_brasil |> 
#   mutate(imc = round((peso/(altura/100 * altura/100)),3))

# para homens

imc_idade_modalidade_homens <- dados_agrupados_brasil |> 
  filter(ano == 2016, sexo == "Male" ) |> 
  group_by(modalidade) |> 
  mutate(imc = round((peso/(altura/100 * altura/100)),3),
         media_idade = round(mean(idade, na.rm = TRUE),2),
         media_imc = round(mean(imc, na.rm = TRUE),2)) |>
  distinct(modalidade,media_idade, media_imc) |> 
  na.exclude() |> 
  ggplot()+
  geom_point()+
  aes(x=media_idade, y = media_imc)+
  geom_hline(yintercept = 18.6, 
             size = 0.5,
             linetype = 2,
             colour = "#6BA368")+
  geom_hline(yintercept = 24.9, 
             size = 0.5,
             linetype = 2,
             colour = "#6BA368")+
  geom_hline(yintercept = 29.9, 
             size = 0.5,
             linetype = 2,
             colour = "#BA1200")+
  geom_hline(yintercept = 39.9, 
             size = 0.5,
             linetype = 2,
             colour = "#470063")+
  geom_text_repel(aes(label = modalidade, color = modalidade))+
  labs(x= "Idade (anos)", y = expression(IMC~(kg/m^2)),
       title ="Relação de IMC e Idade por Modalidade Esportiva",
       subtitle = "Dados para Atletas Masculinos") +
  scale_x_continuous(breaks = seq(10,50,2)) +
  scale_y_continuous(breaks = seq(10,45,2)) +
  theme_light()+
  theme(legend.position = "none")+
  annotate("text", x=44, y=21.5, label = "Peso Normal")+
  annotate("text", x=44, y=27, label = "Sobrepeso")+
  annotate("text", x=43.5, y=35.5, label = "Obesidade Grau 1")

imc_idade_modalidade_homens

# para mulheres

imc_idade_modalidade_mulheres <- dados_agrupados_brasil |> 
  filter(ano == 2016, sexo == "Female" ) |> 
  group_by(modalidade) |> 
  mutate(imc = round((peso/(altura/100 * altura/100)),3),
         media_idade = round(mean(idade, na.rm = TRUE),2),
         media_imc = round(mean(imc, na.rm = TRUE),2)) |>
  distinct(modalidade,media_idade, media_imc) |> 
  na.exclude() |> 
  ggplot()+
  geom_point()+
  aes(x=media_idade, y = media_imc)+
  geom_hline(yintercept = 18.6, 
             size = 0.5,
             linetype = 2,
             colour = "#6BA368")+
  geom_hline(yintercept = 24.9, 
             size = 0.5,
             linetype = 2,
             colour = "#6BA368")+
  geom_text_repel(aes(label = modalidade, color = modalidade))+
  labs(x= "Idade (anos)", y = "IMC (kg/m2)",
       title ="Relação de IMC e Idade por Modalidade Esportiva",
       subtitle = "Dados para Atletas Femininos") +
  scale_x_continuous(breaks = seq(18,50,2)) +
  scale_y_continuous(breaks = seq(18,25,2)) +
  theme_light()+
  theme(legend.position = "none")+
  annotate("text", x=42, y=21.75, label = "Peso Normal")+
  annotate("text", x=42, y=18.25, label = "Abaixo do Peso")

imc_idade_modalidade_mulheres

# primeiro brasileiro
primeiro_brasileiro <- dados_agrupados_brasil |> 
  slice_min(order_by = ano) |> 
  select(nome, idade, modalidade, ano, cidade, evento) |> 
  arrange(ano)

primeiro_brasileiro

# primeiras mulheres
primeiras <- dados_agrupados_brasil |>
  group_by(ano, sexo) |>
  filter(sexo == "Female") |>
  select(nome, sexo, idade, modalidade, ano, cidade) |> 
  arrange(ano) |>
  head(5)
primeiras

# Como descobrir o ano em que o Brasil ganhou mais medalhas por modalidade------------------------------------
medalhas_modalidade <- medalhistas_brasil |>
  group_by(ano) |>
  distinct(evento, medalha) |>
  summarise(quantidade = n()) |>
  arrange(desc(ano))

#  1  2021         21     21
#  2  2016         19     19  
#  3  2012         17     17
#  4  2008         16     17      ** Vide nota abaixo
#  5  2004         10     10
#  6  2000         12     12 
#  7  1996         15     15
#  8  1992          3     3
#  9  1988          6     6
# 10  1984          8     8
# 11  1980          4     4
# 12  1976          2     2
# 13  1972          2     2
# 14  1968          3     3
# 15  1964          1     1
# 16  1960          2     2
# 17  1956          1     1
# 18  1952          3     3
# 19  1948          1     1
# 20  1920          3     3

# Nota: medalha de bronze herdado por conta do dopping da equipe da Jamaica 4x100 masculina de atletismo
# equipe brasileira composta por Vicente Lenilson, Bruno Lins, José Carlos Moreira e Sandro Viana pelo dopping
# do atleta Nesta Carter.
# fonte: https://www.uol.com.br/esporte/reportagens-especiais/medalhas-sem-emocao-os-brasileiros-que-demoraram-
# anos-para-serem-reconhecidos-como-medalhistas-olimpicos/#cover

medalhistas_brasil |>
  group_by(ano) |>
  distinct(evento, medalha) |>
  summarise(quantidade = n()) |>
  arrange(desc(ano)) |> 
    kable_styling(
      bootstrap_options = "basic",
      full_width = NULL,
      row_label_position = "l"
      )

# Gráfico medalhas ao longo dos jogos-------------------------------------------------------------------------
# usar o gráfico no relatório e destacar a evolução ocorrida nos seis últimos quadriênios
grafico_medalhas_modalidade <- medalhas_modalidade |>
  ggplot(aes(x = ano, y = quantidade, label = ano)) +
  geom_line(
    size = 1,
    colour = "#54C6EB"
  ) +
  geom_point(
    shape = "circle",
    colour = "#2D3047"
  )+
  geom_text(aes(label = quantidade),
            hjust = 1, vjust = -0.7
  ) +
  geom_segment(
    aes(x = 2000,
        y= 20,
        xend = 2006,
        yend = 17.5),
    size = 0.3,
    arrow = arrow(length = unit(0.3, "cm")), colour="#508AA8",
    inherit.aes=FALSE, lwd=2)+
  geom_segment(
    aes(x = 1985,
        y= 20,
        xend = 2000,
        yend = 20),
    size = 0.5,
    colour="#508AA8",
    inherit.aes=FALSE, lwd=2)+
  annotate("text", x=1958, y=20, label = "Selecione acima a aba 'tabela de medalhas' para informação
           sobre este ponto")+
  scale_x_continuous(breaks = seq(1896, 2020,4)) +
  scale_y_continuous(breaks = seq(0,22,1)) +
  labs(title = "Quantidade de Medalhas por Edição dos Jogos", subtitle = "Total de Medalhas") +
  xlab("Ano") +
  ylab("Quantidade")+
  theme_bw()+
  theme(legend.position = "none",
        axis.text.x=element_text(size=10),
        axis.text.y=element_text(size=10))

grafico_medalhas_modalidade

# Gráfico anterior diferenciado por sexo
medalhas_masculino_feminino <- medalhistas_brasil |>
  group_by(sexo) |>
  distinct(evento, modalidade, sexo, ano, medalha) |>
  summarise(ano) |>
  ungroup() |>
  group_by(sexo, ano) |>
  summarise(qtde = n()) |>
  ggplot(aes(x = ano, y = qtde, color = sexo)) +
  geom_line(
    size = 1
  ) +
  geom_point(
    shape = "circle",
    color = "#56638A"
  ) +
  geom_text(aes(label = qtde),
            hjust = 1, vjust = -0.7,
            colour = "black"
  ) +
  labs(
    title = "Quantidade de Medalhas por Edição dos Jogos ",
    subtitle = "Medalhas Separadas por Sexo", x = "Ano",
    y = "Quantidade"
  ) +
  scale_x_continuous(breaks = seq(1896, 2020, 4)) +
  scale_y_continuous(breaks = seq(0, 22, 1)) +
  scale_color_manual(values = c("#9DC0BC", "#BBB6DF")) +
  theme_bw()+
  theme(legend.position = "bottom",
        axis.text.x=element_text(size=10),
        axis.text.y=element_text(size=10))

medalhas_masculino_feminino

# Quantidade de medalhas por modalidade-----------------------------------------------------------------------
# usar esta consulta para permitir ao usuário através de um select input do shiny recuperar os dados de 
# quantidade de medalhas por modalidade em qualquer edição dos jogos olímpicos


# nome dos atletas--------------------------------------------------------------------------------------------
# permite recuperar a lista de todos os medalhistas, medalhas e modalidade de uma edição olímpica (no caso 2016)
medalhistas_2016 <- medalhistas_brasil |> 
  group_by(nome) |> 
  filter(ano == 2016) |> 
  summarize(nome, medalha, modalidade)

# medalhas por edicao e medalhas
# função que recebe como parâmetro o ano da edição olímpica e retorno o total de medalhas conquistadas
qtde_medalhas_edicao <- function(edicao) {
  qtde_medalhas <- medalhistas_brasil |> 
    group_by(medalha) |>
    filter(ano == edicao) |>
    distinct(evento, modalidade, medalha) |> 
    nrow()
  return(qtde_medalhas)
}

# atletas por edição olímpica (todos os atletas e não apenas os medalhistas)
# função que recebe como parâmetro o ano da edição olímpica e retorna o total de atletas na edição
qtde_atletas_edicao <- function(edicao) {               
  qtde_atletas <- dados_agrupados_brasil |> 
    group_by(nome) |>
    filter(ano == edicao) |> 
    nrow()
  return(qtde_atletas)                                   
}

# obter os dados para cada edição olímpica
qtde_atletas_vetor <- c()                         # cria o vetor total de atletas de uma edição
qtde_medalhas_vetor <- c()                        # cria o vetor total de medalhas de uma edição
cidades <- c()                                    # cria o vetor com as cidades olímpicas

# laço que alimenta os vetores com os elementos (atletas e medalhas) para cada edição olímpica
for (i in seq(from = 1896, to = 2020, by = 4)){
  qtde_atletas_para_vetor <- qtde_atletas_edicao(i)
  qtde_atletas_vetor <- append(qtde_atletas_vetor, qtde_atletas_para_vetor)
  qtde_medalhas_para_vetor <- qtde_medalhas_edicao(i)
  qtde_medalhas_vetor <- append(qtde_medalhas_para_vetor, qtde_medalhas_vetor)
}

# montar a base com os resultados obtidos
# temos que inverter a ordem dos elementos do vetor com os dados da medalhas por ano
qtde_medalhas_vetor <- rev(qtde_medalhas_vetor)

# corrigir o valor de medalhas da olimpíada de 2008, de 16 medalhas para 17 medalhas
qtde_medalhas_vetor[29] <- 17

# vetor dos anos dos eventos
anos_vetor <- seq(from = 1896, to = 2020, by = 4)

# o primeiro brasileiro na olimpíada
primeiro_brasileiro <- dados_agrupados_brasil |> 
  slice_min(order_by = ano) |> 
  select(nome, idade, modalidade, ano, cidade) |> 
  arrange(ano)

primeiro_brasileiro

















