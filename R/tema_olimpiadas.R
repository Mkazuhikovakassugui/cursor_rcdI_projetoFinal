#--------------------------------------------------------------------------------------------------#
#                                       Funções do Projeto
#--------------------------------------------------------------------------------------------------#


# 1) Função para o tema dos gráficos ----------------------------------------------------------


tema_olimpiadas <- function(){
  theme(
    text = element_text(
      family = "Rio 2016"
    ),                                              
    title = element_text(
      size = 15,
      color = 
    ),                                               
    legend.position = "none",
    axis.text.x=element_text(
      size=11
    ),
    axis.text.y=element_text(
      size=11
    ),
    plot.title = element_text(
      hjust = 0.5,
      face = "bold",
      color = "#000000",
      size = 22
    ),
    panel.background = element_rect(
      fill = "#C8E5F9"
    ),
    plot.background = element_rect(
      fill = "#92A1B5"
    )
  )
}


# 2) Função para definição das cores olimpíadas -----------------------------------------------

cores_olimpiadas <- function(variables) {
  scale_fill_manual(
    values = c(
      "#0085C8",
      "#F3C200",
      "#009E3C",
      "#E00024",
      "#000000",
      "#2218DE"
    )
  )
}
