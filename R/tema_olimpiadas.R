#--------------------------------------------------------------------------------------------------#
#                                       Funções do Projeto
#--------------------------------------------------------------------------------------------------#


# 1) Função para o tema dos gráficos ----------------------------------------------------------


tema_olimpiadas <- function() {
  theme(
    text = element_text(
      family = ""
    ),                                              
    title = element_text(
      size = 15
    ),                                               
    legend.position = "none",
    axis.text.x=element_text(
      size=11
    ),
    axis.text.y=element_text(
      size=11
    ),
    axis.title.x = element_text(
      size = 17,
      color = "#000000",
      hjust = 0.42
    ),
    axis.title.y = element_text(
      size = 17,
      color = "#000000",
      vjust = 0.5
    ),
    plot.title = element_text(
      hjust = 0.42,
      face = "bold",
      color = "#000000",
      size = 22
    ),
    plot.subtitle = element_text(
      hjust = 0
    ),
    panel.background = element_rect(
      fill = "#F3F5F7"
    ),
    plot.background = element_rect(
      fill = "#74DCFB",
      inherit.blank = TRUE
    )
  )
}


# 2) Função para definição das cores olimpíadas -----------------------------------------------

cores_olimpiadas <- function(variables) {
  scale_fill_manual(
    values = c(
      "#0082C9",
      "#FCB32D",
      "#00A750",
      "#EE304D",
      "#000000",
      "#2218DE"
    )
  )
}
