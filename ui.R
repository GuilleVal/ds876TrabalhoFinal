fluidPage(
  titlePanel("Dados sobre a pandemia"),
  fluidRow(
    fluidRow(
      column(3, wellPanel(
        selectInput("dataset", "Tipo de gráfico:",
                    choices = c("Linha", "Barra")),
        textOutput("erroData")
      )),
    
      conditionalPanel(
        condition = "input.dataset == 'Linha'", 
        column(3, wellPanel(
          selectInput("regiao", "Selecione uma região:",
                      choices = optionsRegioes),
          selectInput("paises", "Selecione um país:",
                      ""),
          selectInput("dado", "Selecione um dado a ser analisado:",
                      choices = optionsLineGraph),
        )),
        
        column(3, wellPanel(
          selectInput("dataInicial", "Selecione a data inicial do período:",
                      choices = optionsDates),
          selectInput("dataFinal", "Selecione a data final do período:",
                      choices = optionsDates),
        )),
        
        column(3, wellPanel(
          actionButton("plotGraph", "Gerar gráfico")
        )),
      ),
      
      conditionalPanel(
        condition = "input.dataset == 'Barra'",
        column(6, wellPanel(
          selectInput("dadoBar", "Selecione um dado a ser analisado:",
                      choices = optionsBarGraph),
          actionButton("plotGraphBar", "Gerar gráfico")
        )),
      )
    ),
    
    fluidRow(
      column(width = 6, 
             plotOutput("plot", height = 300, width = 800,
                        brush = brushOpts(
                          id = "plot_brush",
                           resetOnNew = TRUE
                        ))
      )  
    ),
    fluidRow(
      column(width = 6, 
             plotOutput("plot3", height = 300, width = 800)
      )  
    ),
  )
)