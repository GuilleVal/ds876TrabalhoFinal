library(shiny)
library(ggplot2)
library(dplyr)

#####################################--VARIAVEIS NECESSÁRIAS--####################################################################
optionsLineGraph <- c("New_cases", "Cumulative_cases", "New_deaths", "Cumulative_deaths")
optionsBarGraph <- c("TOTAL_VACCINATIONS", "Cases...cumulative.total", "Deaths...cumulative.total", 
                     "Cases...newly.reported.in.last.24.hours", "Deaths...newly.reported.in.last.24.hours")
options2 <- NULL
optionsRegioesPonto <- NULL
optionsPaises <- NULL
optionsDados <- NULL
lineOptions1 <- NULL
lineOptions2 <- NULL
lineOptions3 <- NULL
linePlot <- NULL
optionsDates <- NULL
dadoBar <- NULL
erroData <- NULL
#################################################################################################################################

############################################################--IMPORTANDO DATA SET COMBINADO--###################################
load(file = "./merge_data12_df.RData")
names(merge_data12_df)

datasetCombinado <- merge_data12_df

dim(datasetCombinado)
#################################################################################################################################


#############################################################--CRIANDO DATA SET COM FILTRO DE PAIS--###################################
data1_df <- read.table(file = "C:/Users/guilh/Documents/ds876TrabalhoFinal/WHO-COVID-19-global-data.csv", sep=",", quote="\"",
                       header = T, stringsAsFactors = F)
#names(data1_df)
vars <- sapply(1:ncol(data1_df), function(i){
  class(data1_df[,i])
})

filt_data3_df <- data1_df[complete.cases(data1_df),]
x_df <- filt_data3_df[,c("ï..Date_reported","Country","New_cases", "Cumulative_cases", "New_deaths", "Cumulative_deaths")]
#######################################################################################################################################

filtrarRegioes <- function(){
    filterRegion <- data1_df[complete.cases(data1_df),]
    
    #PEGA APENAS A COLUNA "COUNTRY"
    whoRegion <- filterRegion[,c("WHO_region")]
    
    #RETIRA OS VALORES REPETIDOS
    whoRegion <- unique(whoRegion)
    
    #CRIAR UMA LISTA DE REGIÕES
    optionsRegioesPonto <- c(whoRegion)
    
    return (optionsRegioesPonto)
}

filtrarPaises <- function(input){
  
    #ZERA A LISTA DE PAISES
    optionsPaises <- NULL
    
    filterPais <- data1_df[complete.cases(data1_df),]
    
    #FILTRA OS PAISES COM BASE NA REGIÃO SELECIONADA
    paises <- filter(filterPais, WHO_region == input$regiao)
    
    #PEGA APENAS A COLUNA "COUNTRY"
    paises <- paises[,c("Country")]
    
    #RETIRA OS VALORES REPETIDOS
    paises <- unique(paises)
    
    #CRIA UMA LISTA DE PAISES
    optionsPaises <- c(paises)
    
    return (optionsPaises)
}

filtrarDatas <- function(input){
  filterDatas <- data1_df[complete.cases(data1_df),]

  #CRIAR UM DATA FRAME QUE POSSUI APENAS A COLUNA 'ï..Date_reported'
  datas <- filterDatas[,c("ï..Date_reported")]
  
  #RETIRA OS VALORES REPETIDOS
  datas <- unique(datas)
  
  #CRIA UMA LISTA DE DATAS
  optionsDates <- c(datas)
  
  return (optionsDates)
}

gerarGraficoLinha <- function(input, output){
  #VERIFICA SE A DATA FINAL É MENOR QUE A DATA INICIAL
  if(input$dataFinal < input$dataInicial){
    textoErro <- "Data inicial maior que data final"
    
    output$erroData <- renderText({
      textoErro
    })
  }else{
    #OBSERVA O BOTÃO QUE GERA O GRÁFICO DE LINHA
    observeEvent(input$plotGraph, {
      #ZERA O TEXTO DE AVISO DE ERRO DE DATA
      textoErro <- NULL
      
      output$erroData <- renderText({
        textoErro
      })
      output$plot <- renderPlot({
        #VERIFICA QUAL DADO O USUÁRIO ESCOLHEU, PARA GERAR O GRÁFICO COM BASE NESSE DADO
        switch (input$dado,
          "New_cases" = ggplot(data = subset(x_df, `ï..Date_reported` > as.Date(input$dataInicial)
                                             & `ï..Date_reported` < as.Date(input$dataFinal)
                                             & `Country` == input$paises),
                               aes(`ï..Date_reported`, `New_cases`, group = 1)) + geom_line() + labs(x = "Datas", y = "Casos novos"),
          
          "Cumulative_cases" = ggplot(data = subset(x_df, `ï..Date_reported` > as.Date(input$dataInicial)
                                                    & `ï..Date_reported` < as.Date(input$dataFinal)
                                                    & `Country` == input$paises),
                                      aes(`ï..Date_reported`, `Cumulative_cases`, group = 1)) + geom_line() + labs(x = "Datas", y = "Casos"),
          
          "New_deaths" = ggplot(data = subset(x_df, `ï..Date_reported` > as.Date(input$dataInicial)
                                                    & `ï..Date_reported` < as.Date(input$dataFinal)
                                                    & `Country` == input$paises),
                                      aes(`ï..Date_reported`, `New_deaths`, group = 1)) + geom_line() + labs(x = "Datas", y = "Novas mortes"),
          
          "Cumulative_deaths" = ggplot(data = subset(x_df, `ï..Date_reported` > as.Date(input$dataInicial)
                                                     & `ï..Date_reported` < as.Date(input$dataFinal)
                                                     & `Country` == input$paises),
                                       aes(`ï..Date_reported`, `Cumulative_deaths`, group = 1)) + geom_line() + labs(x = "Datas", y = "Mortes"),
        )
      })
    })
  }
}

gerarGraficoBarra <- function(input, output){
  #OBSERVA O BOTÃO QUE GERA O GRÁFICO DE barra
  observeEvent(input$plotGraphBar, {
    output$plot <- renderPlot({
      #VERIFICA QUAL DADO O USUÁRIO ESCOLHEU, PARA GERAR O GRÁFICO COM BASE NESSE DADO
      switch (input$dadoBar,
        "TOTAL_VACCINATIONS" = ggplot(datasetCombinado, aes(`WHO_REGION`, `TOTAL_VACCINATIONS`)) + geom_bar(stat = "identity") + labs(x = "Região", y = "Total de vacinações"),
        
        "Cases...cumulative.total" = ggplot(datasetCombinado, aes(`WHO_REGION`, `Cases...cumulative.total`)) + geom_bar(stat = "identity") + labs(x = "Região", y = "Casos totais"),
        
        "Deaths...cumulative.total" = ggplot(datasetCombinado, aes(`WHO_REGION`, `Deaths...cumulative.total`)) + geom_bar(stat = "identity") + labs(x = "Região", y = "Mortes totais"),
        
        "Cases...newly.reported.in.last.24.hours" = ggplot(datasetCombinado, aes(`WHO_REGION`, `Cases...newly.reported.in.last.24.hours`)) + geom_bar(stat = "identity") + labs(x = "Região", y = "Casos reportados nas ultimas 24h"),
        
        "Deaths...newly.reported.in.last.24.hours" = ggplot(datasetCombinado, aes(`WHO_REGION`, `Deaths...newly.reported.in.last.24.hours`)) + geom_bar(stat = "identity") + labs(x = "Região", y = "Mortes reportadas nas ultimas 24h"),
      )
    })
  })
}


function(input, output, session) {
  #ATUALIZA SELECT INPUT DAS REGIÕES
  updateSelectInput(session, "regiao", choices = filtrarRegioes())
  
  #ZERA AS VARIAVEIS DOS GRÁFICOS
  observeEvent(input$dataset, {
    output$plot3 <- NULL
    output$plot <- NULL
    ranges2$x <- NULL
    ranges2$y <- NULL
  })
  
  #OBSERVA O BOTÃO QUE GERA O GRÁFICO DE LINHA
  observeEvent(input$plotGraph, {
    gerarGraficoLinha(input, output)
  })
  
  #OBSERVA O BOTÃO QUE GERA O GRÁFICO DE BARRA
  observeEvent(input$plotGraphBar, {
    gerarGraficoBarra(input, output)
  })
  
  #ATUALIZA OS PAISES AO SELECIONAR UMA REGIAO
  observeEvent(input$regiao, {
    optionsPaises <- updateSelectInput(session, "paises", choices = filtrarPaises(input))
  })
  
  #ATUALIZA AS DATAS INICIAIS AO SELECIONAR UMA REGIAO
  observeEvent(input$regiao, {
    optionsDates <- updateSelectizeInput(session, "dataInicial", choices = filtrarDatas(input))
  })
  
  #ATUALIZA AS DATAS FINAIS AO SELECIONAR UMA REGIAO
  observeEvent(input$regiao, {
    optionsDates <- updateSelectizeInput(session, "dataFinal", choices = filtrarDatas(input))
  })
  
  # Linked plots (middle and right)
  ranges2 <- reactiveValues(x = NULL, y = NULL)

  output$plot3 <- NULL
  
  #FUNÇÃO QUE TRATA DE CRIAR O SEGUNDO GRÁFICO, AO SELECIONAR UMA DETERMINADA REGIAO DO PRIMEIRO GRÁFICO
  observe({
    brush <- input$plot_brush
    #VERIFICA SE EXISTE UMA REGIÃO SELECIONADA
    if (!is.null(brush)) {
      #LIMITA AS VARIAVEIS
      ranges2$x <- c(brush$xmin, brush$xmax)
      ranges2$y <- c(brush$ymin, brush$ymax)
        output$plot3 <- renderPlot({
          switch (input$dado,
                  "New_cases" = ggplot(data = subset(x_df, `ï..Date_reported` > as.Date(input$dataInicial) 
                                                     & `ï..Date_reported` < as.Date(input$dataFinal) 
                                                     & `Country` == input$paises), 
                                       aes(`ï..Date_reported`, `New_cases`, group = 1)) + geom_line() + labs(x = "Datas", y = "Casos novos")
                  + coord_cartesian(xlim = ranges2$x, ylim = ranges2$y, expand = FALSE),
                  
                  "Cumulative_cases" = ggplot(data = subset(x_df, `ï..Date_reported` > as.Date(input$dataInicial) 
                                                            & `ï..Date_reported` < as.Date(input$dataFinal) 
                                                            & `Country` == input$paises), 
                                              aes(`ï..Date_reported`, `Cumulative_cases`, group = 1)) + geom_line() + labs(x = "Datas", y = "Casos")
                  + coord_cartesian(xlim = ranges2$x, ylim = ranges2$y, expand = FALSE),
                  
                  "New_deaths" = ggplot(data = subset(x_df, `ï..Date_reported` > as.Date(input$dataInicial) 
                                                      & `ï..Date_reported` < as.Date(input$dataFinal) 
                                                      & `Country` == input$paises), 
                                        aes(`ï..Date_reported`, `New_deaths`, group = 1)) + geom_line() + labs(x = "Datas", y = "Novas mortes")
                  + coord_cartesian(xlim = ranges2$x, ylim = ranges2$y, expand = FALSE),
                  
                  "Cumulative_deaths" = ggplot(data = subset(x_df, `ï..Date_reported` > as.Date(input$dataInicial) 
                                                             & `ï..Date_reported` < as.Date(input$dataFinal) 
                                                             & `Country` == input$paises), 
                                               aes(`ï..Date_reported`, `Cumulative_deaths`, group = 1)) + geom_line() + labs(x = "Datas", y = "Mortes")
                  + coord_cartesian(xlim = ranges2$x, ylim = ranges2$y, expand = FALSE)
          )
        })
    } else {
      #ZERA AS VARIAVEIS DO SEGUNDO GRÁFICO
      ranges2$x <- NULL
      ranges2$y <- NULL
      output$plot3 <- NULL
    }
  })
  
}


