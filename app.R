library(ggplot2)
library(Cairo)   # For nicer ggplot2 output when deployed on Linux

ui <- fluidPage(
    fluidRow(
        column(width = 12, class = "well",
               h4("Mortes acidentas nos EUA de 1973 a 1978"),
               fluidRow(
                   column(width = 6,
                          plotOutput("plot2", height = 300, width = 830,
                                     brush = brushOpts(
                                         id = "plot2_brush",
                                         resetOnNew = TRUE
                                     )
                          )
                   ),
                   
               )
        ),
        column(width = 12, class = "well",
               h4("Mortes acidentas nos EUA do período selecionado"),
               fluidRow(
                   column(width = 6, 
                          plotOutput("plot3", height = 300, width = )
                   )  
               )
       )
    )
)



shinyApp(ui, server)