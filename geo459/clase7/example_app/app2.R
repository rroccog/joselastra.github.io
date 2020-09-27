library(shiny)
library(tidyverse)

ui <- fluidPage(
  selectInput('campo1',label = 'Seleccione primer campo',choices = c("Temperatura","Salinidad",
                                                                     "Sigma" ,"Oxigeno_disuelto")),
  selectInput('campo2',label = 'Seleccione primer campo',choices = c("Temperatura","Salinidad",
                                                                     "Sigma" ,"Oxigeno_disuelto")),
  actionButton('submit','Mostrar Correlación'),hr(),
  textOutput('corr'),
  plotOutput('plot')
)

server <- function(input, output, session) {
 #load  data
  data.CTD <- read.csv('CTD_data.csv',sep = ';')
  # reactiveValues
  text_corr <- reactiveValues(
    text = "Seleccione dos variables diferentes",
    correlacion = " y oprima 'Mostrar Correlación' "
  )
  #reactive expression
  data1 <- reactive(
    cor(data.CTD[,input$campo1],data.CTD[,input$campo2])
  )
  #observe actualiza el valor dentro de reactive values
  observeEvent(input$submit,
               {
                 text_corr$text <- 'Valor de correlación'
                 text_corr$correlacion <- data1()
               })
  # salida corr
  output$corr <- renderText({ c(text_corr$text,text_corr$correlacion) })
  #event reactive expression
  plot1 <- reactive({
    ggplot(data = data.CTD, aes_string(x = input$campo1, y = input$campo2)) + geom_point() + 
      geom_smooth(method = 'lm') + theme_bw()
  }) 
  #salida plot
  output$plot <- renderPlot({ plot1() })

}

shinyApp(ui, server)