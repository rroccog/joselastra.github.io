library(shiny)
library(readxl)
library(tidyverse)

ui <- fluidPage(sidebarPanel(
  h1('Panel de opciones'),
  selectInput('select',label = h4("Seleccione una estación"),
              choices = list('Nehuentué'='Nehuentué', 'Queule'='Queule',
                             'Puerto Saavedra'= 'Puerto Saavedra'),
              selected = 'Nehuentué'),
  radioButtons('millas', label = 'Distancia de la costa',
                choices = list("2 millas" = 2, "5 millas" = 5, "10 millas" = 10),
               selected = 2),
  selectInput("campo", label = h4("Seleccione una variable"),
              choices = list("Temperatura" = "Temperatura (°C)",
                             "Sigma- t"= 'Sigma T (Kg/m3)',
                             "Salinidad" = "Salinidad (UPS)",
                             "Oxígeno" =  "Oxigeno (ml/L)"),
              selected = 'Temperatura')
),
mainPanel(
  h1('Panel principal'),
  column(plotOutput(outputId = 'perfil',height = 500),width = 3, offset = 2),br(),
  tableOutput(outputId = 'resumen')
)
  
)

server <- function(input, output, session) {
  #reading data
  tabla <- read_excel('data/Araucania2018_CTD_C1.xls')
  
  #Tabla
  output$resumen <- renderTable({
  
    df.resumen <- tabla %>% group_by(Estaciones) %>%
      summarise(median = median(`Temperatura (°C)`), n= n())
    df.resumen
  })
  
  
  #plot
  output$perfil <- renderPlot({
    #making data plot
    data.filter <- paste(input$select,input$millas, sep='')
    # table plot
    plot.data <- tabla %>% filter(Estaciones == data.filter) %>% 
      mutate( prof = `Profundidad (m)`*-1) %>% as.data.frame()
    plot.data$variable <- plot.data[, input$campo]
    
    #plot
    par(mar = c(4, 4, 2, 2)) #only for margins control
    plot(plot.data[,'variable'], plot.data[,'prof'], type='l',
         xlab = 'Temperatura °C', ylab='Prof. (m)', main = paste(input$select,':', input$campo))
  })
  
}

shinyApp(ui, server)