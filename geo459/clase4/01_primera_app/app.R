library(shiny)
library(readxl)
library(tidyverse)

## User Interface
ui <- fluidPage(sidebarPanel(
                  h4(strong('Panel de opciones')),
                  selectInput("select", label = h4("Seleccione una estación"), 
                              choices = list("Nehuentué" = "Nehuentué", "Queule" = 'Queule',
                                             "Puerto Saavedra" = "Puerto Saavedra"), 
                              selected = 'Nehuentué'),
                  radioButtons("millas", label = h4("Distancia de la costa"),
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
                  h4('Panel principal'),
                  column(plotOutput(outputId = 'perfil',height = 500),width = 3, offset = 2),br(),
                  fluidRow(tableOutput(outputId = 'resumen'))
                )
)

## Server
server <- function(input, output, session) {
  #reading data
  tabla <- read_excel('data/Araucania2018_CTD_C1.xls') %>% as.data.frame()
  
  #rendering outputs
  #Tabla
  output$resumen <- renderTable({
    df.resumen <- tabla %>% 
      group_by(Estaciones) %>%
      summarise(median = median(`Temperatura (°C)`), n = n())
    df.resumen
  })
  
  #plot
  output$perfil <- renderPlot({
    #making plot data
    data.filter <- paste(input$select,input$millas,sep = '')
    plot.data <- tabla %>% filter(Estaciones==data.filter) %>% 
      mutate(prof = `Profundidad (m)`*-1) %>% as.data.frame()
    plot.data$variable <- plot.data[, as.vector(input$campo[[1]])] 
    #plot
    par(mar = c(4, 4, 2, 2))
  
    plot(plot.data[,'variable'], plot.data[,'prof'], type='l',xlab = input$campo, ylab='Prof. (m)',
         main = paste(input$select, ':', input$campo[[1]]))
  })
}
#compiling app
shinyApp(ui, server)