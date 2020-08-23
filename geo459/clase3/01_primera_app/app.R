library(shiny)
library(readxl)
library(tidyverse)

## User Interface
ui <- fluidPage(sidebarPanel(
  h4('Panel de opciones')
                             ),
                mainPanel(
                  h4('Panel principal'),
                  plotOutput(outputId = 'perfil'),
                  tableOutput(outputId = 'resumen')
                )
)

## Server
server <- function(input, output, session) {
  #reading data
  tabla <- read_excel('data/Araucania2018_CTD_C1.xls')
  
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
    plot.data <- tabla %>% filter(Estaciones=='Nehuentué2')
    ggplot(data = plot.data, aes(y=`Profundidad (m)`*-1,x = `Temperatura (°C)`)) + 
      geom_line() + theme_bw()
  })
}
#compiling app
shinyApp(ui, server)