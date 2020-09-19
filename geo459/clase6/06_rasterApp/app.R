library(shiny)
library(tidyverse)
library(reshape2)
library(sf)
library(raster)
library(rasterVis)

ui <- fluidPage( ## armemos nuestra appa simple
  'Explorador de temperatura de Chile' %>% strong() %>% h4(),
  uiOutput(outputId = 'regiones'), ## crearemos el input en el server
  plotOutput('rasPlot'),
  plotOutput('histo',width = '1000px') %>% column(width = 4,offset = 3)
  
)

server <- function(input, output, session) {
  ##load data
  temp.data <- stack('data/chile_temp.tif')
  chile <- st_read('data/Regional.shp')
  names(temp.data) <- c('min','mean','max')

  #render uiOutput
  regionInput <- chile$Region %>% unique()
  
  output$regiones <- renderUI({
      selectInput(inputId = "region",label = "Seleccione una Región:",choices = regionInput[1:16],multiple = F )
    })
  #render rasPlot
  output$rasPlot <- renderPlot({
    req(input$region)
    shp.crop <- chile %>% filter(Region == input$region)
    r.plot <- temp.data %>% crop(shp.crop) %>% mask(shp.crop)
    levelplot(r.plot/10)
  })
  ##render histo
  output$histo <- renderPlot({
    req(input$region)
    shp.crop <- chile %>% filter(Region == input$region)
    r.df <- temp.data %>% crop(shp.crop) %>% mask(shp.crop) %>% 
      as('SpatialPixelsDataFrame') %>% as.data.frame()
    names(r.df) <- c('min','mean','max','x','y')
    r.dfPlot <- r.df %>% melt(id=c('x','y'))
    
    ggplot(data = r.dfPlot, aes(x=value/10, fill=variable)) +
      geom_histogram(alpha=0.6, position = 'identity',bins = 20) + theme_bw()+ labs(fill="")+
      xlab('Temperatura') + ylab("Frecuencia")
  })
    
}

shinyApp(ui, server)