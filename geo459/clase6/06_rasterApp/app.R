library(shiny)
library(tidyverse)
library(reshape2)
library(sf)
library(raster)
library(rasterVis)
library(shinybusy)

ui <- fluidPage( ## armemos nuestra appa simple
  add_loading_state(".shiny-plot-output", timeout = 5000,messageFontSize = 50 ,text = "Cargando gráfico...",
    svgColor = "steelblue"),
  'Explorador de temperatura de Chile' %>% strong() %>% h4(),
  uiOutput(outputId = 'regiones'),## crearemos el input en el server
  plotOutput('rasPlot'),
  plotOutput('histo',width = '1000px') %>% column(width = 4,offset = 3)
)

server <- function(input, output, session) {
  ##load data
  temp.data <- stack('data/chile_temp.tif')
  chile <- read_sf('data/Regional.shp')
  names(temp.data) <- c('min','mean','max')

  #render uiOutput
  regionInput <- chile$Region %>% unique()
  
  output$regiones <- renderUI({
      selectInput(inputId = "region",label = "Seleccione una Región:",choices = regionInput[1:16],multiple = F )
    })
  
  #reactive data
  data.select <- function (){
    req(input$region)
    shp.crop <- chile %>% filter(Region == input$region)
    r.plot <- temp.data %>% crop(shp.crop) %>% mask(shp.crop)
    r.plot
  }
 
  #render rasPlot
  output$rasPlot <- renderPlot({
    req(data.select())
    levelplot(data.select()/10)
  })
  
  ##render histo
  output$histo <- renderPlot({
    req(data.select())
    r.df <- data.select() %>% as('SpatialPixelsDataFrame') %>% as.data.frame()
    names(r.df) <- c('min','mean','max','x','y')
    r.dfPlot <- r.df %>% melt(id=c('x','y'))
    
    ggplot(data = r.dfPlot, aes(x=value/10, fill=variable)) +
      geom_histogram(alpha=0.6, position = 'identity',bins = 20) + theme_bw()+ labs(fill="")+
      xlab('Temperatura') + ylab("Frecuencia")
  })
  
}

shinyApp(ui, server)