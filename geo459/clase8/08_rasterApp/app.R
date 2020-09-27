library(shiny)
library(tidyverse)
library(reshape2)
library(leaflet)
library(sf)
library(raster)
library(rasterVis)
library(shinybusy)

##nuestra interfaz de usuario
ui <- fluidPage(
  add_loading_state(".shiny-plot-output",text = 'Cargando gráficos...', svgColor = 'steelblue'),
  ##titulo y cuerpo de UI
  "Explorador de temperatura Chile" %>% strong() %>% h4(),
  uiOutput(outputId = 'regiones'),## crearemos el input en el server
  checkboxGroupInput("banda", label = "Seleccione información", 
                     choices = list("Mínimo" = 1, "Promedio" = 2, "Máximo" = 3),
                     selected = 2),
  leafletOutput('map', width = 1500) ,
  plotOutput('histo',width = '1000px') %>% column(width = 4,offset = 2)
)

server <- function(input, output, session) {
  ##load data
  temp.data <- stack('data/chile_temp.tif')
  chile <- st_read('data/Regional.shp')
  names(temp.data) <- c('min','mean','max')
  ###############
  regionInput <- chile$Region %>% unique() #nombres de las regiones
  #render de selección de regiones
  output$regiones <- renderUI({
    selectInput(inputId = "region",label = "Seleccione una Región:",choices = regionInput[1:16],multiple = F)
  })
  
  #reactive data
  data.select <- eventReactive(input$region,{
    req(input$region)
    shp.crop <- chile %>% filter(Region == input$region) #selección por atributos
    r.plot <- temp.data %>% crop(shp.crop) %>% mask(shp.crop) # crop and mask
    r.plot
  })

  ##render histo
  output$histo <- renderPlot({
    #req(data.select())
    r.df <- data.select() %>% 
      as('SpatialPixelsDataFrame') %>% as.data.frame() # pasando a data frame
    names(r.df) <- c('min','mean','max','x','y') #cambiar nombres a columnas
    r.dfPlot <- r.df %>% melt(id=c('x','y')) # arreglar datos para ploteo de histograma
    
    ggplot(data = r.dfPlot, aes(x=value/10, fill=variable)) +
      geom_histogram(alpha=0.6, position = 'identity',bins = 20) + theme_bw()+ labs(fill="")+
      xlab('Temperatura') + ylab("Frecuencia")
  })
 
  output$map <- renderLeaflet({
   leaflet() %>% addTiles() 
  })
  
  observe({
    req(data.select())
    nl <- input$banda[[1]]
    r <-  data.select()[[nl]]
    
    leafletProxy('map') %>% clearImages() %>% addRasterImage(x = r)
  })
  
}

shinyApp(ui, server)












