library(shiny)
library(leaflet)
library(tidyverse)
library(sf)
library(shinybusy)


source('proxyFun.R')

ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput('map', width = "100%", height = "100%"),
  absolutePanel(id="controls",
                style="z-index:500;", top = 90, left = "auto", right = 20, 
                bottom = "auto",
                width = 400, height ="auto",
                class = "panel panel-default",
                selectInput(inputId = 'campo',label = 'Seleccione variable a visualizar',
                            choices = list('Total personas' = 'PERSONAS',
                                           'Densidad (per/ha)'='DENSIDAD',
                                           'Viviendas'='TOTAL_VIVI'))
                )
)

server <- function(input, output, session) {
  
  #Load data
  shp <- read_sf('data/viña_del_mar.gpkg') %>% st_transform(4326)
  
  ## rendering base map
  output$map <- renderLeaflet({

    leaflet() %>% addProviderTiles(providers$CartoDB.DarkMatter) %>% 
      fitBounds(lng1 =-71.586 ,lat1 =-33.105 ,lng2 =-71.45 ,lat2 = -32.944) %>%
      addLayersControl(overlayGroups = 'Manzanas',position = 'topleft')
  })
  
  observe({
    #preparando paletas
    dominio <- shp[,input$campo[[1]]] %>% unlist() %>% as.numeric()
    pal <- colorBin(palette = "viridis",domain = dominio)
  #crando objeto proxy  
  proxyMap <- leafletProxy('map') %>% clearShapes()
  
  proxyMapApp(shp.object = shp, proxyMap.object = proxyMap, campo = input$campo[[1]], paleta = pal)
  })

}

shinyApp(ui, server)