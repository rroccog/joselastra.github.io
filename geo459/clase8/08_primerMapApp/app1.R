library(shiny)
library(leaflet)
library(tidyverse)
library(sf)
library(shinybusy)

ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput('map', width = "100%", height = "100%"),
  absolutePanel(id="controls",
                style="z-index:500;", top = 90, left = "auto", right = 20, 
                bottom = "auto",
                width = 400, height ="auto",
                class = "panel panel-default",
                selectInput(inputId = 'campo',label = 'Seleccione campo a visualizar',
                            choices = c(1,2,3)),
                actionButton('remap',label = 'Mostrar información seleccionada')
  )
)

server <- function(input, output, session) {
  
  #Load data
  shp <- read_sf('data/viña_del_mar.shp') %>% st_transform(4326)
  ## rendering base map
  output$map <- renderLeaflet({
    leaflet() %>% addProviderTiles(providers$CartoDB.DarkMatter) %>% 
      addPolygons(data = shp,group = 'Manzanas') %>%
      addLayersControl(overlayGroups = 'Manzanas',position = 'topleft')
  })
}

shinyApp(ui, server)