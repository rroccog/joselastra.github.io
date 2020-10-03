library(shiny)
library(leaflet)
library(tidyverse)
library(sf)
library(shinybusy)

ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput('map', width = "100%", height = "100%")
)

server <- function(input, output, session) {
  
  #Load data
  shp <- read_sf('data/viña_del_mar.shp') %>% st_transform(4326)
  ## rendering base map
  output$map <- renderLeaflet({
    leaflet() %>% addProviderTiles(providers$CartoDB.DarkMatter) 
  })
}

shinyApp(ui, server)