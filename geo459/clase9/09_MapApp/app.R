library(shiny)
library(leaflet)
library(htmltools)
library(tidyverse)
library(sf)
library(shinybusy)
# Funciones
source('dataPlot.R')
################

ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"), #esto hace la magia
  leafletOutput(outputId = 'map', width = "100%", height = "100%"),
  absolutePanel(id="controls",
                style="z-index:500;", top = 90, left = "auto", right = 20, 
                bottom = "auto",
                width = 400, height ="auto",
                class = "panel panel-default",
                selectInput(inputId = 'campo',label = 'Seleccione variable a visualizar',
                            choices = list('Total personas' = 'PERSONAS',
                                           'Densidad (per/ha)'='DENSIDAD',
                                           'Viviendas'='TOTAL_VIVI')),
                actionButton(inputId = 'plot',label = 'Presione para ver el histograma'),
                plotOutput(outputId = 'grafico')
  )
)

server <- function(input, output, session) {
  #Load data
  shp <- read_sf('data/viña_del_mar.gpkg') %>% st_transform(4326)
  shp$label_pop <- paste0('<strong>', 'Data:' , '</strong>','</br>', shp$PERSONAS) %>% 
    lapply(htmltools::HTML)
  ## rendering base map
  output$map <- renderLeaflet({
    leaflet() %>% addProviderTiles(providers$CartoDB.DarkMatter)%>% 
      fitBounds(lng1 =-71.586 ,lat1 =-33.105 ,lng2 =-71.45 ,lat2 = -32.944) %>%
      addLayersControl(overlayGroups = 'Manzanas',position = 'topleft')
  })
  #proxy map changes
  observe({
    #preparando paletas
    tabla <- shp[,] %>% as.data.frame() 
    dominio <- tabla[, input$campo[[1]]]
    #creando paleta de colores dinámica
    pal <- colorQuantile(palette = "viridis",domain = dominio,n = 7)
    #proxy Map
    proxyMap <- leafletProxy('map') 
    
    if(input$campo[[1]] == 'PERSONAS'){
      proxyMap <- proxyMap %>% clearShapes() %>% clearControls() %>% 
        addPolygons(data = shp,group = 'Manzanas', fillColor = ~pal(PERSONAS), fillOpacity = 0.7,
                    stroke = 0.1,color = 'white',weight = 1, smoothFactor = 0.2, 
                    label = ~htmlEscape(PERSONAS)) %>%
        addLegend("bottomright", pal = pal, values = dominio,
                  title = "Total de personas por manzana censal",
                  opacity = 0.7,group = 'Leyenda')
    }
    if(input$campo[[1]] == 'TOTAL_VIVI'){
      proxyMap <- proxyMap %>% clearShapes() %>% clearControls() %>% 
        addPolygons(data = shp,group = 'Manzanas', fillColor = ~pal(TOTAL_VIVI), fillOpacity = 0.7,
                    stroke = 0.1,color = 'white',weight = 1, smoothFactor = 0.2, 
                    popup = ~htmlEscape(TOTAL_VIVI))%>%
        addLegend("bottomright", pal = pal, values = dominio,
                  title = "Total de viviendas por manzana censal",
                  opacity = 0.7,group = 'Manzanas')
    }
    if(input$campo[[1]] == 'DENSIDAD'){
      proxyMap <- proxyMap %>% clearShapes() %>% clearControls() %>% 
        addPolygons(data = shp,group = 'Manzanas', fillColor = ~pal(DENSIDAD), fillOpacity = 0.7,
                    stroke = 0.1,color = 'white',weight = 1, smoothFactor = 0.2, 
                    label = ~htmlEscape(DENSIDAD))%>%
        addLegend("bottomright", pal = pal, values = dominio,
                  title = "Densidad (personas/hectáreas)",
                  opacity = 0.7,group = 'Manzanas')
    }
    #print map
    proxyMap
    
  })
  
  output$grafico <- renderPlot({
    req(input$plot) # require
    #evitar comportamiento no deseado al cambiar el campo
    campo <- isolate({input$campo[[1]]})
    
    dataPlot(datos = shp,campo = campo)
  })

}

#compilar
shinyApp(ui,server)