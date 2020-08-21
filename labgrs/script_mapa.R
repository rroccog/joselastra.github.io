library(leaflet)
library(sf)

pasantes <- readxl::read_excel('pasantes_list.xls')

dots <- st_as_sf(x = pasantes,                         
         coords = c("long", "lat"),crs=4326)

iconos <- icons(
  iconUrl ="https://joselastra.github.io/labgrs/icono.png",
  iconWidth = 25, iconHeight = 28,
  iconAnchorX = 22, iconAnchorY = 94
)

leaflet(dots) %>% addProviderTiles(providers$CartoDB.DarkMatter) %>% 
  addMarkers(icon = iconos,popup = paste0("<b>Nombre: </b>", pasantes$NAME,"<br>",
                                          "<b>Universidad: </b>",pasantes$UNIVERSITY,"<br>",
                                          "<b>Pais: </b>", pasantes$COUNTRY,"<br>",
                                          "<a href='", pasantes$URL,"' target='_blank'>",
                                          "Click Here to view university page</a>"),
             popupOptions = style = list("font-weight" = "normal"
                                         , padding = "3px 8px"
                                         , textsize = "15px"
                                         , direction = "auto" )
             )