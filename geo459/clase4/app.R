library(shiny)

ui <- fluidPage(sidebarPanel(
  h1('Panel de opciones')
),
mainPanel(
  h1('Panel principal')
)
  
)

server <- function(input, output, session) {
  
}

shinyApp(ui, server)