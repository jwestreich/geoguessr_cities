library(leaflet)
library(sf)
library(tigris)
library(shiny)
library(clipr)
library(dplyr)

ui <- fluidPage(
  titlePanel("Geoguessr Cities"),
  tags$head(
    tags$style(HTML("
      #mapContainer {
        width: 70%; /* Set the width for portrait ratio */
        height: 80vh; /* Adjust height as needed */
        margin: auto; /* Center the map horizontally */
      }
    "))
  ),
  fluidRow(
    column(1, actionButton("dc_button", "DC")),
    column(1, actionButton("nyc_button", "NYC"))
  ),
  div(
    id = "mapContainer",
    leafletOutput("map", width = "100%", height = "100%")
  ),
  verbatimTextOutput("coords"),
  tags$a(href = "https://drive.google.com/drive/folders/1SC6i48TZ9XqfO_0-EW7pSASEYJPX_zgj?usp=drive_link", "Round 1 photos", target = "_blank"),
  tags$br(),
  tags$a(href = "https://drive.google.com/drive/folders/1eon0YM4-xRIwulEKgXmN-A_qFrPUH6_O?usp=drive_link", "Round 2 photos", target = "_blank"),
  tags$br(),
  tags$a(href = "https://drive.google.com/drive/folders/1tYXD85_fmU52VgYsgAzVcHYniErzgk8y?usp=drive_link", "Round 3 photos", target = "_blank"),
  tags$br(),
  tags$a(href = "https://docs.google.com/spreadsheets/d/1FnSqet68G3hWA0wMUM0Ti-W-PRbS-DMRt4g8zYX5t5s/edit?usp=sharing", "Submit answers here", target = "_blank")
)

server <- function(input, output, session) {
  output$map <- renderLeaflet({
    leaflet(data = tibble(name = character())) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = -77.0147, lat = 38.8943, zoom = 12)
  })
  
  observeEvent(input$dc_button, {
    leafletProxy("map") %>%
      setView(lng = -77.0147, lat = 38.8943, zoom = 12)
  })
  
  observeEvent(input$nyc_button, {
    leafletProxy("map") %>%
      setView(lng = -73.9431, lat = 40.7212, zoom = 11)
  })
  
  observeEvent(input$map_click, {
    click <- input$map_click
    lat <- click$lat
    lng <- click$lng
    
    leafletProxy("map") %>%
      clearMarkers() %>%
      addMarkers(lng = lng, lat = lat)
    
    lat_lng <- paste(lat, lng, sep = ", ")
    output$coords <- renderText({
      paste(lat_lng)
    })
  })
}


shinyApp(ui, server)
