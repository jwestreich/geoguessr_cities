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
  tags$a(href = "https://drive.google.com/drive/folders/1DHE2YUcJenWKWS2VksRXitA4BsHyy1nB?usp=sharing", "Round 1 photos", target = "_blank"),
  tags$br(),
  tags$a(href = "https://drive.google.com/drive/folders/1W5D3R5V12aE42rcHFYG40nUMZHv8E5I6?usp=sharing", "Round 2 photos", target = "_blank"),
  tags$br(),
  tags$a(href = "https://drive.google.com/drive/folders/1FZ3foPiVTefv_AGCcyltR1Z2xZgWzy4-?usp=sharing", "Round 3 photos", target = "_blank"),
  tags$br(),
  tags$a(href = "https://docs.google.com/spreadsheets/d/1wlRz9DgH8z2CxAIZNSs_gphTU5dXCvdlvmhNCdrdY3k/edit?usp=sharing", "Submit answers here", target = "_blank")
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
