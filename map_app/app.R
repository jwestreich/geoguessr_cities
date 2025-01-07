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
      .mapContainer {
        width: 70%;
        height: 80vh;
        margin: auto;
        margin-bottom: 150px; /* Add space between each map */
      }
      .mapTitle {
        text-align: center;
        font-size: 20px;
        font-weight: bold;
        margin-bottom: 10px;
      }
    "))
  ),
  fluidRow(
    column(2, tags$h4(tags$b("Pick a city"))),
    column(1, actionButton("dc_button", "DC")),
    column(1, actionButton("nyc_button", "NYC"))
  ),
  textInput("team_name", "Team Name:", value = "", placeholder = "Enter your team name"),
  lapply(1:5, function(i) {
    div(
      class = "mapContainer",
      div(class = "mapTitle", paste("Location", i)),
      leafletOutput(paste0("map", i), width = "100%", height = "100%"),
      tags$br(),
      textInput(paste0("coords_text", i), paste("Location", i, "Coordinates:"), value = "", placeholder = "Click on the map to get coordinates")
    )
  }),
  actionButton("submit_button", "Submit")
)

server <- function(input, output, session) {
  coords_file <- "coords.csv"
  if (!file.exists(coords_file)) {
    write.csv(data.frame(team_name = character(0), location = numeric(0), lat = numeric(0), lng = numeric(0)), coords_file, row.names = FALSE)
  }
  
  selected_coords <- reactiveValues(coords = vector("list", 5))
  
  lapply(1:5, function(i) {
    output[[paste0("map", i)]] <- renderLeaflet({
      leaflet(data = tibble(name = character())) %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        setView(lng = -77.0147, lat = 38.8943, zoom = 12)
    })
    
    observeEvent(input$dc_button, {
      leafletProxy(paste0("map", i)) %>%
        setView(lng = -77.0147, lat = 38.8943, zoom = 12)
    })
    
    observeEvent(input$nyc_button, {
      leafletProxy(paste0("map", i)) %>%
        setView(lng = -73.9431, lat = 40.7212, zoom = 11)
    })
    
    observeEvent(input[[paste0("map", i, "_click")]], {
      click <- input[[paste0("map", i, "_click")]]
      selected_coords$coords[[i]] <- c(lat = click$lat, lng = click$lng)
      
      leafletProxy(paste0("map", i)) %>%
        clearMarkers() %>%
        addMarkers(lng = click$lng, lat = click$lat)
      
      updateTextInput(session, paste0("coords_text", i), value = paste(click$lat, click$lng, sep = ", "))
    })
  })
  
  observeEvent(input$submit_button, {
    team_name <- input$team_name
    if (team_name == "") {
      showNotification("Please enter a team name before submitting.", type = "error")
      return()
    }
    
    all_coords <- lapply(1:5, function(i) {
      coords <- selected_coords$coords[[i]]
      if (!is.null(coords)) {
        data.frame(team_name = team_name, location = i, lat = coords["lat"], lng = coords["lng"])
      } else {
        NULL
      }
    })
    all_coords <- bind_rows(all_coords)
    
    if (nrow(all_coords) > 0) {
      write.table(
        all_coords,
        coords_file,
        append = TRUE,
        sep = ",",
        row.names = FALSE,
        col.names = !file.exists(coords_file)
      )
      showNotification("Coordinates and team name saved successfully!", type = "message")
    } else {
      showNotification("Please select points on all maps before submitting.", type = "error")
    }
  })
}

shinyApp(ui, server)
