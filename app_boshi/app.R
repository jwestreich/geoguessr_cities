library(leaflet)
library(sf)
library(tigris)
library(shiny)
library(clipr)
library(dplyr)
library(googlesheets4)
#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

ui <- fluidPage(
  titlePanel("NYC Geoguesser Party @ Boshi's"),
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
  tags$a(href = "https://drive.google.com/drive/folders/1DcE2ILZFs9kCR51G6fJ5pR4igar7_NYq?usp=sharing", "Link to photos", target = "_blank"),
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
  gs4_auth(path = "enduring-sign-310821-3890a30b88a6.json") 
  
  spreadsheet_id <- "188wl-XhzJ0fz0TCU9zHMOKmARJes0_Is-VX1b9NZgAs"
  
  selected_coords <- reactiveValues(coords = vector("list", 5))
  
  lapply(1:5, function(i) {
    output[[paste0("map", i)]] <- renderLeaflet({
      leaflet(data = tibble(name = character())) %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        setView(lng = -73.9431, lat = 40.7212, zoom = 11)  # NYC view
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
        data.frame(
          team_name = team_name,
          location = i,
          lat = coords["lat"],
          lng = coords["lng"],
          datetime = Sys.time()
        )
      } else {
        NULL
      }
    })
    all_coords <- bind_rows(all_coords)
    
    if (nrow(all_coords) > 0) {
      sheet_append(ss = spreadsheet_id, data = all_coords)
      showNotification("Responses saved successfully!", type = "message")
    } else {
      showNotification("Please select points on all maps before submitting.", type = "error")
    }
  })
}

shinyApp(ui, server)
