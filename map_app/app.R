library(leaflet)
library(sf)
library(tigris)
library(shiny)
library(clipr)
library(dplyr)
library(googlesheets4)
library(shinyjs)

ui <- fluidPage(
  useShinyjs(),
  
  tags$head(
    tags$style(HTML("
      body {
        background-image: url('https://file.garden/Zy75IlKnFHAuCMr4/subtletiles.png');
        background-size: 400px;
        background-repeat: repeat;
        min-height: 100vh;
        max-height: 100vh;
        max-width: 600px;
        margin: auto;
        position: relative;
      }
      .mapContainer {
        width: 100%;
        height: 40vh;
        margin: auto;
        margin-bottom: 150px;
        filter: drop-shadow(2px 2px 0.12rem lightgray);
      }
      .mapTitle {
        text-align: left;
        font-size: 20px;
        font-weight: bold;
        margin-bottom: 10px;
      }
      .hidden {
        display: none;
      }
      .btn {
        background-color: #45A8F5 !important;
        border-color: #45A8F5 !important;
        color: black !important;
        font-weight: bold !important;
        transition: background-color 0.2s ease-in-out, box-shadow 0.2s ease-in-out;
        box-shadow: 2px 2px 5px #296DA3 !important;
        margin: 5px;
      }
      .btn:hover {
        background-color: lightgray !important;
      }
      .header-image {
        margin: auto;
        display: block;
        margin-top: 20px;
        width: 100%;
        filter: drop-shadow(2px 2px 0.12rem #969696);
      }
      .spacer {
        margin-top: 15px;
      }
      .spacer_large {
        margin-top: 100px;
      }
      .wide-btn {
        width: 200px !important;
        height: 40px !important;
        font-size: 1.1em !important;
      }
      .left-align {
        text-align: left;
        margin-top: 10px;
      }
    "))
  ),
  
  tags$img(src = "https://file.garden/Z6qa6jS9ZGKtBZjZ/localguessr%2Bpylon.PNG", class = "header-image"),
  
  tags$div(class = "spacer"),
  
  div(class = "left-align", 
      actionButton("photo_link", "View Photos", class = "btn wide-btn", 
                   onclick = "window.open('https://drive.google.com/drive/folders/1DcE2ILZFs9kCR51G6fJ5pR4igar7_NYq?usp=sharing', '_blank')")),
  
  tags$div(class = "spacer"),
  
  textInput("team_name", "Team Name:", value = "", placeholder = "Enter your team name", width = "80%"),
  
  #  tags$div(class = "spacer_large"),
  
  fluidRow(
    column(6, tags$h4(tags$b(tags$span("Number of Locations"), tags$br(), tags$span("this Round")))),
    column(2, actionButton("show_five", "5 maps", class = "btn")),
    column(2, actionButton("show_one", "1 map", class = "btn"))
  ),
  
  lapply(1:5, function(i) {
    div(
      id = paste0("map_container", i),
      class = "mapContainer",
      div(class = "mapTitle", paste("Location", i)),
      leafletOutput(paste0("map", i), width = "100%", height = "100%"),
      tags$br(),
      textInput(paste0("coords_text", i), paste("Location", i, "Coordinates:"), value = "", placeholder = "Click on the map to get coordinates")
    )
  }),
  
  actionButton("submit_button", "Submit", class = "wide-btn"),
  
  tags$div(class = "spacer"),
  
)

server <- function(input, output, session) {
  gs4_auth(path = "enduring-sign-310821-3890a30b88a6.json") 
  
  spreadsheet_id <- "188wl-XhzJ0fz0TCU9zHMOKmARJes0_Is-VX1b9NZgAs"
  
  selected_coords <- reactiveValues(coords = vector("list", 5), num_maps = 5)
  
  observeEvent(input$show_five, {
    selected_coords$num_maps <- 5
    lapply(1:5, function(i) {
      shinyjs::show(paste0("map_container", i))
    })
  })
  
  observeEvent(input$show_one, {
    selected_coords$num_maps <- 1
    lapply(2:5, function(i) {
      shinyjs::hide(paste0("map_container", i))
    })
    shinyjs::show("map_container1")
  })
  
  lapply(1:5, function(i) {
    output[[paste0("map", i)]] <- renderLeaflet({
      leaflet(data = tibble(name = character())) %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        setView(lng = -77.0147, lat = 38.9031, zoom = 11)
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
    
    all_coords <- lapply(1:selected_coords$num_maps, function(i) {
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
