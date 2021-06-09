

# Setup -------------------------------------------------------------------

library(shiny)
library(leaflet)
library(purrr)
library(dplyr)
library(readr)
library(DT)

Sys.setenv(TZ="America/New_York")


set_time <- function(x, tz) {
  x <- as.POSIXct(x, tz = "America/New_York")
  out <- as.list(rep(x, length(tz)))
  
  for (i in seq_along(out)) {
    attr(out[[i]], "tzone") <- tz[[i]]
  }
  
  map_chr(out, function(x) format(x, "%H:%M"))
  
}

set_day <- function(x, tz) {
  x <- as.POSIXct(x, tz = "America/New_York")
  out <- as.list(rep(x, length(tz)))
  
  for (i in seq_along(out)) {
    attr(out[[i]], "tzone") <- tz[[i]]
  }
  
  map_chr(out, function(x) format(x, "%A"))
  
}


# Data --------------------------------------------------------------------

grid <- read_rds("locations.rds") %>% 
  mutate(across(c(start, end), as.Date))


grid <- grid 
  #sf::st_as_sf(coords = c("lon", "lat"), crs = sf::st_crs(4326)) %>% 
  #sf::st_transform("+proj=moll")


ui <- fluidPage(
  
  fluidRow(
    column(
      width = 4, offset = 1,
      
      selectInput(
        inputId = "user_tzone", label = "Choose your time zone:", 
        choices = unique(grid$tzone), selected = "America/New_York")
      )
  ),
  
  fluidRow(
    column(10, offset = 1, 
    leafletOutput(outputId = "leaflet_map", height = 500)
  )),
  
  fluidRow(
    column(
      width = 4, offset = 1,
      dateInput(inputId = "date", label = "Choose date:")
    ),
    column(
      width = 5, offset = 1,
      sliderInput(inputId = "time", label = "Choose time:", step = 1,
                  min = 0, max = 23, value = 14)
    )
  ),
  
  fluidRow(
    column(10, offset = 1, 
    DT::dataTableOutput("table")
    )
  )
  #verbatimTextOutput("console"),
  
)

server <- function(input, output, session) {
  
  # weird Dr. Who time bending
  
  observeEvent(input$user_tzone, {
    Sys.setenv(TZ= input$user_tzone)
  })
  
  grid_resp <- eventReactive(input$user_tzone, {
    
    grid %>% 
      mutate(time = set_time(as.POSIXct(paste0(input$date, " ", input$time, ":00:00")), tz = tzone),
             day = set_day(as.POSIXct(paste0(input$date, " ", input$time, ":00:00")), tz = tzone)) %>% 
      mutate(`is sicss currently taking place?` = ifelse(start <= input$date & input$date <= end, "yes", "no"))
    
  })
  
  df <- reactive({
    grid_resp() %>% 
      mutate(time = set_time(as.POSIXct(paste0(input$date, " ", input$time, ":00:00")), tz = tzone),
             day = set_day(as.POSIXct(paste0(input$date, " ", input$time, ":00:00")), tz = tzone)) %>% 
      mutate(`is sicss currently taking place?` = ifelse(start <= input$date & input$date <= end, "yes", "no")) 
    
  })
  
  ## map
  
  output$leaflet_map <- renderLeaflet({
    
    
    grid %>% 
      leaflet(options = leafletOptions(minZoom = 0, maxZoom = 18, attributionControl = FALSE)) %>% 
      setView(zoom = 1, lat = 0, lng = 0) %>% 
      addProviderTiles(providers$CartoDB) 
    
  })
  
  observe({
    
    proxy <- leafletProxy("leaflet_map", data = grid) 
    
    proxy %>% 
      clearMarkers() %>%
      addMarkers(
        lng = ~lon,
        lat = ~lat, 
        popup = paste0("<b>", df()$location, ":</b><br/>", df()$time, " (", df()$day, ")")
      )
    
    
    
  })
  
  ## Table
  
  output$table <- DT::renderDataTable({
    df() %>% 
      select(location, time, day, `is sicss currently taking place?`) %>% 
      DT::datatable(rownames = FALSE, filter = "top",
                    style = 'bootstrap',
                    class = 'table-bordered',
                    selection = "single",
                    options = list(pageLength = 25, paginate = FALSE, buttons = c('copy', 'excel'))
                    )

  })
  
}

shinyApp(ui, server)

