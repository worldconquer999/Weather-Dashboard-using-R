library(shiny)
library(shinydashboard)
library(httr)
library(jsonlite)
library(dplyr)
library(leaflet)
library(ggplot2)

api_key <- "c5c242ae20e914f09e3dcb860399e057"
city <- "Hanoi"
base_url <- "https://api.openweathermap.org/data/2.5/"

# Function to get current weather data
get_current_weather <- function(lat, lon, api_key) {
  url <- paste0(base_url, "weather?lat=", lat, "&lon=", lon, "&appid=", api_key, "&units=metric")
  response <- GET(url)
  content(response, "text", encoding = "UTF-8") %>% fromJSON()
}

# Function to get 5-day forecast data
get_5day_forecast <- function(lat, lon, api_key) {
  url <- paste0(base_url, "forecast?lat=", lat, "&lon=", lon, "&appid=", api_key, "&units=metric")
  response <- GET(url)
  content(response, "text", encoding = "UTF-8") %>% fromJSON()
}

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "Weather Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Basic Information", tabName = "info", icon = icon("info")),
      menuItem("Forecast", tabName = "forecast", icon = icon("cloud"))
    )
  ),
  dashboardBody(
    tabItems(
      # Basic Information tab content
      tabItem(tabName = "info",
              h2("Basic Information"),
              fluidRow(
                box(title = "Location", status = "primary", solidHeader = TRUE, width = 6, textOutput("location")),
                box(title = "Current Temperature", status = "success", solidHeader = TRUE, width = 6, textOutput("temp")),
                box(title = "Humidity", status = "warning", solidHeader = TRUE, width = 6, textOutput("humidity")),
                box(title = "Visibility", status = "info", solidHeader = TRUE, width = 6, textOutput("visibility")),
                box(title = "Feels Like", status = "danger", solidHeader = TRUE, width = 6, textOutput("feels_like")),
                box(title = "Condition", status = "success", solidHeader = TRUE, width = 6, textOutput("condition")),
                box(title = "Wind Speed", status = "info", solidHeader = TRUE, width = 6, textOutput("wind_speed")),
                box(title = "Air Pressure", status = "warning", solidHeader = TRUE, width = 6, textOutput("pressure")),
                box(title = "Location Map", status = "primary", solidHeader = TRUE, width = 12,
                    leafletOutput("map"))
              )
      ),
      
      # Forecast tab content
      tabItem(tabName = "forecast",
              h2("Weather Forecast"),
              fluidRow(
                box(title = "Temperature Forecast", status = "primary", solidHeader = TRUE, width = 12, plotOutput("tempPlot")),
                box(title = "Visibility Forecast", status = "primary", solidHeader = TRUE, width = 12, plotOutput("visibilityPlot")),
                box(title = "Wind Speed Forecast", status = "primary", solidHeader = TRUE, width = 12, plotOutput("windPlot")),
                box(title = "Humidity Forecast", status = "primary", solidHeader = TRUE, width = 12, plotOutput("humidityPlot"))
              )
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  # Initial coordinates for Hanoi
  lat <- 21.0285
  lon <- 105.8542
  
  # Reactive values to store selected coordinates
  selected_coords <- reactiveValues(lat = lat, lon = lon)
  
  # Render Leaflet map with a marker at the selected coordinates and allow user to choose location
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addMarkers(lng = selected_coords$lon, lat = selected_coords$lat, popup = city) %>%
      setView(lng = selected_coords$lon, lat = selected_coords$lat, zoom = 10)
  })
  
  # Update marker and weather information when user clicks on the map
  observeEvent(input$map_click, {
    click <- input$map_click
    selected_coords$lat <- click$lat
    selected_coords$lon <- click$lng
    
    leafletProxy("map") %>%
      clearMarkers() %>%
      addMarkers(lng = selected_coords$lon, lat = selected_coords$lat, popup = paste("Lat:", selected_coords$lat, "Lon:", selected_coords$lon))
    
    # Get new weather data based on chosen location
    new_weather <- get_current_weather(selected_coords$lat, selected_coords$lon, api_key)
    output$location <- renderText({ paste("Lat:", selected_coords$lat, "Lon:", selected_coords$lon) })
    output$temp <- renderText({ paste(new_weather$main$temp, "°C") })
    output$humidity <- renderText({ paste(new_weather$main$humidity, "%") })
    output$visibility <- renderText({ paste(new_weather$visibility / 1000, "km") })
    output$feels_like <- renderText({ paste(new_weather$main$feels_like, "°C") })
    output$condition <- renderText({ new_weather$weather$description })
    output$wind_speed <- renderText({ paste(new_weather$wind$speed, "m/s") })
    output$pressure <- renderText({ paste(new_weather$main$pressure, "hPa") })
  })
  
  output$location <- renderText({ paste("Lat:", lat, "Lon:", lon) })
  output$temp <- renderText({ paste(get_current_weather(lat, lon, api_key)$main$temp, "°C") })
  output$humidity <- renderText({ paste(get_current_weather(lat, lon, api_key)$main$humidity, "%") })
  output$visibility <- renderText({ paste(get_current_weather(lat, lon, api_key)$visibility / 1000, "km") })
  output$feels_like <- renderText({ paste(get_current_weather(lat, lon, api_key)$main$feels_like, "°C") })
  output$condition <- renderText({ get_current_weather(lat, lon, api_key)$weather$description })
  output$wind_speed <- renderText({ paste(get_current_weather(lat, lon, api_key)$wind$speed, "m/s") })
  output$pressure <- renderText({ paste(get_current_weather(lat, lon, api_key)$main$pressure, "hPa") })
  
  # Update forecast plots
  forecast_data <- reactive({
    get_5day_forecast(selected_coords$lat, selected_coords$lon, api_key)
  })
  
  output$tempPlot <- renderPlot({
    forecast <- forecast_data()
    temp_data <- forecast$list
    
    ggplot(temp_data, aes(x = dt, y = main$temp)) +
      geom_line() +
      labs(title = "5-day Temperature Forecast", x = "Date", y = "Temperature (°C)")
  })
  
  output$visibilityPlot <- renderPlot({
    forecast <- forecast_data()
    visibility_data <- forecast$list %>%
      mutate(dt = as.POSIXct(dt_txt)) %>%
      select(dt, visibility)
    
    ggplot(visibility_data, aes(x = dt, y = visibility)) +
      geom_line() +
      labs(title = "5-day Visibility Forecast", x = "Date", y = "Visibility (m)")
  })
  
  output$windPlot <- renderPlot({
    forecast <- forecast_data()
    wind_data <- forecast$list
    
    ggplot(wind_data, aes(x = dt, y = wind$speed)) +
      geom_line() +
      labs(title = "5-day Wind Speed Forecast", x = "Date", y = "Wind Speed (m/s)")
  })
  
  output$humidityPlot <- renderPlot({
    forecast <- forecast_data()
    humidity_data <- forecast$list
    
    ggplot(humidity_data, aes(x = dt, y = main$humidity)) +
      geom_line() +
      labs(title = "5-day Humidity Forecast", x = "Date", y = "Humidity (%)")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
