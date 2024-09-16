library(shiny)
library(leaflet)
library(dplyr)
library(sf)
library(bslib)
library(shinydashboard)

# Reading the dataset with countries and their corresponding coordinates and cases
COVIDCasesLatLong <- readRDS("Saved RDS Files/CountryCases.rds")

# Define color palette for the markers
colorPalette <- colorFactor(palette = c("blue", "red"), domain = COVIDCasesLatLong$Cases)

####################################

ui <- page_sidebar(
  title = "COVID Cases Per Country (2020)",
  
  #Sidebar Panel for Country Select (Drop down)
  sidebar = sidebar(
    "Sidebar",
    selectInput("country", "Select a country:", choices = unique(COVIDCasesLatLong$Countries))
  ),
  
  #Value Box that displays the number of COVID cases
  valueBoxOutput("valueBox"),
  
  #Leaflet Map Display
  card(leafletOutput("map")),
)

server <- function(input, output, session) {
  
  
  #Rendering the leaflet output
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = 0, lat = 0, zoom = 2)
  })
  
  #Adding circular markers based on the number of COVID cases
  #Circular markers react to clicks with a 'popup'
  observe({
    leafletProxy("map", data = COVIDCasesLatLong) %>%
      addProviderTiles("CartoDB.Voyager") %>%
      clearMarkers() %>%
      addCircleMarkers(~Longitude, ~Latitude, radius = ~sqrt(Cases)/70, color = ~colorPalette(Cases), fillOpacity = 0.5, 
                       popup = ~paste("Country: ", Countries, "<br>Cases: ", Cases))
  })
  
  #A value box output based on the input in the drop down 
  output$valueBox <- renderValueBox({
    selected_country <- input$country
    selected_cases <- COVIDCasesLatLong$Cases[COVIDCasesLatLong$Countries == selected_country]
    value_box(
      title = "Number of Cases",
      value = selected_cases,
      showcase = bsicons::bs_icon("bar-chart"),
    )
  })

}

shinyApp(ui, server)
