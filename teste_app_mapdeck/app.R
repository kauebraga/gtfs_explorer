library(shiny)
library(shinyWidgets)
library(gtfstools)
library(mapdeck)


gtfs <- read_gtfs("data/gtfs_spo_bernardo_2020-11.zip")
gtfs$shapes <- gtfstools::convert_shapes_to_sf(gtfs)
gtfs$shapes <- merge(gtfs$shapes, unique(gtfs$trips, by = c("route_id", "shape_id")))
gtfs$shapes <- merge(gtfs$shapes, gtfs$routes[, .(route_id, route_short_name, route_long_name, route_type)])
gtfs$shapes <- sf::st_sf(gtfs$shapes)

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Hello Shiny!"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Slider for the number of bins ----
      radioButtons(inputId = "change",
                   label = "Number of bins:",
                   c("on", "off"),
                   selected = "on"
      ),
      textOutput("go")
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Histogram ----
      mapdeckOutput("map")
      
    )
  )
)



server <- function(input, output) {
  
  output$map <- renderMapdeck({
    
    
    mapdeck() %>%
      add_path(data = subset(gtfs$shapes, route_type == 1), layer_id = "id1") %>%
      add_path(data = subset(gtfs$shapes, route_type == 2), layer_id = "id2") %>%
      add_path(data = subset(gtfs$shapes, route_type == 3), layer_id = "id3")
    
    
  })
  
  
  output$go <- renderText({
    
    nrow(gtfs$shapes)
    
  })
  
  # observeEvent(c(input$change), {
  #   
  #   print(input$change)
  #   
  #   if(input$change == "on") {
  #   
  #   mapdeck::mapdeck_update(map_id = "map") %>%
  #       add_path(data = shapes %>% filter(route_type == 3), layer_id = "id3")
  #   
  #     } else {
  #   
  #   mapdeck::mapdeck_update(map_id = "map") %>%
  #     clear_path(layer_id = "id3")
  #   }
  #   
  # })
  
}



shinyApp(ui, server)
