library(shiny)
library(leaflet)
library(waiter)
library(shinyWidgets)
library(shinydashboard)
library(highcharter)


shinyUI(
  
  div(class = "navbar-default",
      # Load Css
      tags$head(includeCSS("www/styles.css")),
      tags$style(type = "text/css", "#map_city {height: calc(100vh - 120px) !important;}"),
      useShinydashboard(),
      # Use loading page
      use_waiter(),
      # Start navbar page
      navbarPage("GTFS Explorer", id = "tabs",
                 # Map page
                 tabPanel(
                   # title = uiOutput('title_map'), 
                   # autoWaiter(),
                   # waiterOnBusy(),
                   # waiterPreloader(),
                   waiter_hide_on_render("map_city"),
                   title = "Mapa", 
                   value = "tab_general",
                   fluidRow(
                     column(width = 8, 
                            box(width = NULL, height = NULL, solidHeader = TRUE,
                                leafletOutput("map_city"))),
                     column(width = 4, 
                            box(width = NULL, solidHeader = TRUE,
                                highchartOutput("graph_trips_by_service", height = "250px")))
                     # absolutePanel(id = "controls_graphs1", class = "panel panel-default", 
                     #               fixed = TRUE, draggable = FALSE,
                     #               top = 80, left = 80, height = "100%", width = "70%",
                     #               leafletOutput("map_city", height = "90%", width = "100%")
                     # )
                     # Output map
                     # # Create the side panel  
                     # absolutePanel(id = "controls", class = "panel panel-default", 
                     #               fixed = TRUE, draggable = FALSE,
                     #               top = 60, right = 10, width = 350, height = 550,
                     #               # Output the 'UI' that was generated in the server
                     #               uiOutput('page_content')
                     # )
                   )),
                 tabPanel(
                   # title = uiOutput('title_map'), 
                   title = "Rotas", 
                   value = "tab_routes", 
                   fluidRow(
                     column(width = 8,
                            box(width = NULL, solidHeader = TRUE,
                                # # selecionar linha
                                uiOutput("route_choice"),
                                # Output map
                                leafletOutput("map_routes")),
                            box(width = NULL, solidHeader = TRUE,
                                infoBoxOutput("speed_infobox"))
                     )
                     # tableOutput("table_routes")
                     # # Create the side panel  
                     # absolutePanel(id = "controls", class = "panel panel-default", 
                     #               fixed = TRUE, draggable = FALSE,
                     #               top = 60, right = 10, width = 350, height = 550,
                     #               # Output the 'UI' that was generated in the server
                     #               uiOutput('page_content')
                     # )
                   ))
                 # # Graphs page
                 # tabPanel(id = "tab_routes", title = uiOutput('title_graph'),
                 #          # Create the side panel
                 #          absolutePanel(id = "controls_graphs", class = "panel panel-default", 
                 #                        fixed = TRUE, draggable = FALSE,
                 #                        top = 60, right = 10, width = 350,
                 #                        uiOutput('graphs')
                 #          ),
                 #          # Panel to put download button
                 #          absolutePanel(id = "download_panel", class = "panel panel-default", 
                 #                        fixed = TRUE, draggable = FALSE,
                 #                        top = 70, right = 300, width = 200, height = 100,
                 #                        dropdownButton(
                 #                          uiOutput('ui_download_button'),
                 #                          hr(),
                 #                          uiOutput('ui_download_dic'),
                 #                          circle = FALSE, 
                 #                          # status = "danger",
                 #                          label = "Download",
                 #                          right = TRUE,
                 #                          up = FALSE,
                 #                          icon = icon("download"), width = "500px",
                 #                          # tooltip = tooltipOptions(title = "Click to see inputs !"),
                 #                          inputId = "download_dropdown"
                 #                          
                 #                        )),
                 #          # Panel to the graphs output
                 #          absolutePanel(id = "controls_graphs1", class = "panel panel-default", 
                 #                        fixed = TRUE, draggable = FALSE,
                 #                        top = 80, left = 80, height = "100%",
                 #                        highchartOutput('output_graph', height = "90%", width = "100%")
                 #          )
                 # )
                 
                 
      )
      
  )
)