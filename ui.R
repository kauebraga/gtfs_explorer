

shinyUI(
  
  div(class = "navbar-default",
      # Load Css
      tags$head(includeCSS("www/styles.css")),
      tags$style(type = "text/css", "#map_city, #map_routes {height: calc(100vh - 120px) !important;}"),
      useShinydashboard(),
      # shinyjs::useShinyjs(),
      # tags$head(tags$script(src = "teste.js")),
      # use_bs_popover(), # you need to call this function somewhere in your ui
      # disconnectMessage(),
      # Use loading page
      use_waiter(),
      waiter_preloader(html = tagList(spin_loaders(id = 2, color = "black")), 
                                      color = "rgba(233, 235, 240, .5)"),
      # Start navbar page
      navbarPage("GTFS Explorer", id = "tabs",
                 # Map page
                 tabPanel(
                   # title = uiOutput('title_map'), 
                   # autoWaiter(),
                   # waiterOnBusy(),
                   # waiterPreloader(),
                   waiter_hide_on_render("map_city"),
                   title = "Map", 
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
                   title = "Routes", 
                   value = "tab_routes", 
                   fluidRow(
                     column(width = 7,
                            box(width = NULL, height = NULL, solidHeader = TRUE,
                                # Output map
                                leafletOutput("map_routes"))),
                     column(width = 5,
                            box(width = NULL, height = NULL, solidHeader = TRUE,
                                # # selecionar linha
                                uiOutput("service_choice"),
                                uiOutput("route_choice"),
                                bsPopover(
                                  id = "bins", title = "bins", content =  "this one is okay!"
                                )),
                            box(width = NULL, solidHeader = TRUE,
                                infoBoxOutput("speed_infobox"),
                                infoBoxOutput("stops_infobox"),
                                # div(class = "valuebox-tip",
                                #     shinydashboard::infoBoxOutput("length_infobox")
                                # ),
                                # tippy::tippy_class(
                                #   "valuebox-tip",
                                #   content = "Você precisa importar o css do pacote shinydashboard se quiser usar valueBoxes fora do shinydashboard."
                                # )
                                infoBoxOutput("length_infobox")
                                # shinyInput_label_embed(
                                #   shiny_iconlink() %>%
                                #     bs_embed_popover(
                                #       title = "Letter", content = "Choose a favorite", placement = "left"
                                #     )
                                # shinyInput_label_embed(
                                # shiny_iconlink() %>%
                                #   bs_embed_popover(
                                #     title = "Letter", content = "Choose a favorite", placement = "left"
                                #   ))
                                # div(bsPopover(id = "q3_graph",
                                #               title = "teste",
                                #               content = includeHTML("www/tooltips/popover_activity.html"),
                                #               placement = "top",
                                #               trigger = "hover",
                                #               options = list(container = "body")
                                # ))
                                # shinyInput_label_embed(bs_embed_popover(title = "Bins", content = "this one is okay!"))
                            ),
                            box(width = NULL, solidHeader = TRUE,
                                highchartOutput("graph_frequency", height = "300px")
                            )
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