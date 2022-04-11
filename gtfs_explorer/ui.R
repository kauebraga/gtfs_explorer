

fluidPage(
  # this script will add elements to the right side of the topbar (links etc)
  tags$head(includeScript("www/navbar_element.js")),
  # Load Css
  tags$head(includeCSS("www/styles.css")),
  tags$head(includeCSS("www/upload_button.css")),
  tags$head(includeCSS("www/navbar.css")),
  tags$style(type = "text/css", "#map_city, #map_routes {height: calc(100vh - 120px) !important;}"),
  useShinydashboard(),
  # shinyjs::useShinyjs(),
  # use_bs_popover(), # you need to call this function somewhere in your ui
  # disconnectMessage(),
  # Use loading page
  use_waiter(),
  waiter_preloader(html = tagList(spin_loaders(id = 2, color = "black")), 
                   color = "rgba(233, 235, 240, .5)"),
  # Start navbar page
  navbarPage(title = "GTFS Explorer", id = "tabs", collapsible = TRUE,
             # Map page
             tabPanel(
               # title = uiOutput('title_map'), 
               # autoWaiter(),
               # waiterOnBusy(),
               # waiterPreloader(),
               waiter_hide_on_render("graph_trips_by_service"),
               title = "Map", 
               value = "tab_general",
               fluidRow(
                 column1(width = 7, width2 = 8, width3 = 9,  
                         box(width = NULL, height = NULL, solidHeader = TRUE,
                             leafglOutput("map_city")
                         )),
                 column1(width = 5,  width2 = 4, width3 = 3,  
                         box(width = NULL, solidHeader = TRUE, title = "Trips by weekday",
                             highchartOutput("graph_trips_by_service", height = "250px")
                         ),
                         box(width = NULL, solidHeader = TRUE, title = "Routes",
                             uiOutput("ibox")))
               )),
             tabPanel(
               # title = uiOutput('title_map'), 
               title = "Routes", 
               value = "tab_routes", 
               fluidRow(
                 column1(width = 6, width2 = 7, width3 = 8,
                         box(width = NULL, height = NULL, solidHeader = TRUE,
                             # Output map
                             leafletOutput("map_routes"))),
                 column1(width = 6,  width2 = 5, width3 = 4,  
                         box(width = NULL, height = NULL, solidHeader = TRUE,
                             # # selecionar linha
                             uiOutput("service_choice"),
                             uiOutput("route_choice"),
                             bsPopover(
                               id = "bins", title = "bins", content =  "this one is okay!"
                             )),
                         box(width = NULL, solidHeader = TRUE,
                             uiOutput("infobox_routes")
                         ),
                         box(width = NULL, solidHeader = TRUE,
                             highchartOutput("graph_frequency", height = "250px")
                         )
                 )
               ))
             # bslib::nav_spacer(),
             # bslib::nav_menu(title = "About the app",
             #                 bslib::nav_item(a(href="http://stackoverflow.com", "stackoverflow")),
             #                 bslib::nav_item(a(href="http://stackoverflow.com", "stackoverflow"))
             # )
             # tags$script(HTML("var header = $('.navbar-collapse');
             #                   header.append('<div style=\"float:right\"><a href=\"https://google.com\" target=\"_blank\">Open Sales Gsheet</a>');
             #                   console.log(header);"))
                         # tags$li(class="dropdown", data-toggle="tab", data-bs-toggle="tab",a("Dados de vacinação: ", href="https://brasil.io/", "brasil.io", target="_blank", style = "float: right"))
                         # # tags$li(class="dropdown", uiOutput("dica")),
                         # tags$li(class="dropdown", tags$a(href="https://kauebraga.dev/", "Contato", target="_blank")),
                         # # tags$li(class="dropdown",tags$a(href="https://www.linkedin.com/in/abhinav-agrawal-pmp%C2%AE-itil%C2%AE-5720309/" ,icon("linkedin"), "My Profile", target="_blank")),
                         # tags$li(class="dropdown",tags$a(href="https://github.com/kauebraga/painel_vacinacao_covid", icon("github"), "Código", target="_blank"))
                         
             )
  )