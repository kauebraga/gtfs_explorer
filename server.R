# increase max upload size to 30mb
options(shiny.maxRequestSize=30*1024^2)

source(dir("fun", full.names = TRUE))

library(gtfstools)
library(shiny)
library(leaflet)
library(leafgl)
library(shinydashboard)
library(data.table)
library(waiter)
library(highcharter)
# library(kauetools)
library(htmltools)


function(input, output, session) {
  
  values <- reactiveValues(gtfs = NULL, stop_times = NULL, modal_closed = FALSE,
                           route_selected = NULL)
  
  # 1) modal to upload gtfs at startup ----------------------------------------------------------
  query_modal <- div(id = "modal_lang", 
                     modalDialog(
                       title = HTML("Select a GTFS file: "),
                       renderUI({
                         div(style = "width: 50%;margin: 0 auto;", 
                             fileInput(
                               inputId = "gtfs", 
                               label = "Choose CSV File",
                               multiple = TRUE,
                               accept = c(".zip"))
                         )
                       }),
                       easyClose = FALSE,
                       size = "m",
                       footer = NULL
                       # footer = actionButton("dismiss_modal",label = "Dismiss")
                       # footer = div(id = "openDetails", class = "btn btn-default action-button shiny-bound-input",
                       #              tagList(
                       #                modalButton(icon("check"))
                       #              )
                       # )
                       
                     )
  ) 
  
  # Show the model on start up ...
  showModal(query_modal)
  
  # ok 
  w <- Waiter$new(
    # html = span("Loading..."),
    # html = spin_loader(),
    # color = transparent(.7)
  )
  # w <- Waiter$new(id = c('map_city', "graph_trips_by_service", "map_routes"),
  #                        html = spin_loader(), color = "##F4F6F6")
  
  # observeEvent(input$dismiss_modal, {
  #   
  #   values$modal_closed <- T
  #   waiter_show(html = spin_loader(), color = "##F4F6F6")
  #   removeModal()
  #   
  # })
  
  
  observeEvent(input$gtfs, {
    
    
    req(input$gtfs)
    
    removeModal()
    # ok
    waiter_show(html = tagList(spin_loaders(id = 2, color = "black"), br(), span("Opening GTFS...", style = "color: black")),
                color = "rgba(233, 235, 240, .5)")
    
    # w$update(html = tagList(spin_fading_circles(), br(), "Opening GTFS.."))
    
    print(length(input$gtfs$datapath))
    
    if (length(input$gtfs$datapath) == 1) {
      
      
      gtfs1 <- gtfstools::read_gtfs(input$gtfs$datapath,
                                    skip = "stop_times")
      
    } else {
      
      gtfs1 <- lapply(input$gtfs$datapath, gtfstools::read_gtfs, skip = "stop_times")
      gtfs1 <- do.call(gtfstools::merge_gtfs, gtfs1)
      
    }
    
    print(input$gtfs$name)
    
    # # upload gtfs locally
    # gtfstools::write_gtfs(
    #   gtfs1,
    #   path = file.path("data_teste", paste0(basename(input$gtfs$name), "-", gsub(" ", "_", Sys.time())))
    # )
    
    # upload gtfs locally
    data_upload <- data.table(gtfs = input$gtfs$name, time = Sys.time())
    fwrite(data_upload,
           file = "data_teste/gtfs_uploads.csv",
           append = TRUE
    )
    
    w$update(html = tagList(spin_loaders(id = 2, color = "black"), br(), span("Generating map...", style = "color: black")))
    
    values$gtfs <- gtfs1
    shapes <- gtfstools::convert_shapes_to_sf(gtfs1)
    # shapes <- sf::st_simplify(shapes)
    # bring route to the shapes
    shapes <- merge(shapes, unique(gtfs1$trips, by = c("route_id", "shape_id")))
    shapes <- merge(shapes, gtfs1$routes[, .(route_id, route_short_name, route_long_name, route_type)])
    shapes <- sf::st_sf(shapes)
    values$shapes <- shapes
    
    # boxes:
    # number of trips by service_id
    # number of routes by route_type
    # 
    
    # identify weekday in services
    services_workday <- values$gtfs$calendar[monday == 1 | tuesday == 1 | wednesday == 1 | thursday == 1 | friday == 1]
    services_saturday <- values$gtfs$calendar[saturday == 1]
    services_sunday <- values$gtfs$calendar[sunday == 1]
    
    trips_workday <- values$gtfs$trips[service_id %in% services_workday$service_id, .(trips = .N)]
    trips_workday[, type := "Workday"]
    trips_saturday <- values$gtfs$trips[service_id %in% services_saturday$service_id, .(trips = .N)]
    trips_saturday[, type := "Saturday"]
    trips_sunday <- values$gtfs$trips[service_id %in% services_sunday$service_id, .(trips = .N)]
    trips_sunday[, type := "Sunday"]
    # bind
    trips_days <- rbind(trips_workday, trips_saturday, trips_sunday)
    
    values$trips_workday <- values$gtfs$trips[service_id %in% services_workday$service_id, .(trip_id)]
    values$trips_saturday <- values$gtfs$trips[service_id %in% services_workday$service_id, .(trip_id)]
    values$trips_sunday <- values$gtfs$trips[service_id %in% services_workday$service_id, .(trip_id)]
    
    output$graph_trips_by_service <- renderHighchart({
      
      
      hchart(trips_days, "bar", hcaes(y = trips, x = type),
             name = "Frequência") %>%
        hc_xAxis(
          title = list(text = "")
          #   labels = list(useHTML = TRUE,
          #                 style = list(width = '100px')))%>%
        ) %>%
        hc_yAxis(labels = list(enabled = TRUE),
                 title = list(text = ""),
                 tickLength = 0,
                 gridLineWidth = 0) %>%
        hc_title(
          text = "Trips by weekday"
        ) %>%
        hc_chart(style = list(fontFamily = "Roboto Condensed")) %>%
        hc_plotOptions(bar = list(borderRadius = 1,
                                  borderColor = "#000000",
                                  color = "#F4F4F4",
                                  tooltip = list(
                                    pointFormat = sprintf("%s: {point.y}", "Total"),
                                    valueDecimals = 0),
                                  # stacking = FALSE,
                                  allowPointSelect = TRUE
                                  
                                  
        ))
    })
    
    
    output$map_city <- renderLeaflet({
      
      
      map_layers <- function() {
        
        
        # get route types
        k <- unique(shapes$route_type)
        
        df <- data.table(sigla = 0:3,
                         text = c("LRT", "Subway", "Rail", "Bus"))
        
        
        #base map
        map <- leaflet() %>%
          addProviderTiles(providers$CartoDB.Positron)
        
        #loop through all groups and add a layer one at a time
        for (i in seq_along(k)) {
          map <- map %>% 
            addGlPolylines(
              data = subset(shapes, route_type == k[[i]]), 
              group = as.character(df[sigla == k[[i]]]$text),
              color = "black"
              # layerId = ~route_id
            )
          # addPolylines(
          #   data = subset(shapes, route_type == k[[i]]), group = as.character(df[sigla == k[[i]]]$text)
          # )
        }
        
        #create layer control
        map %>% 
          addLayersControl(
            overlayGroups = df[sigla %in% k]$text,
            options = layersControlOptions(collapsed = FALSE))
        # hideGroup(as.character(c(2:k))) #hide all groups except the 1st one
        
      }
      
      #plot the map
      map_layers()
      
      # # Stop the loading page here !
      # waiter_hide_on_render()
      
      
      # leaflet() %>%
      #   addTiles() %>%
      #   addPolylines(data = values$shapes, group = "Linha") %>%
      #   addLayersControl(overlayGroups = c("Linha"),
      #                    options = layersControlOptions(collapsed = FALSE))
      
      
    })
    
    # waiter_hide()
    
    
    
    # routes by type
    route_by_type <- values$gtfs$routes[, .N, by = route_type]
    
    
    # output$speed_infobox <- renderInfoBox({
    #   infoBox(
    #     "Velocidade",
    #     paste0(round(mean_speed), " km/h"),
    #     icon = icon("user-friends"),
    #     color = "black"
    #   )
    #   
    # })
    
    
    # click event
    # click event
    
  })
  
  # click event
  observeEvent(input$map_city_shape_click, {
    # click event
    click <- input$map_city_shape_click
    print(click) # this returns NULL if a polygone is clicked for the second time
    
    values$route_selected <- click$id
    
  })
  
  
  
  # count to know how many times tab was clicked
  count <- reactiveVal(0)
  
  
  observeEvent(input$tabs, {
    
    if (input$tabs == "tab_routes") {
      
      count(count()+1)
      
      if(count() == 1) {
        
        
        waiter_show(html = tagList(spin_loaders(id = 2, color = "black"), br(), span("Calculating...", style = "color: black")),
                    color = "rgba(233, 235, 240, .5)")
        
        
        # print(paste("You clicked tab:", input$tabs))
        
        # print(input$gtfs$datapath)
        
        print("loading st")
        
        stop_times <- gtfstools::read_gtfs(input$gtfs$datapath,
                                           files = "stop_times")$stop_times
        # bring routes
        stop_times <- merge(stop_times, values$gtfs$trips[, .(service_id, trip_id, route_id, shape_id, direction_id)],
                            sort = FALSE)
        
        values$stop_times <- stop_times
      }
      
      
    }
    
    print(count())
    
  })
  
  
  
  
  
  output$service_choice <- renderUI({
    
    pickerInput(inputId = "choose_service",
                label = "Choose a service",
                choices = c("Weekday", "Saturday", "Sunday"),
                selected = "Weekday"
    )
    
  })
  
  output$route_choice <- renderUI({
    
    
    # create lists with routes names and types --------------------------------
    
    # list available route_types
    route_types_available <- sort(unique(values$shapes$route_type))
    
    a <- function(route_type1) {
      
      
      # get route type name
      route_type_name <- fcase(
        route_type1 == 0, "LRT",
        route_type1 == 1, "Subway", 
        route_type1 == 2, "Rail", 
        route_type1 == 3,"Bus"
      )
      
      # list all routes from that type
      routes <- subset(values$shapes, route_type == route_type1)
      routes <- sf::st_set_geometry(routes, NULL)
      routes <- setDT(routes)
      # get unique routes
      routes <- unique(routes, by = c("route_id"))
      # modify long name
      routes[, route_name := paste0(route_long_name)]
      # routes[, route_name := paste0(route_short_name)]
      # routes[, route_name := paste0(route_short_name, " - ", route_long_name)]
      
      
      # compose names that will be on the pickerinput
      list_routes <- list(structure(routes$route_id, .Names = routes$route_name))
      # define name
      names(list_routes) <- route_type_name
      
      return(list_routes)
      
    }
    
    routes_choices <- lapply(unique(route_types_available), a)
    routes_choices <- do.call(c, routes_choices)
    
    pickerInput(inputId = "choose_route",
                label = "Choose a route",
                choices = routes_choices,
                # selected = "075",
                options = pickerOptions(
                  liveSearch = TRUE
                ))
    
  })
  
  
  
  observeEvent(c(input$choose_route, input$choose_service), {
    
    # output$table_routes <- renderTable(
    #   
    #   head(values$stop_times[trip_id == input$choose_route])
    #   
    # )
    
    
    
    values$gtfs$stop_times <- values$stop_times
    
    # print(names(values$gtfs))
    
    
    # shapes_filter <- subset(values$shapes, route_id == values$route_selected)
    shapes_filter <- subset(values$shapes, route_id == input$choose_route)
    shapes_filter$direction_id <- rleid(shapes_filter$shape_id)
    
    # print(head(shapes_filter))
    
    # filter service
    service_to_filter <- if(input$choose_service == "Weekday") {
      values$trips_workday$trip_id 
    } else if (input$choose_service == "Saturday") {
      
      values$trips_saturday$trip_id 
      
    } else if (input$choose_service == "Sunday") {
      
      values$trips_sunday$trip_id
    }
    
    trips_filter <- subset(values$gtfs$trips, trip_id %in% service_to_filter)
    # filter route
    trips_filter <- subset(trips_filter, route_id == input$choose_route)
    
    # stops
    stops_filter <- kauetools::extract_scheduled_stops(values$gtfs, route_id = input$choose_route)
    stops_filter[, direction_id := rleid(shape_id)]
    
    
    # speeds
    mean_speed <- gtfstools::get_trip_speed(values$gtfs, trip_id = trips_filter$trip_id, file = "shapes")
    mean_speed <- mean(mean_speed$speed, na.rm = TRUE)
    # print(mean_speed)
    
    print(input$choose_route)
    print(service_to_filter)
    
    # frequency
    mean_frequency <- calculate_route_frequency(gtfs = values$gtfs,
                                                route_id = input$choose_route,
                                                trip_id = service_to_filter,
                                                start_time = "05:00:00",
                                                end_time = "21:00:00",
                                                mean_headway = TRUE)
    
    
    # mean_frequency_inbound <- mean_frequency$headway_mean[1]
    # mean_frequency_outbound <- mean_frequency$headway_mean[2]
    
    
    
    icons <- awesomeIcons(
      icon = "bus",
      library = "fa",
      iconrColor = 'black',
      iconRotate = 10)
    
    
    output$map_routes <- renderLeaflet({
      
      
      
      leaflet() %>%
        addTiles() %>%
        addAwesomeMarkers(data = stops_filter[direction_id == 1], 
                          lng = ~stop_lon, lat = ~stop_lat, 
                          group = "Inbound",
                          label = ~htmlEscape(stop_name),
                          icon = icons) %>%
        addAwesomeMarkers(data = stops_filter[direction_id == 2], 
                          lng = ~stop_lon, lat = ~stop_lat, 
                          group = "Outbound",
                          label = ~htmlEscape(stop_name), 
                          icon = icons) %>%
        addPolylines(data = subset(shapes_filter, direction_id == 1), group = "Inbound") %>%
        addPolylines(data = subset(shapes_filter, direction_id == 2), group = "Outbound") %>%
        addLayersControl(
          overlayGroups = c("Inbound", "Outbound"),
          options = layersControlOptions(collapsed = FALSE)) %>%
        addMiniMap()
      
      
    })
    
    output$speed_infobox <- renderInfoBox({
      infoBox(
        "Velocidade",
        paste0(round(mean_speed), " km/h"),
        icon = icon("user-friends"),
        color = "black"
        # width = 12
      )
      
    })
    
    output$frequency_infobox <- renderInfoBox({
      infoBox(
        "Frequência",
        paste0(round(c(mean_frequency_inbound)), " minutos"),
        icon = icon("user-friends"),
        color = "black",
        width = 12
      )
      
    })
    
    output$graph_frequency <- renderHighchart({
      
      
      hchart(mean_frequency[[2]], "column", hcaes(y = headway_mean, x = hour, group = shape_id)
             # dataLabels = list(enabled = TRUE, format='{point.headway_mean}')
      ) %>%
        hc_xAxis(
          title = list(text = "Hour"),
          type = "category",
          allowDecimals =  FALSE,
          labels =
            list(enabled = TRUE,
                 format = "{value}h",
                 # labels every 15 days
                 step = 1
            )
          #   labels = list(useHTML = TRUE,
          #                 style = list(width = '100px')))%>%
        ) %>%
        hc_yAxis(labels = list(enabled = TRUE),
                 title = list(text = "Minutes"),
                 tickLength = 0,
                 gridLineWidth = 1) %>%
        hc_title(
          text = "Headway by hour (minutes)"
        ) %>%
        hc_chart(style = list(fontFamily = "Roboto Condensed")) %>%
        hc_plotOptions(column = list(borderRadius = 1,
                                     borderColor = "#000000",
                                     # color = "#F4F4F4",
                                     tooltip = list(
                                       pointFormat = sprintf("%s: {point.y} mins", "Headway"),
                                       valueDecimals = 0),
                                     # stacking = FALSE,
                                     allowPointSelect = TRUE
                                     
                                     
        ))
    })
    
    w$hide()
    
  })
  
  
  
  
  
}