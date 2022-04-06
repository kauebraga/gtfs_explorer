

function(input, output, session) {
  
  # set reactive values
  values <- reactiveValues(gtfs = NULL, stop_times = NULL, modal_closed = FALSE,
                           route_selected = NULL, trips_day = NULL)
  
  # 1) modal to upload gtfs at startup ----------------------------------------------------------
  query_modal <- div(id = "modal_lang", 
                     modalDialog(
                       title = HTML("Select a GTFS file: "),
                       renderUI({
                         div(style = "width: 50%;margin: 0 auto;", 
                             fileInput(
                               inputId = "gtfs", 
                               label = "Choose GTFS File",
                               multiple = TRUE,
                               accept = c(".zip"))
                         )
                       }),
                       easyClose = FALSE,
                       size = "m",
                       footer = NULL
                       # footer = "Please be aware that this is an experimental application"
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
  
  
  
  # ok
  
  
  
  
  # 2) open gtfs and calculate map ------------------------------------------
  
  
  observeEvent(input$gtfs, {
    
    removeModal()
    
    if (is.null(values$gtfs$shapes)) {
      
      waiter_hide()
      a <- div(id = "modal_lang",
               modalDialog(title = "Without Shapes",
                           "GTFS doenst have shapes.txt file, and will not show visualizations",
                           easyClose = TRUE,
                           size = "m"
                           # footer = tagList(
                           #   # modalButton("Cancel"),
                           #   actionButton("refresh", "Refresh")
                           #
                           # )
               ))
      
      showModal(a)
      
      # stop("uii")
      # session$close()
    }
    
    
    waiter_show(html = tagList(spin_loaders(id = 2, color = "black"), br(), span("Opening GTFS...", style = "color: black")),
                color = "rgba(233, 235, 240, .5)")
    
    
    # req(input$gtfs)
    
    if (length(input$gtfs$datapath) == 1) {
      
      
      values$gtfs <- gtfstools::read_gtfs(input$gtfs$datapath,
                                          skip = "stop_times")
      
    } else {
      
      values$gtfs <- lapply(input$gtfs$datapath, gtfstools::read_gtfs, skip = "stop_times")
      values$gtfs <- do.call(gtfstools::merge_gtfs, values$gtfs)
      
    }
    
    # calculate shapes
    
    if (is.null(values$gtfs$shapes)) {
      
      waiter_hide()
      a <- div(id = "modal_lang",
               modalDialog(title = "Without Shapes",
                           "GTFS doenst have shapes.txt file, and will not show visualizations, speeds, and distance data",
                           easyClose = TRUE,
                           size = "m"
                           # footer = tagList(
                           #   # modalButton("Cancel"),
                           #   actionButton("refresh", "Refresh")
                           #
                           # )
               ))
      
      showModal(a)
      
      # stop("uii")
      # session$close()
    } else {
      
      values$gtfs$shapes <- gtfstools::convert_shapes_to_sf(values$gtfs)
      # bring route to the shapes
      values$gtfs$shapes <- merge(values$gtfs$shapes, unique(values$gtfs$trips, by = c("route_id", "shape_id")))
      values$gtfs$shapes <- merge(values$gtfs$shapes, values$gtfs$routes[, .(route_id, route_short_name, route_long_name, route_type)])
      values$gtfs$shapes <- sf::st_sf(values$gtfs$shapes)
      # calculate distance
      values$gtfs$shapes$dist <- as.numeric(sf::st_length(values$gtfs$shapes))
      
    }
    
    type_gtfs <- if (any(grepl(pattern = "frequencies", x = names(values$gtfs)))) {
      
      if(nrow(values$gtfs$frequencies) > 0) "frequencies" else "stop_times"
      
    } else "stop_times"
    
    # calculate trips
    if (type_gtfs == "frequencies") {
      
      # calculate the number of trips by trip_id
      
      # transform to itime
      values$gtfs$frequencies[, ':='(end_time = as.ITime(end_time), start_time = as.ITime(start_time))]
      
      # definir quantidade de viagens
      # transform hours to numeric
      # now hours are represented as the number of seconds within a day
      values$gtfs$frequencies[, ':='(end_time = fifelse(end_time == as.ITime("00:00:00"), 86400, as.numeric(end_time)), 
                                     start_time = as.numeric(start_time))]
      
      values$gtfs$frequencies[, tempo_freq := end_time - start_time]
      
      # calculate the amount of trips
      values$gtfs$frequencies[, ntrips := floor(tempo_freq/headway_secs) + 1]
      # group and sumns
      trips_by_tripid <- values$gtfs$frequencies[, .(ntrips = sum(ntrips)), by = trip_id]
      
      # bring ntrips to the trips files
      values$gtfs$trips[trips_by_tripid, on = "trip_id",
                        c("ntrips") := list(i.ntrips)]
      
      # delete trips that dont have frequency
      values$gtfs$trips <- values$gtfs$trips[!is.na(ntrips)]
      
      
    } else {
      
      # print(class(values$gtfs$trips))
      values$gtfs$trips[, ntrips := 1]
      
    }
    
    # if doesnt have shape id, fake it!
    if (is.null(values$gtfs$trips$shape_id)) values$gtfs$trips[, shape_id := NA] else values$gtfs$trips
    
    # identify weekday in services
    services_workday <- values$gtfs$calendar[monday == 1 | tuesday == 1 | wednesday == 1 | thursday == 1 | friday == 1]
    services_saturday <- values$gtfs$calendar[saturday == 1]
    services_sunday <- values$gtfs$calendar[sunday == 1]
    
    trips_workday <- values$gtfs$trips[service_id %in% services_workday$service_id, .(trips = sum(ntrips))]
    trips_workday[, type := "Workday"]
    trips_saturday <- values$gtfs$trips[service_id %in% services_saturday$service_id, .(trips = sum(ntrips))]
    trips_saturday[, type := "Saturday"]
    trips_sunday <- values$gtfs$trips[service_id %in% services_sunday$service_id, .(trips = sum(ntrips))]
    trips_sunday[, type := "Sunday"]
    
    values$trips_workday <- values$gtfs$trips[service_id %in% services_workday$service_id, .(trip_id)]
    values$trips_saturday <- values$gtfs$trips[service_id %in% services_workday$service_id, .(trip_id)]
    values$trips_sunday <- values$gtfs$trips[service_id %in% services_workday$service_id, .(trip_id)]
    
    # bind
    values$trips_day <- rbind(trips_workday, trips_saturday, trips_sunday)
    print(values$trips_day)
    
    
    
  })
  
  output$graph_trips_by_service <- renderHighchart({
    
    req(values$trips_day)
    
    hchart(values$trips_day, "bar", hcaes(y = trips, x = type),
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
      # hc_title(
      #   text = "Trips by weekday"
      # ) %>%
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
    
    req(values$gtfs$shapes)
    
    
    if (!is.null(values$gtfs$shapes)) {
      
      
      w$update(html = tagList(spin_loaders(id = 2, color = "black"), br(), 
                              # span("Opening GTFS...", style = "color: black"), br(),
                              span("Generating map with ", br(), strong(sprintf("%s", nrow(values$gtfs$shapes))), "transit lines...", 
                                   style = "color: black"),
                              br(),
                              if (nrow(values$gtfs$shapes) > 500) span(strong("This may take a while"), style = "color: red") else span("")
      ))
      
      
      
      map_layers <- function() {
        
        
        # get route types
        k <- unique(values$gtfs$shapes$route_type)
        
        df <- data.table(sigla = 0:3,
                         text = c("LRT", "Subway", "Rail", "Bus"))
        
        
        #base map
        map <- leaflet() %>%
          addProviderTiles(providers$CartoDB.Positron)
        
        #loop through all groups and add a layer one at a time
        for (i in seq_along(k)) {
          map <- map %>% 
            addGlPolylines(
              data = subset(values$gtfs$shapes, route_type == k[[i]]), 
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
      
    } else {
      
      map <- leaflet() %>%
        addProviderTiles(providers$CartoDB.Positron)
      
    }
    
  })
  
  
  
  routes_by_type <- reactive({
    
    # only run if routes are available
    req(values$gtfs$routes)
    
    # routes by type
    route_by_type <- values$gtfs$routes[, .N, by = route_type]
    # bring names
    route_by_type <- merge(route_by_type, data.table(route_type = 0:3,
                                                     text = c("LRT", "Subway", "Rail", "Bus")),
                           sort = FALSE,
                           by = "route_type")
    
    setorder(route_by_type, route_type)
    
    return(route_by_type)
    
    
  })  
  
  output$ibox <- renderUI({
    
    info_routes <- function(route) {
      infoBox1(
        title = routes_by_type()[route_type == route]$text,
        value = paste0(routes_by_type()[route_type == route]$N, " routes"),
        icon = switch(as.character(route), 
                      "0" = icon("user-friends"),
                      "1" = icon("train-subway"),
                      "2" = icon("train"),
                      "3" = icon("bus")), 
        color = "black"
      )
    }
    
    # apply fun
    lapply(sort(routes_by_type()$route_type), info_routes)
    
  })
  
  
  
  # observeEvent(input$gtfs, {
  
  
  
  
  
  
  # # upload gtfs locally
  # gtfstools::write_gtfs(
  #   gtfs1,
  #   path = file.path("data_teste", paste0(basename(input$gtfs$name), "-", gsub(" ", "_", Sys.time())))
  # )
  
  # upload gtfs locally
  # data_upload <- data.table(gtfs = input$gtfs$name, time = Sys.time())
  # fwrite(data_upload,
  #        file = "data_teste/gtfs_uploads.csv",
  #        append = TRUE
  # )
  
  # w$update(html = tagList(spin_loaders(id = 2, color = "black"), br(), span("Generating map...", style = "color: black")))
  
  
  # values$gtfs <- gtfs1
  
  
  
  # # if there is no shapes..
  # if (!("shapes" %in% names(values$gtfs) )) {
  #   
  #   waiter_hide()
  #   a <- div(id = "modal_lang",
  #            modalDialog(title = "Without Shapes",
  #                        "GTFS doenst have shapes.txt file, and will not show visualizations",
  #                        easyClose = TRUE,
  #                        size = "m"
  #                        # footer = tagList(
  #                        #   # modalButton("Cancel"),
  #                        #   actionButton("refresh", "Refresh")
  #                        #   
  #                        # )
  #            ))
  #   
  #   showModal(a)
  #   
  #   # stop("uii")
  #   # session$close()
  # }
  
  
  
  
  
  # })
  
  
  
  
  
  
  
  
  
  # click event
  observeEvent(input$map_city_shape_click, {
    # click event
    click <- input$map_city_shape_click
    print(click) # this returns NULL if a polygone is clicked for the second time
    
    values$route_selected <- click$id
    
  })
  
  
  
  # count to know how many times tab was clicked
  count <- reactiveVal(0)
  count1 <- reactiveVal(0)
  
  
  
  # 3) routes tab -----------------------------------------------------------
  
  
  observeEvent(input$tabs, {
    
    if (input$tabs == "tab_routes") {
      
      # count to know how many times tab was clicked
      count(count()+1)
      
      # open gtfs only on the first time the tab is open
      if(count() == 1) {
        
        
        waiter_show(html = tagList(spin_loaders(id = 2, color = "black"), br(), span("Calculating...", style = "color: black")),
                    color = "rgba(233, 235, 240, .5)")
        
        
        # print(paste("You clicked tab:", input$tabs))
        
        print("loading st")
        
        if (length(input$gtfs$datapath) == 1) {
          
          
          values$gtfs$stop_times <- gtfstools::read_gtfs(input$gtfs$datapath,
                                                         files = "stop_times")$stop_times
          
        } else {
          
          values$gtfs$stop_times <- lapply(input$gtfs$datapath, gtfstools::read_gtfs, 
                                           files = "stop_times")
          values$gtfs$stop_times <- do.call(gtfstools::merge_gtfs, values$gtfs$stop_times)
          values$gtfs$stop_times <- values$gtfs$stop_times$stop_times
          
        }
        
        
        # bring routes
        values$gtfs$stop_times <- merge(values$gtfs$stop_times, values$gtfs$trips[, .(service_id, trip_id, route_id, shape_id, direction_id)],
                                        sort = FALSE)
        
        # values$stop_times <- stop_times
      }
      
      
    }
    
    
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
    route_types_available <- sort(unique(values$gtfs$shapes$route_type))
    
    a <- function(route_type1) {
      
      
      # get route type name
      route_type_name <- fcase(
        route_type1 == 0, "LRT",
        route_type1 == 1, "Subway", 
        route_type1 == 2, "Rail", 
        route_type1 == 3,"Bus"
      )
      
      # list all routes from that type
      routes <- subset(values$gtfs$shapes, route_type == route_type1)
      routes <- sf::st_set_geometry(routes, NULL)
      routes <- setDT(routes)
      # get unique routes
      routes <- unique(routes, by = c("route_id"))
      # modify long name
      # routes[, route_name := paste0(route_short_name)]
      # routes[, route_name := paste0(route_short_name)]
      # routes[, route_name := paste0(route_short_name, " - ", route_long_name)]
      
      
      # compose names that will be on the pickerinput
      list_routes1 <- list(structure(routes$route_id, .Names = routes$route_short_name))
      names(list_routes1) <- route_type_name
      # list_routes2 <- list(structure(routes$route_id, .Names = routes$route_long_name))
      # names(list_routes2) <- route_type_name
      # list_routes3 <- list(structure(routes$route_id, .Names = routes$route_id))
      # names(list_routes3) <- route_type_name
      
      
      list_end <- list_routes1
      # list_end <- list(list_routes1, list_routes2, list_routes3)
      return(list_end)
      
    }
    
    routes_choices <- lapply(unique(route_types_available), a)
    routes_choices <- do.call(c, routes_choices)
    
    # TDODO: search by "id, short name or long name"
    pickerInput(inputId = "choose_route",
                label = "Choose a route",
                # RADIO BUTTONS HERE TO SEELCT NAMES
                # label = div("Choose a route")
                choices = routes_choices,
                # selected = "075",
                options = pickerOptions(
                  liveSearch = TRUE
                ))
    
  })
  
  
  # reactive to calculate indicators
  
  shapes_filter <- reactive ({
    
    # filter shapes
    if (is.null(values$gtfs$shapes)) {
      
      shapes_filter <- NULL
      
    } else {
      
      
      shapes_filter <- subset(values$gtfs$shapes, route_id == input$choose_route)
      
    }
    # class(shapes_filter)
    # shapes_filter$direction_id <- rleid(shapes_filter$shape_id)
    
    
  })
  
  service_filter <- reactive ({
    
    
    # filter service
    service_to_filter <- if(input$choose_service == "Weekday") {
      values$trips_workday$trip_id 
    } else if (input$choose_service == "Saturday") {
      
      values$trips_saturday$trip_id 
      
    } else if (input$choose_service == "Sunday") {
      
      values$trips_sunday$trip_id
    }
    
    
  })
  
  trips_filter <- reactive ({
    
    
    print(input$choose_route)
    # filter service
    trips_filter <- subset(values$gtfs$trips, trip_id %in% service_filter())
    # print(trips_filter)
    # filter route
    trips_filter <- subset(trips_filter, route_id == input$choose_route)
    # print(trips_filter)
    
    
  })
  
  
  stops_filter <- reactive ({
    
    
    # calculate stops for each route
    stops_filter <- extract_scheduled_stops(values$gtfs, route_id = input$choose_route)
    # stops_filter[, direction_id := rleid(shape_id) - 1]
    
    
  })
  
  stops_n_filter <- reactive ({
    
    # calculate number of stops
    stops_n <- stops_filter()[, .N, by = direction_id]
    
    
  })
  
  distance_filter <- reactive ({
    
    
    if (is.null(values$gtfs$shapes)) {
      
      NULL 
      
    } else {
      # calculate distance
      dist_mean <- mean(shapes_filter()$dist)
    }
    
    
  })
  
  speed_filter <- reactive ({
    
    if (is.null(values$gtfs$shapes)) {
      
      NULL
      
    } else {
      # calculate speeds
      # print(shapes_filter())
      # print(trips_filter())
      mean_speed <- get_trip_speed1(gtfs = values$gtfs, shapes = shapes_filter(), trips = trips_filter())
      # print(mean_speed)
      mean_speed <- mean(mean_speed$speed, na.rm = TRUE)
    }
    
    
  })
  
  frequency_filter <- reactive ({
    
    
    # calculate frequency
    mean_frequency <- calculate_route_frequency(gtfs = values$gtfs,
                                                route_id = input$choose_route,
                                                trip_id = service_filter(),
                                                start_time = "05:00:00",
                                                end_time = "21:00:00",
                                                mean_headway = TRUE)
    
    
  })
  
  
  
  # render initial routes map -----------------------------------------------
  
  # first, extract gtfs bound
  gtfs_bound <- reactive({
    
    if (is.null(values$gtfs$shapes)) {
      
      NULL
      
    } else {
      
      sf::st_bbox(values$gtfs$shapes)
      
    }
  })
  
  output$map_routes <- renderLeaflet({
    
    if (is.null(values$gtfs$shapes)) {
      
      leaflet() %>%
        addProviderTiles(providers$CartoDB.Positron) }
    
    else {
    
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      fitBounds(gtfs_bound()[[1]], gtfs_bound()[[2]], gtfs_bound()[[3]], gtfs_bound()[[4]])
      
    }
    
  })
  
  
  
  # first observer to change the map
  observeEvent(c(input$choose_route), {
    
    # count to know how many times tab was clicked
    count1(count1()+1)
    
    # open gtfs only on the first time the tab is open
    if(count1() > 1) {
      waiter_show(html = tagList(spin_loaders(id = 2, color = "black")),
                  color = "rgba(233, 235, 240, .5)")
    }
    
    
    icons <- awesomeIcons(
      icon = "bus",
      library = "fa",
      # iconrColor = 'black',
      iconRotate = 10)
    
    # # list available direction_id
    # direction_id_go <- 
    
    
    bbox <- sf::st_bbox(shapes_filter())
    
    map_go <- leafletProxy("map_routes", session) %>%
      clearMarkers() %>%
      clearShapes() %>%
      clearControls() %>%
      flyToBounds(bbox[[1]], bbox[[2]], bbox[[3]], bbox[[4]],
                  options = list(duration = 1.5))
    
    
    map_go %>%
      addAwesomeMarkers(data = stops_filter()[direction_id == 0], 
                        lng = ~stop_lon, lat = ~stop_lat, 
                        group = "Inbound",
                        label = ~htmlEscape(stop_name),
                        icon = icons) %>%
      addAwesomeMarkers(data = stops_filter()[direction_id == 1], 
                        lng = ~stop_lon, lat = ~stop_lat, 
                        group = "Outbound",
                        label = ~htmlEscape(stop_name), 
                        icon = icons) %>%
      addPolylines(data = subset(shapes_filter(), direction_id == 0), group = "Inbound") %>%
      addPolylines(data = subset(shapes_filter(), direction_id == 1), group = "Outbound") %>%
      addLayersControl(
        overlayGroups = c("Inbound", "Outbound"),
        options = layersControlOptions(collapsed = FALSE)) %>%
      addMiniMap()
    
    
  })
  
  
  observeEvent(c(input$choose_route, input$choose_service), {
    
    
    
    # infobox routes ----------------------------------------------------------
    output$infobox_routes <- renderUI({    
      
      
      list(
        # speed
        infoBox1(
          "Speed",
          if (!is.null(speed_filter())) paste0(round(speed_filter()), " km/h") else "NA km/h",
          # icon = icon("gauge-high", verify_fa = FALSE),
          icon = icon("gauge", verify_fa = FALSE),
          # icon = fontawesome::fa("clock"),
          # icon = HTML('<i class="fa-regular fa-gauge-high"></i>'),
          # color = ifelse(mean_speed < 12, "red", ifelse(mean_speed >= 12 & mean_speed <= 17, "black", "blue")),
          color = "black",
          fill = FALSE,
          width = 12, width2 = 6, width3 = 6, width4 = 4,
        ),
        
        # quantidade de paradas
        infoBox1(
          label_with_info(label = "stops",
                          tooltip_id = "q4_graph",
                          tooltip_title = "Stops",
                          tooltip_text = "www/tooltips/popover_stops.html"),
          paste0(stops_n_filter()[1,2], " stops"),
          icon = icon("bus", verify_fa = FALSE),
          # icon = HTML('<i class="fa-regular fa-gauge-high"></i>'),
          color = "black",
          fill = FALSE,
          width = 12, width2 = 6, width3 = 6, width4 = 4,
          # width = 12
        ),
        
        # comprimento da linha
        infoBox1(
          # "Distance",
          label_with_info(label = "Distance",
                          tooltip_id = "q3_graph",
                          tooltip_title = "Distance",
                          tooltip_text = "www/tooltips/popover_activity.html"),
          # "Distância",
          paste0(round(distance_filter()/1000), " km"),
          icon = icon("ruler", verify_fa = FALSE),
          # icon = HTML('<i class="fa-regular fa-gauge-high"></i>'),
          color = "black",
          fill = FALSE,
          width = 12, width2 = 12, width3 = 6, width4 = 4,
          # width = 12
        )
      )
      
    })
    
    
    # tempo total de viagem
    
    output$graph_frequency <- renderHighchart({
      
      
      frequency_filter()[[2]][, direction := fcase(direction_id == 0, "Inbound",
                                                   direction_id == 1, "Outbound")]
      
      hchart(frequency_filter()[[2]], "column", hcaes(y = headway_mean, x = hour, group = direction)
             # dataLabels = list(enabled = TRUE, format='{point.headway_mean}')
      ) %>%
        hc_xAxis(
          title = list(text = "Hour"),
          type = "category",
          allowDecimals =  FALSE,
          labels =
            list(enabled = TRUE,
                 # put an 'h' after the our
                 format = "{value}h",
                 # labels every 1  hour
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
    
    # stop the waiter once all calculations are finished
    w$hide()
    
  })
  
  
  
  
  
}