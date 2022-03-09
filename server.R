# increase max upload size to 30mb
options(shiny.maxRequestSize=30*1024^2)

library(gtfstools)
library(shiny)
library(leaflet)
library(shinydashboard)
library(data.table)
library(waiter)
library(highcharter)


function(input, output, session) {
  
  
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
                               accept = c(".zip")),
                         )
                       }),
                       easyClose = TRUE,
                       size = "m",
                       footer = div(id = "openDetails", class = "btn btn-default action-button shiny-bound-input",
                                    tagList(
                                      modalButton(icon("check"))
                                    )
                       )
                       
                     )
  ) 
  
  # Show the model on start up ...
  showModal(query_modal)
  
  values <- reactiveValues(gtfs = NULL, stop_times = NULL)
  
  observeEvent(input$gtfs, {
    
    # waiter_show(html = spin_loader(), color = "##F4F6F6")
    
    req(input$gtfs)
    
    if (input$gtfs$datapath == 1) {
      
      
      gtfs1 <- gtfstools::read_gtfs(input$gtfs$datapath,
                                    skip = "stop_times")
      
    } else {
      
      gtfs1 <- lapply(input$gtfs$datapath, gtfstools::read_gtfs, skip = "stop_times")
      gtfs1 <- do.call(gtfstools::merge_gtfs, gtfs1)
      
    }
    
    values$gtfs <- gtfs1
    shapes <- gtfstools::convert_shapes_to_sf(gtfs1)
    # bring route to the shapes
    shapes <- merge(shapes, unique(gtfs1$trips, by = c("route_id", "shape_id")))
    shapes <- merge(shapes, gtfs1$routes[, .(route_id, route_long_name, route_type)])
    shapes <- sf::st_sf(shapes)
    values$shapes <- shapes
    
    
    # TODO: by route type
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
            addPolylines(
              data = subset(shapes, route_type == k[[i]]), group = as.character(df[sigla == k[[i]]]$text)
            )
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
      # waiter_hide()
      
      
      # leaflet() %>%
      #   addTiles() %>%
      #   addPolylines(data = values$shapes, group = "Linha") %>%
      #   addLayersControl(overlayGroups = c("Linha"),
      #                    options = layersControlOptions(collapsed = FALSE))
      
      
    })
    
    
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
    
    # routes by type
    route_by_type <- values$gtfs$routes[, .N, by = route_type]
    
    
    output$speed_infobox <- renderInfoBox({
      infoBox(
        "Velocidade",
        paste0(round(mean_speed), " km/h"),
        icon = icon("user-friends"),
        color = "black"
      )
      
    })
    
    
  })
  
  
  
  
  
  observeEvent(input$tabs, {
    
    if (input$tabs == "tab_routes") {
      
      # print(paste("You clicked tab:", input$tabs))
      
      # print(input$gtfs$datapath)
      
      stop_times <- gtfstools::read_gtfs(input$gtfs$datapath,
                                         files = "stop_times")$stop_times
      # bring routes
      stop_times <- merge(stop_times, values$gtfs$trips[, .(trip_id, route_id, shape_id, direction_id)],
                          sort = FALSE)
      
      values$stop_times <- stop_times
      
    }
    
  })
  
  output$route_choice <- renderUI({
    
    pickerInput(inputId = "choose_route",
                label = "Choose route",
                choices = unique(values$stop_times$route_id),
                selected = "075")
    
  })
  
  observeEvent(input$choose_route, {
    
    # output$table_routes <- renderTable(
    #   
    #   head(values$stop_times[trip_id == input$choose_route])
    #   
    # )
    
    values$gtfs$stop_times <- values$stop_times
    
    print(head(values$gtfs$stop_times))
    
    shapes_filter <- subset(values$shapes, route_id == input$choose_route)
    
    print(head(shapes_filter))
    
    
    trips_filter <- subset(values$gtfs$trips, route_id == input$choose_route)
    print(head(trips_filter))
    
    # speeds
    mean_speed <- gtfstools::get_trip_speed(values$gtfs, trip_id = trips_filter$trip_id, file = "shapes")
    mean_speed <- mean(mean_speed$speed, na.rm = TRUE)
    print(mean_speed)
    
    output$map_routes <- renderLeaflet(
      
      leaflet() %>%
        addTiles() %>%
        addPolylines(data = shapes_filter)
      
    )
    
    output$speed_infobox <- renderInfoBox({
      infoBox(
        "Velocidade",
        paste0(round(mean_speed), " km/h"),
        icon = icon("user-friends"),
        color = "black"
      )
      
    })
    
  })
  
  
  
  
  
}