calculate_route_frequency <- function(gtfs, route_id = NULL, trip_id = NULL,
                                      start_time = "06:00:00", end_time = "08:00:00",
                                      mean_headway = TRUE) {
  
  env <- environment()
  
  # identify type of gtfs
  type_gtfs <- if (any(grepl(pattern = "frequencies", x = names(gtfs)))) {
    
    if(nrow(gtfs$frequencies) > 0) "frequencies" else "stop_times"
    
  } else "stop_times"
  
  # get trips
  trips <- gtfs$trips
  
  # filter trips by route
  trips <- if (!is.null(get("route_id", envir = env))) trips[route_id %in% get("route_id", envir = env)] else trips
  
  # filter trips by service
  trips <- if (!is.null(get("trip_id", envir = env))) trips[trip_id %in% get("trip_id", envir = env)] else trips
  
  # get route info
  routes <- gtfs$routes[, .(route_id, route_long_name)]
  routes <- if (!is.null(get("route_id", envir = env))) routes[route_id %in% get("route_id", envir = env)] else routes
  
  
  if (type_gtfs == "frequencies") {
    
    frequencies <- gtfs$frequencies[trip_id %in% trips$trip_id]
    
    frequencies[, start_time := as.ITime(start_time)]
    frequencies[, end_time := as.ITime(end_time)]
    
    # filter only peak hours
    frequencies_filter <- frequencies[start_time >= as.ITime(get("start_time", envir = env))]
    frequencies_filter <- frequencies_filter[end_time < as.ITime(get("end_time", envir = env))]
    
    # bring route id and direction_id
    frequencies_filter <- merge(frequencies_filter, trips[, .(trip_id, route_id, direction_id, shape_id)],
                                sort = FALSE)
    # bring route info
    frequencies_filter <- merge(frequencies_filter, routes, by = "route_id", sort = FALSE)
    
    
    
    # calculate mean headways
    headways1 <- frequencies_filter[, .(headway_mean = as.integer(mean(headway_secs/60, na.rm = TRUE))),
                                    by = .(route_id, direction_id, shape_id, route_long_name)]
    
    frequencies_filter[, hour := data.table::hour(start_time)]
    headways2 <- frequencies_filter[, .(headway_mean = headway_secs/60),
                                    by = .(route_id, direction_id, shape_id, route_long_name, hour)]
    
    headways <- list(headways1, headways2)
    
    
  } else {
    
    # get stoptimes
    stop_times <- gtfs$stop_times[trip_id %in% trips$trip_id]
    
    # # bring route_id to stop_times
    # stop_times <- merge(stop_times,
    #                     trips[, .(trip_id, route_id, direction_id, shape_id, service_id, trip_headsign)],
    #                     by = "trip_id",
    #                     sort = FALSE)
    
    
    stop_times[, arrival_time := as.ITime(arrival_time)]
    
    # get start of each trip
    stop_times_starts <- stop_times[, .(arrival_time = as.ITime(data.table::first(arrival_time)),
                                        n_stops = .N,
                                        ttime_trip = (last(arrival_time) - first(arrival_time))/60),
                                    by = .(service_id, route_id, trip_id, direction_id, shape_id)]
    
    setorder(stop_times_starts, route_id, direction_id, shape_id, arrival_time)
    
    stop_times_starts_filter <- stop_times_starts[between(arrival_time, as.ITime(get("start_time", envir = env)), as.ITime(get("end_time", envir = env)))]
    
    stop_times_starts_filter[, headway := arrival_time - shift(arrival_time, type = "lag"),
                             by = .(service_id, route_id, direction_id, shape_id)]
    
    stop_times_starts_filter[, headway := as.integer(headway) / 60]
    
    # bring route info
    stop_times_starts_filter <- merge(stop_times_starts_filter, routes, by = "route_id", sort = FALSE)
    
    # calculate mean headway by direction
    headways1 <- stop_times_starts_filter[, .(headway_mean = as.integer(mean(headway, na.rm = TRUE))),
                                          by = .(route_id, direction_id, shape_id, route_long_name)]
    
    stop_times_starts_filter[, hour := data.table::hour(arrival_time)]
    headways2 <- stop_times_starts_filter[, .(headway_mean = as.integer(mean(headway, na.rm = TRUE))),
                                          by = .(route_id, direction_id, shape_id, route_long_name, hour)]
    headways <- list(headways1, headways2)
    
    
  }
  
  return(headways)
  
}
