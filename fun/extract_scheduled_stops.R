extract_scheduled_stops <- function (gtfs, service_id = NULL, route_id = NULL) 
{
  env <- environment()
  trips <- gtfs$trips
  trips <- if (!is.null(route_id)) trips[route_id %in% get("route_id", envir = env)] else trips
  trips <- if (!is.null(service_id)) trips[service_id %in% get("service_id", envir = env)] else trips
  stops <- gtfs$stops[, .(stop_id, stop_name, stop_lon, stop_lat)]
  
  stop_times <- gtfs$stop_times[trips$trip_id, on = "trip_id"]
  # stop_times <- gtfs$stop_times[trip_id %in% trips$trip_id]
  
  stop_times <- stop_times[, .(trip_id, stop_id, arrival_time, 
                               departure_time, stop_sequence)]
  trips <- trips[, .(trip_id, route_id, shape_id)]
  stops_linhas_vai <- merge(stop_times, trips, by = "trip_id", 
                            sort = FALSE)
  stops_linhas_vai <- stops_linhas_vai[, .N, by = .(trip_id, 
                                                    shape_id)]
  stops_linhas_vai <- stops_linhas_vai[, .SD[which.max(N)], 
                                       by = shape_id]
  stops_linhas <- stop_times[trip_id %in% stops_linhas_vai$trip_id]
  stops_linhas <- merge(stops_linhas, trips, by = "trip_id", 
                        sort = FALSE)
  stops_linhas <- merge(stops_linhas, stops, by = "stop_id", 
                        sort = FALSE)
  stops_linhas <- stops_linhas[, .(route_id, shape_id, stop_id, 
                                   stop_name, stop_sequence, stop_lon, stop_lat)]
}
