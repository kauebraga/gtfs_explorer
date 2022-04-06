# trip_id <- trips_filter$trip_id
# stop_times <- gtfs$stop_times

get_trip_speed1 <- function (gtfs, shapes, trips) 
{
  length_unit <- "km/h"
  
  # trips_length <- shapes_filter$dist
  shapes_filter1 <- setDT(copy(shapes))
  shapes_filter1 <- shapes_filter1[, .(shape_id, dist)]
  
  
  # existing_trips <- unique(trips_length$trip_id)
  # if (is.null(trip_id)) {
  #   stop_times_trips <- unique(gtfs$stop_times$trip_id)
  #   existing_trips <- existing_trips[existing_trips %chin% 
  #                                      stop_times_trips]
  # }
  # duration_unit <- data.table::fifelse(unit == "km/h", "h", 
  #                                      "s")
  # trips_duration <- get_trip_duration(gtfs, trip_id, 
  #                                     duration_unit)
  trips_duration <- gtfs$stop_times[trip_id %chin% trips$trip_id]
  trips_duration[, `:=`(arrival_time_secs, data.table::as.ITime(arrival_time))]
  # only trips between 5 and 8
  trips_duration[, trip_start := first(arrival_time_secs), by = trip_id]
  if ("frequencies" %in% names(gtfs)) trips_duration else trips_duration <- trips_duration[between(trip_start, 18000, 75600)]
  trips_duration <- trips_duration[, .(duration = max(arrival_time_secs, 
                                                      na.rm = TRUE) - min(arrival_time_secs, na.rm = TRUE)), 
                                   keyby = .(trip_id, shape_id)]
  
  # bring gistance
  trips_speed <- trips_duration[shapes_filter1, on = "shape_id"]
  
  trips_speed[, `:=`(speed, (dist/duration)*3.6)]
  trips_speed[, `:=`(dist = NULL, duration = NULL)]
}
