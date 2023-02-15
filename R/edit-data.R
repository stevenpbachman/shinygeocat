# Functions for user input data from editing the map

#' Create data for an additional point at a user-specified location.
#'
#' Takes a leaflet feature created during user map-editing and wrangles the
#' data into the internal data format.
#'
#' @param feature a leaflet point feature.
#' 
#' @returns a single-rowed tibble with the occurrence data in it.
#' 
add_point <- function(feature) {
  id <- paste0("user", feature$properties[["_leaflet_id"]])
  note <- paste0(
    "New point added at (",
    feature$geometry$coordinates[[1]],
    ",",
    feature$geometry$coordinates[[2]],
    ")"
  )
  
  tibble::tibble(
    latitude=feature$geometry$coordinates[[2]],
    longitude=feature$geometry$coordinates[[1]],
    geocat_use=TRUE,
    geocat_deleted=FALSE,
    geocat_source="User point",
    geocat_id=id,
    geocat_notes=note
  )
}

move_point <- function(feature, point_tbl) {
  id <- feature$properties$layerId
  if (is.null(id)) {
    id <- paste0("user", feature$properties[["_leaflet_id"]])
  }
  
  new_lon <- feature$geometry$coordinates[[1]]
  new_lat <- feature$geometry$coordinates[[2]]
  
  notes <- point_tbl[point_tbl$geocat_id == id,]$geocat_notes
  notes <- paste0(
    notes,
    "> ",
    "Point move to ",
    "(",
    new_lon,
    ", ",
    new_lat,
    ")"
  )
    
  point_tbl %>%
    mutate(
      latitude=ifelse(geocat_id == id, new_lat, latitude),
      longitude=ifelse(geocat_id == id, new_lon, longitude),
      geocat_notes=ifelse(geocat_id == id, notes, geocat_notes)
    )
}

delete_point <- function(feature, point_tbl) {
  id <- feature$properties$layerId
  
  if (is.null(id)) {
    id <- paste0("user", feature$properties[["_leaflet_id"]])
  }
  
  point_tbl %>%
    mutate(
      geocat_use=ifelse(geocat_id == id, FALSE, geocat_use),
      geocat_deleted=ifelse(geocat_id == id, TRUE, geocat_deleted)
    )
}