# Functions for user input data from editing the map

create_point_feature <- function(long, lat) {
  list(
    type = "Feature",
    properties = list(
      `_leaflet_id` = withr::with_options(
        list(scipen = 999),
        round(as.numeric(Sys.time()) * 1000)
      ),
      feature_type = "circlemarker",
      radius = 7
    ),
    geometry = list(
      type = 'Point',
      coordinates = list(long, lat)
    )
  )
}



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
  note <- format_new_point(feature)
  #################################
  #JM this corrects any outside of -180 or 180 points in the data, but not the drawing
  if (feature$geometry$coordinates[[1]] > 180){
    longituder <- feature$geometry$coordinates[[1]]-360
  } else if (feature$geometry$coordinates[[1]] < -180) {
    longituder <- feature$geometry$coordinates[[1]] + 360
  } else {
    longituder <- feature$geometry$coordinates[[1]]
  }
  #################################

  tibble::tibble(
    latitude=feature$geometry$coordinates[[2]],
    #longitude=feature$geometry$coordinates[[1]],
    longitude=longituder,
    spatialref="WGS84",
    yrcompiled=as.integer(format(Sys.Date(), "%Y")),
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
  notes <- paste0(notes, "> ", format_move_point(feature))
  
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

flag_native <- function(point_tbl, native_geom) {
  points_sf <- st_as_sf(point_tbl, coords=c("longitude", "latitude"),
                        crs=st_crs(native_geom))
  
  in_native <- st_within(points_sf, st_make_valid(native_geom), sparse=FALSE)
  
  is_native <- rowSums(in_native) >= 1
  
  point_tbl$geocat_native <- is_native
  
  point_tbl
}