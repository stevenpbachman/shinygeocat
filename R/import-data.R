# functions to import data from data sources.

#' Import a table of occurrence points from a user-provided CSV.
#'
#' Load points from a CSV and parse them into a standard format. All columns
#' that don't match the standard format will be discard.
#' 
#' @param path path to csv to read.
#' 
#' @return a standardised tibble of occurrence records
#' 
#' @examples
#' points <- import_csv("path/to/file")
#' 
#' @import dplyr stringr readr
import_csv <- function(path) {
  data <- read_csv(path, show_col_types=FALSE, progress=FALSE)
  data <- rename_with(data, str_to_lower)
  
  template <- empty_tbl_()
  template_names <- colnames(template)
  
  data <- select(data, any_of(str_to_lower(template_names)))
  
  kept_idx <- which(str_to_lower(template_names) %in% colnames(data))
  colnames(data) <- template_names[kept_idx]
  
  if (! "geocat_use" %in% colnames(data)) {
    data$geocat_use <- TRUE
  }
  
  data$geocat_source <- "User CSV"
  data$geocat_deleted <- FALSE
  data$geocat_id <- paste0("csv", seq(1, nrow(data)))
  
  data
}

#' Import a table of occurrence points from GBIF.
#' 
#' Request points from GBIF and parses them into a standard format. By default,
#' gets the first 900 points with coordinates and no geospatial issues for a name. 
#' If the exact name entered is not in the GBIF backbone, no points will be returned.
#' 
#' @param name scientific name of the taxon to request points for.
#' @param limit the maximum number of points to request.
#'
#' @return a standardised tibble of occurrence records.
#' 
#' @examples
#' points <- import_gbif("Poa annua")
#' 
import_gbif <- function(name, limit=900) {
  points <- list()
  end_of_records <- FALSE
  i <- 0
  
  while (length(points) < limit & !end_of_records) {
    results <- gbif_request_(name, offset=i)
    points <- c(points, results$results)
    end_of_records <- results$endOfRecords
  }
  
  points <- gbif_points_(points)
  points$geocat_use <- TRUE
  points$geocat_deleted <- FALSE
  points$geocat_source <- "GBIF"
  points$geocat_id <- paste0("gbif", seq(1, nrow(points)))
  
  points
}

#' Request occurrence points for a name from GBIF.
#' 
#' Makes a request directly to the GBIF occurrence API using the exact name entered.
#' If the name is not in the GBIF backbone, no occurrence records will be returned. Only
#' points with coordinates and no spatial issues will be returned. By default, only gets
#' the first 300 records.
#' 
#' @param name the scientific name of the taxon to request points for.
#' @param offset number of records to start the request at.
#' @param limit maximum number of records to return.
#' 
#' @returns the parsed occurrence request as a nested list.
#' 
#' @noRd
#' 
gbif_request_ = function(name, offset=0, limit=300) {
  url <- "http://api.gbif.org/v1/occurrence/search"
  query <- list(
    "hasCoordinate"="true",
    "limit"=limit,
    "hasGeospatialIssue"="false",
    "scientificName"=name,
    offset=offset * limit
  )
  
  response <- httr::GET(url, query=query)
  httr::warn_for_status(response)
  httr::content(response, as="parsed")
}

#' Parse GBIF points into a standard format.
#'
#' @param points a list of points returned from the GBIF API
#' @return a tibble storing data from a subset of the point fields, renamed
#'  to a standard template.
#'  
#' @noRd
#' 
#' @import rlang
#' 
gbif_points_ <- function(points) {
  GBIF_MAPPING <- list(
    basisOfRecord="basisOfRecord",
    genus="genus",
    specificEpithet="specificEpithet",
    latitude="decimalLatitude",
    longitude="decimalLongitude",
    coordinateUncertaintyInMeters="coordinateUncertaintyInMeters",
    locality="verbatimLocality",
    event_year="year",
    catalogNumber="catalogNumber",
    spatialref="geodeticDatum",
    source="occurrenceID",
    recordedBy="recordedBy",
    recordNumber="collectionCode",
    datasetKey="datasetKey"
  )
  
  points_list <- sapply(
    GBIF_MAPPING,
    function(x) sapply(points, function(y) y[[x]] %||% NA),
    simplify=FALSE,
    USE.NAMES=TRUE
  ) 
  
  dplyr::as_tibble(points_list)
}