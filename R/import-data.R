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
  #######handles ; and ,#######
  browser()
  fl <- readLines(path, n = 1)
  numfields <- count.fields(textConnection(fl), sep = ";")
  if (numfields == 1) data <- read.csv(path,fileEncoding="latin1") else data <- read.csv2(path,fileEncoding="latin1")
  
  #data <- read_csv(path, show_col_types=FALSE, progress=FALSE)
  data <- rename_with(data, str_to_lower)
  ############check for lat or long fields and rename
  colnames(data)[which("lat"== colnames(data))] <- "latitude"
  colnames(data)[which("long"== colnames(data))] <- "longitude"
  ###############
  
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
  data$geocat_notes <- NA_character_
#####

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
import_gbif <- function(name, limit=5000) {
  points <- list()
  end_of_records <- FALSE
  i <- 0
  
  while (length(points) < limit & !end_of_records) {
    results <- gbif_request_(name, offset=i)
    points <- c(points, results$results)
    end_of_records <- results$endOfRecords
  }
  
  points <- gbif_points_(points)
  if (nrow(points) == 0) {
    return(empty_tbl_())
  }
  
  points$geocat_use <- TRUE
  points$geocat_deleted <- FALSE
  points$geocat_source <- "GBIF"
  points$geocat_id <- paste0("gbif", seq(1, nrow(points)))
  points$geocat_notes <- NA_character_
  
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
    recordNumber="recordNumber",
    datasetKey="datasetKey",
    gbifID="gbifID"
  )
  
  points_list <- sapply(
    GBIF_MAPPING,
    function(x) sapply(points, function(y) y[[x]] %||% NA),
    simplify=FALSE,
    USE.NAMES=TRUE
  ) 
  
  points_list$gbifID <- paste0("https://www.gbif.org/occurrence/", points_list$gbifID)
  
  dplyr::as_tibble(points_list)
}

#' Import polygons from POWO for a given ID
import_powo <- function(id) {
  tryCatch({
    results <- kewr::lookup_powo(id, distribution=TRUE)
    dist_codes <- sapply(results$distribution$natives, function(x) x$tdwgCode)
    filter(wgsrpd, LEVEL3_COD %in% dist_codes)
  }, error=function(e) {
    if (str_detect(e$message, "\\(404\\) Not Found")) {
      return(NULL)
    }
    
    stop(e)
  })
}
