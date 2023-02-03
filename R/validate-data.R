#Function to setup empty dataframe for geoCAT to work with
#JM
#notes
#geocat_source = where the data has come from (ie gbif, csv import, user defined)
#geocat_id = an internal ID which is populated on import (ie 1:nrow), new points will be added to this with a nrow + _leaflet_id (_leaflet_id is a unique number from leaflet) this should allow me to use this id to query all on map edits/delete. The reason for odd allocation is that leaflet assigns its own unique values and I can’t find a way to update (read only), there is a possibility of a clash if,  I just increment ID and I may have get back out the leaflet ID as some point (not sure I do). I may simplify this later.
#geocat_status = a record of what has happen to a point ie “new point”, “moved point”, “deleted point”
#geocat_use = Flag to false it point is deleted (could query on above, but I feel this is quicker)?
#geocat_analysis = Flag for any queries (ie GBIF only etc), which we can switch on or off depending on reactive # elements (ie your switches for GBIF/user)
#geocat_notes = records what has happened the points when moved (ie moved from x,y to p,q)
#note at this point #geocat_analysis is not used
buildspdf <- function(){
  df <- data.frame(BasisOfRec = as.character(),
             EVENT_YEAR = as.integer(),
             latitude = as.numeric(),
             longitude = as.numeric(),
             sci_name = as.character(),
             PRESENCE = as.integer(),
             ORIGIN = as.integer(),
             SEASONAL = as.integer(),
             CATALOG_NO = as.character(),
             SPATIALREF = as.character(),
             CITATION = as.character(),
             COMPILER = as.character(),
             recordno = as.character(),
             recordedBy = as.character(),
             yrcompiled = as.integer(),
             DATA_SENS = as.logical(),
             SOURCE = as.logical(),
             dist_comm = as.logical(),
             tax_comm = as.logical(),
             #user specific
             id = as.integer(),
             #geocat specific fields
             geocat_source = as.character(),
             geocat_id = as.integer(),
             geocat_status = as.character(),
             geocat_use = as.logical(),
             geocat_analysis = as.logical(),
             geocat_notes = as.character()
  )
  #add some dummy data
  mypoints <- data.frame(longitude = rnorm(4) + 46, latitude = rnorm(4) + -21, geocat_id = c(1:4),geocat_use=TRUE)
  merge(df,mypoints, all=TRUE)
  
  
}


# Functions to validate occurrences and occurrence files

check_fields_ <- function(df, required_fields) {
  msg <- NULL
  
  missing_cols <- setdiff(required_fields, colnames(df))
  if (length(missing_cols) > 0) {
    fields_formatted <- glue::glue_collapse(required_fields, ", ", last=" and ")
    missing_formatted <- glue::glue_collapse(missing_cols, ", ", last=" and ")
    msg <- glue::glue("Uploaded CSV should have the column(s) {fields_formatted}",
                      "yours is missing {missing_formatted}", .sep=", ")
  }
  
  msg
}

check_numeric_ <- function(df, numeric_fields) {
  msg <- NULL
  
  is_numeric <- sapply(numeric_fields, function(x) is.numeric(df[[x]]))
  bad_fields <- names(is_numeric)[!is_numeric]

  if (length(bad_fields) > 0) {
    bad_formatted <- glue::glue_collapse(bad_fields, ", ", last=" and ")
    msg <- glue::glue("The column(s) {bad_formatted} contain some values that aren't numbers")
  }
  
  msg
}

check_complete_ <- function(df, complete_fields) {
  msg <- NULL
  
  has_missing <- sapply(complete_fields, function(x) any(is.na(df[[x]])))
  incomplete <- names(has_missing)[has_missing]
  
  if (length(incomplete) > 0) {
    incomplete_formatted <- glue::glue_collapse(incomplete, ", ", last=" and ")
    msg <- glue::glue("The column(s) {incomplete_formatted} contain missing values")
  }
  
  msg
  
}

check_range_ <- function(df, field, min, max) {
  msg <- NULL
  
  if (any(df[[field]] < min, na.rm=TRUE) | any(df[[field]] > max, na.rm=TRUE)) {
    msg <- glue::glue("Values in {field} found outside {min} and {max}")
  }
  
  msg
}

check_rounded_ <- function(df, field, digits=0, threshold=0.1) {
  msg <- NULL
  
  rounded <- mean(round(df[[field]], digits=digits) == df[[field]], na.rm=TRUE)
  
  if (rounded > threshold) {
    msg <- glue::glue("{round(rounded*100)} % of values in the {field}",
    "column have {digits} decimal points", .sep=" ")
  }
  
  msg
}

check_zeros_ <- function(df, field) {
  msg <- NULL
  
  rounded <- mean(df[[field]] == 0, na.rm=TRUE)
  
  if (rounded > 0) {
    msg <- glue::glue("{round(rounded*100)} % of values in the {field}",
                      "column are zero", .sep=" ")
  }
  
  msg
}

#' standardised headings and types for occurrence table
#' 
empty_tbl_ <- function() {
  tibble::tibble(
    basisOfRecord=character(),
    genus=character(),
    specificEpithet=character(),
    sci_name=character(),
    latitude=numeric(),
    longitude=numeric(),
    coordinateUncertaintyInMeters=numeric(),
    locality=character(),
    event_year=numeric(),
    catalogNumber=character(),
    spatialref=character(),
    presence=numeric(),
    origin=numeric(),
    seasonal=numeric(),
    data_sens=character(),
    source=character(),
    yrcompiled=character(),
    compiler=character(),
    citation=character(),
    recordedBy=character(),
    recordNumber=character(),
    datasetKey=character(),
    group=character(),
    geocat_id=numeric(),
    geocat_status=character(),
    geocat_use=logical(),
    geocat_analysis=logical()
  )
}
