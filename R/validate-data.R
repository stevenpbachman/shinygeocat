# Functions to validate occurrences and occurrence files

validate_csv <- function(df) {
  # errors for essentials
  msg <- check_fields_(df, c("longitude", "latitude"))
  if (!is.null(msg)) {
    return(list(msg=error_message(msg)))
  }
  
  msg <- check_numeric_(df, c("longitude", "latitude"))
  if (!is.null(msg)) {
    return(list(msg=error_message(msg)))
  }
  
  # warnings for things that are removed
  msg <- warn_message(check_complete_(df, c("longitude", "latitude")))
  validated <- mutate(
    df, 
    geocat_use=ifelse(is.na(longitude) | is.na(latitude), FALSE, geocat_use),
    geocat_deleted=ifelse(is.na(longitude) | is.na(latitude), TRUE, geocat_deleted),
    geocat_notes=ifelse(is.na(longitude) | is.na(latitude), "Missing coordinates", geocat_notes)
  )
  
  #deals with the case of points 0-360 or -360-0 for longitude and forces them to between -180-180
  #JM 12 2023
  #should send a message, but I can't work out Baz's code
  validated <- mutate(
      validated,
      geocat_notes=case_when(longitude < -180 & longitude > -360 ~ "Longitude below -180, forced to between -180-180",
                             longitude > 180 & longitude < 360 ~ "Longitude above 180, forced to between -180-180",
                            .default = ""),

      longitude=case_when(longitude < -180 & longitude > -360 ~ longitude + 360,
                            longitude > 180 & longitude < 360 ~ longitude - 360,
                          .default = longitude),
  )

  
  msg <- c(msg, warn_message(check_range_(df, "longitude", -360, 360)))
  validated <- mutate(
    validated,


    geocat_use=case_when(longitude < -360 ~ FALSE,
                         longitude > 360 ~ FALSE, 
                         TRUE ~ geocat_use),
    geocat_deleted=case_when(longitude < -36 ~ TRUE,
                             longitude > 360 ~ TRUE, 
                             TRUE ~ geocat_deleted),

    
    geocat_notes=case_when(longitude < -360 ~ "Longitude out of range",
                           longitude > 360 ~ "Longitude out of range", 
                           TRUE ~ geocat_notes)
    )
  
  msg <- c(msg, check_range_(df, "latitude", -90, 90))
  validated <- mutate(
    validated, 
    geocat_use=case_when(latitude < -90 ~ FALSE,
                         latitude > 90 ~ FALSE, 
                         TRUE ~ geocat_use),
    geocat_deleted=case_when(latitude < -90 ~ TRUE,
                             latitude > 90 ~ TRUE, 
                             TRUE ~ geocat_deleted),
    geocat_notes=case_when(latitude < -90 ~ "Latitude out of range",
                           latitude > 90 ~ "Latitude out of range", 
                           TRUE ~ geocat_notes)
  )
  
  # alerts for dubious things
  msg <- c(msg, alert_message(check_rounded_(df, "longitude")))
  msg <- c(msg, alert_message(check_rounded_(df, "latitude")))
  
  msg <- c(msg, alert_message(check_zeros_(df, "longitude")))
  msg <- c(msg, alert_message(check_zeros_(df, "latitude")))
  
  list(valid_data=validated, msg=msg)
}

validate_gbif <- function(df) {
  if (nrow(df) == 0) {
    msg <- error_message("No records found in GBIF. Check the name is in the GBIF backbone.")
  } else {
    msg <- NULL
  }
  
  list(valid_data=df, msg=msg)
}

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
    yrcompiled=numeric(),
    compiler=character(),
    citation=character(),
    recordedBy=character(),
    recordNumber=character(),
    datasetKey=character(),
    group=character(),
    geocat_id=character(),
    geocat_source=character(),
    geocat_use=logical(),
    geocat_notes=character(),
    geocat_native=logical(),
    geocat_deleted=logical()
  )
}
