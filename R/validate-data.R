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
