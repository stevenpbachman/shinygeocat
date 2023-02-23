# Functions to generate messages to the user

message_ <- function(msg, type, colour) {
  glue::glue("<font color='{colour}'><b>[{type}]</b>: {msg}</font>")
}

error_message <- function(msg) {
  message_(msg, "ERROR", "#B33A3A")
}

warn_message <- function(msg) {
  message_(msg, "WARNING", "#FF6700")
}

alert_message <- function(msg) {
  message_(msg, "ALERT", "#FFB703")
}

info_message <- function(msg) {
  message_(msg, "INFO", "#8ECAE6")
}

format_new_point <- function(feature) {
  lat <- feature$geometry$coordinates[[1]]
  lng <- feature$geometry$coordinates[[2]]
  glue::glue("New point added at ({lat}, {lng})")
}

format_move_point <- function(feature) {
  lat <- feature$geometry$coordinates[[1]]
  lng <- feature$geometry$coordinates[[2]]
  glue::glue("Point moved to ({lat}, {lng})")
}

format_delete_point <- function(feature) {
  lat <- feature$geometry$coordinates[[1]]
  lng <- feature$geometry$coordinates[[2]]
  glue::glue("Point deleted from ({lat}, {lng})")
}
