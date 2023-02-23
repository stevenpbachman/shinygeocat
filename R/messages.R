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
