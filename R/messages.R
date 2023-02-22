# Functions to generate messages to the user

error_message <- function(msg) {
  glue::glue("<font color='#a9a9a9'><b>[ERROR]</b>: {msg}</font>")
}

warn_message <- function(msg) {
  glue::glue("[WARNING]: {msg}")
}

alert_message <- function(msg) {
  glue::glue("[ALERT]: {msg}")
}

info_message <- function(msg) {
  glue::glue("[INFO]: {msg}")
}
