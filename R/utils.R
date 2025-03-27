# R/utils.R

#' Create a display card for the UI
#' @param header Card header text
#' @param body Card body content
#' @param colour Background color class for the header
#' @param fs Enable full-screen option
#' @param min_height Minimum height of the card body
#' @param fillable Whether the card body should be fillable
#' @return A Shiny card UI element
#' @noRd
display_card <- function(header, body, colour = "bg-dark", fs = TRUE, min_height = "10em", fillable = TRUE) {
  card(
    full_screen = fs,
    card_header(header, class = colour),
    card_body(body, min_height = min_height, fillable = fillable)
  )
}

#' Detect the operating system
#' @return A string indicating the OS ("WIN", "LINUX", "MAC")
#' @noRd
detect_os <- function() {
  os <- .Platform$OS.type
  if (os == "windows") {
    return("WIN")
  } else if (os == "unix") {
    if (Sys.info()["sysname"] == "Darwin") {
      return("MAC")
    } else {
      return("LINUX")
    }
  } else {
    return("UNKNOWN")
  }
}