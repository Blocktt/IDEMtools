#' @title run MassIBItools Shiny Example
#'
#' @description Launches Shiny app for MassIBItools.
#'
#' @details The Shiny app based on the R package MassIBItools is included in the R package.
#' This function launches that app.
#'
#' The Shiny app is online at:
#' https://tetratech-wtr-wne.shinyapps.io/MassIBItools/
#'
#' @examples
#' \dontrun{
#' # Run Function
#' runShiny()
#' }
#
#' @export
runShiny <- function(){##FUNCTION.START
  #
  appDir <- system.file("shiny-examples", "MassIBItools", package = "MassIBItools")
  #
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `MassIBItools`.", call. = FALSE)
  }
  #
  shiny::runApp(appDir, display.mode = "normal")
  #
}##FUNCTION.END
