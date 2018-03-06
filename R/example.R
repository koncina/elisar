# Adapting example function from readxl. Default is here the main example

NULL

#' Get path to elisar example
#'
#' elisar provides an example file in its `inst/extdata`
#' directory. This function returns the path to the file.
#'
#' @export
#' @examples
#' elisar_example()
elisar_example <- function() {
  system.file("extdata", "example.xls", package = "elisar", mustWork = TRUE)
}
